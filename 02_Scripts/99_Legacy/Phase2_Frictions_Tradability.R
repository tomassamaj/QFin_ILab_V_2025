# ==============================================================================
# Phase2_Frictions_Tradability.R
# ==============================================================================
# Purpose : Apply frictions (TC, SC, dividend tax) to Phase 2 single-stock
#           factor momentum portfolios; test multiple position-concentration
#           versions to find a tradable implementation.
#
# Input   : 01_Data/Processed/Phase2/phase2_master.parquet
#           Schema: strategy, lookback, eom_signal, eom_hold, id,
#                   net_weight, gross_weight, n_factors, me,
#                   div12m_me, ret_exc_lead1m
#
# Outputs : 01_Data/Processed/Phase2/phase2_frictions_<strategy>_<lb>.parquet
#           03_Outputs/Frictions/Phase2_Friction_Decomposition_<strategy>_<lb>.pdf
#           03_Outputs/Frictions/Phase2_Concentration_Comparison_<strategy>_<lb>.pdf
#
# KEY METHODOLOGICAL NOTES
# -------------------------
# 1. div12m_me is the ANNUAL trailing dividend yield. Divide by 12 for monthly.
# 2. me is already in phase2_master (no secondary USA.parquet join needed).
# 3. ret_exc_lead1m is the full holding-month excess return (no 1-day lag
#    correction available at stock level -- minor bias vs factor-level).
# 4. net_weight is normalised so gross exposure = 1.0 per (strategy, lookback, eom).
# 5. Concentration via percentile-filtering mirrors 08_Limit_no_of_positions.R.
#    After filtering, long/short legs are each rescaled to 0.50 so gross = 1.0.
# ==============================================================================


# --- LIBRARIES ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  arrow, tidyverse, data.table, lubridate,
  PerformanceAnalytics, scales, ggplot2, patchwork
)


# --- 1. CONFIGURATION --------------------------------------------------------

MASTER_FILE      <- "01_Data/Processed/Phase2/phase2_master.parquet"
FACTOR_RETS_FILE <- "01_Data/Processed/Phase2/phase2_factor_returns.parquet"
OUTPUT_DIR  <- "01_Data/Processed/Phase2"
PLOTS_DIR   <- "03_Outputs/Frictions"
dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Strategies to analyse (change here to run others)
FOCUS_STRATEGY  <- c("CS_LO_50", "CS_LO_25")
FOCUS_LOOKBACKS <- c("1M", "6M", "12M")

# A. Transaction Cost parameters (power-law, same as Frictions.R)
TC_BASE_BPS  <- 5      # base cost (bps) for the largest stocks
TC_SLOPE_BPS <- 1      # marginal illiquidity premium
TC_ALPHA     <- 0.25   # concavity (0 = flat, 1 = linear in size ratio)
TC_MAX_BPS   <- 100    # cap to avoid extreme small-cap penalties

# B. Short-sale Borrow Cost parameters (annual bps, power-law)
SC_BASE_BPS  <- 25     # base annual borrow rate (bps)
SC_SLOPE_BPS <- 10     # marginal illiquidity premium
SC_ALPHA     <- 0.5    # concavity
SC_MAX_BPS   <- 500    # cap

# C. Dividend withholding tax on short positions
#    Short sellers must pay dividends to the lender; 27.5% Austrian withholding rate
TAX_RATE     <- 0.275

# Concentration versions to test:
#   full       : all positions, as-is from phase2_master (gross = 1.0)
#   min5bps    : drop positions with |weight| < 5 bps, renormalise 50/50
#   top10pct   : keep p10 (short) and p90 (long) by weight, rescale 50/50
#   top5pct    : keep p05 / p95
#   top2_5pct  : keep p02.5 / p97.5
CONC_VERSIONS <- list(
  full      = list(method = "full",    threshold = NA,     pct = NA),
  min5bps   = list(method = "min_abs", threshold = 5e-4,   pct = NA),
  top10pct  = list(method = "pctile",  threshold = NA,     pct = 0.10),
  top5pct   = list(method = "pctile",  threshold = NA,     pct = 0.05),
  top2_5pct = list(method = "pctile",  threshold = NA,     pct = 0.025)
)


# --- 2. HELPER FUNCTIONS -----------------------------------------------------

#' apply_concentration : filter & rescale a data.table of positions
#'
#' @param dt        data.table with columns (eom, id, net_weight, ...)
#' @param method    "full" | "min_abs" | "pctile"
#' @param threshold minimum |weight| kept (used when method="min_abs")
#' @param pct       tail percentile kept on each side (used when method="pctile")
#' @return          filtered & rescaled data.table (gross=1, net~0 for LS)
apply_concentration <- function(dt, method, threshold, pct) {

  if (method == "full") {
    # Already normalised: gross=1, net~0
    return(copy(dt))
  }

  if (method == "min_abs") {
    # Keep positions whose |net_weight| >= threshold
    out <- dt[abs(net_weight) >= threshold]

  } else if (method == "pctile") {
    # Compute per-month percentile cutoffs on net_weight
    dt[, `:=`(
      p_lo = quantile(net_weight, pct,     na.rm = TRUE),
      p_hi = quantile(net_weight, 1 - pct, na.rm = TRUE)
    ), by = eom]
    out <- dt[net_weight <= p_lo | net_weight >= p_hi]
    out[, `:=`(p_lo = NULL, p_hi = NULL)]
    dt[,  `:=`(p_lo = NULL, p_hi = NULL)]   # clean parent
  }

  # Rescale: long leg sums to 0.5, short leg sums to -0.5 (gross = 1.0, net = 0.0)
  out[net_weight > 0, net_weight := 0.5 * (net_weight / sum(net_weight)),      by = eom]
  out[net_weight < 0, net_weight := 0.5 * (net_weight / sum(abs(net_weight))), by = eom]

  return(out)
}


#' compute_frictions : TC + SC + dividend tax for one portfolio data.table
#'
#' @param dt      data.table with (eom, id, net_weight, me, div12m_me)
#' @return list(perf_ts = monthly performance series, drag_summary = annual averages)
compute_frictions <- function(dt) {

  # ---- A. TC rates (once per stock-month) ------------------------------------
  dt[, ref_mcap := {
    top500 <- head(sort(me, decreasing = TRUE), 500)
    median(top500, na.rm = TRUE)
  }, by = eom]

  dt[, tc_bps := pmin(TC_MAX_BPS,
                      TC_BASE_BPS + TC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^TC_ALPHA)]
  # Fill NA tc_bps (missing me) with the monthly 90th percentile
  dt[is.na(tc_bps),
     tc_bps := quantile(tc_bps, 0.90, na.rm = TRUE), by = eom]

  # ---- B. SC rates -----------------------------------------------------------
  dt[, sc_annual_bps := pmin(SC_MAX_BPS,
                             SC_BASE_BPS + SC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^SC_ALPHA)]
  dt[is.na(sc_annual_bps),
     sc_annual_bps := quantile(sc_annual_bps, 0.90, na.rm = TRUE), by = eom]

  # ---- C. Transaction costs --------------------------------------------------
  # Turnover = |w_t - w_{t-1}| (one-way)
  # Shift w_prev forward by one month so it aligns with w_t on the same eom row
  w_curr <- dt[, .(eom, id, w_t    = net_weight, tc_bps)]
  w_prev <- dt[, .(eom = eom %m+% months(1), id, w_prev = net_weight)]

  trades <- merge(w_curr, w_prev, by = c("eom", "id"), all = TRUE)
  trades[is.na(w_t),    c("w_t", "tc_bps") := .(0, NA)]
  trades[is.na(w_prev), w_prev := 0]

  # Fill missing tc_bps for new entries using monthly average
  avg_tc <- trades[, .(avg_tc = mean(tc_bps, na.rm = TRUE)), by = eom]
  trades[avg_tc, on = "eom", avg_tc := i.avg_tc]
  trades[is.na(tc_bps), tc_bps := avg_tc]

  trades[, tc_cost := abs(w_t - w_prev) * (tc_bps / 10000)]
  monthly_tc <- trades[, .(drag_tc = sum(tc_cost, na.rm = TRUE)), by = eom]

  # ---- D. Shorting costs (monthly) -------------------------------------------
  dt[, sc_cost := 0]
  dt[net_weight < 0,
     sc_cost := abs(net_weight) * (sc_annual_bps / 10000) / 12]
  monthly_sc <- dt[, .(drag_sc = sum(sc_cost, na.rm = TRUE)), by = eom]

  # ---- E. Dividend withholding tax on shorts ----------------------------------
  # div12m_me is the ANNUAL trailing yield; divide by 12 for monthly
  dt[, tax_cost := 0]
  dt[net_weight < 0 & !is.na(div12m_me) & div12m_me > 0,
     tax_cost := abs(net_weight) * (div12m_me / 12) * TAX_RATE]
  monthly_tax <- dt[, .(drag_tax = sum(tax_cost, na.rm = TRUE)), by = eom]

  # ---- F. Gross portfolio return (no day-1 lag at stock level) ---------------
  perf_ts <- dt[, .(
    ret_gross = sum(net_weight * ret_exc_lead1m, na.rm = TRUE),
    n_pos     = .N,
    turnover  = NA_real_    # placeholder; filled below from trades
  ), by = eom][order(eom)]

  # One-way turnover per month
  to_dt <- trades[, .(turnover = sum(abs(w_t - w_prev), na.rm = TRUE) / 2), by = eom]
  perf_ts[to_dt, on = "eom", turnover := i.turnover]

  # Merge friction layers
  perf_ts <- merge(perf_ts, monthly_tc,  by = "eom", all.x = TRUE)
  perf_ts <- merge(perf_ts, monthly_sc,  by = "eom", all.x = TRUE)
  perf_ts <- merge(perf_ts, monthly_tax, by = "eom", all.x = TRUE)
  perf_ts[is.na(drag_tc),  drag_tc  := 0]
  perf_ts[is.na(drag_sc),  drag_sc  := 0]
  perf_ts[is.na(drag_tax), drag_tax := 0]

  # Net return series (layered)
  perf_ts[, `:=`(
    ret_net_tc  = ret_gross - drag_tc,
    ret_net_sc  = ret_gross - drag_tc - drag_sc,
    ret_net_all = ret_gross - drag_tc - drag_sc - drag_tax
  )]

  # Annual drag averages
  drag_summary <- data.table(
    drag_tc_ann  = mean(perf_ts$drag_tc,  na.rm = TRUE) * 12,
    drag_sc_ann  = mean(perf_ts$drag_sc,  na.rm = TRUE) * 12,
    drag_tax_ann = mean(perf_ts$drag_tax, na.rm = TRUE) * 12
  )
  drag_summary[, drag_total_ann := drag_tc_ann + drag_sc_ann + drag_tax_ann]

  list(perf_ts = perf_ts, drag_summary = drag_summary)
}


#' annualised_metrics : compute performance stats from a return vector
annualised_metrics <- function(r, label = "") {
  r <- r[!is.na(r)]
  n       <- length(r)
  ann_ret <- mean(r) * 12
  ann_vol <- sd(r) * sqrt(12)
  sharpe  <- ifelse(ann_vol > 0, ann_ret / ann_vol, NA_real_)
  cum     <- cumprod(1 + r)
  peak    <- cummax(cum)
  max_dd  <- min((cum - peak) / peak)
  data.table(
    label    = label,
    n_months = n,
    ann_ret  = ann_ret,
    ann_vol  = ann_vol,
    sharpe   = sharpe,
    cum_ret  = tail(cum, 1) - 1,
    max_dd   = max_dd,
    calmar   = ifelse(max_dd < 0, ann_ret / abs(max_dd), NA_real_)
  )
}


# --- 3. MAIN LOOP: strategy × lookback × concentration ----------------------

# Pre-load factor-level lag-adjusted returns (used for day-1 lag correction)
# Key insight: stock_gross (no lag) ≈ factor_full_return at portfolio level.
# factor_ret (lag-adjusted) = stock return you'd earn with 1-day implementation lag.
# => ret_gross_lag = factor_ret_lag_adj  (the correct real-world comparable)
fac_rets <- read_parquet(FACTOR_RETS_FILE) %>%
  as.data.table() %>%
  setnames("eom_signal", "eom") %>%
  .[, eom := as.Date(eom)] %>%
  .[, .(strategy, lookback, eom, ret_lag_adj = ret)]

cat("\n======================================================================\n")
cat("  PHASE 2 — FRICTIONS & TRADABILITY ANALYSIS\n")
cat("======================================================================\n\n")

for (strat in FOCUS_STRATEGY) {
  for (lb in FOCUS_LOOKBACKS) {

    cat(sprintf("\n--- Strategy: %s | Lookback: %s ---\n", strat, lb))

    # 3.1 Load subset from master ------------------------------------------------
    cat("  Loading data...\n")
    dt_raw <- read_parquet(MASTER_FILE) %>%
      as.data.table() %>%
      .[strategy == strat & lookback == lb]

    # Rename eom_signal → eom for consistency with Frictions.R convention
    setnames(dt_raw, "eom_signal", "eom")
    dt_raw[, eom := as.Date(eom)]

    cat(sprintf("  Rows loaded: %s | Months: %d | Unique stocks: %s\n",
                format(nrow(dt_raw), big.mark = ","),
                dt_raw[, uniqueN(eom)],
                format(dt_raw[, uniqueN(id)], big.mark = ",")))

    # 3.2 Weight distribution diagnostics ----------------------------------------
    wt_pct <- dt_raw[, .(abs_w = abs(net_weight)), by = eom][
      , .(
        n_pos       = .N,
        pct_lt1bps  = mean(abs_w < 1e-4),
        pct_lt5bps  = mean(abs_w < 5e-4),
        pct_lt10bps = mean(abs_w < 1e-3),
        mean_abs_w  = mean(abs_w)
      ), by = eom
    ][, lapply(.SD, mean, na.rm = TRUE), .SDcols = -"eom"]

    cat(sprintf("\n  Weight distribution (avg across months):\n"))
    cat(sprintf("    Positions/month   : %8.0f\n", wt_pct$n_pos))
    cat(sprintf("    < 1 bps           : %7.1f%%\n", 100 * wt_pct$pct_lt1bps))
    cat(sprintf("    < 5 bps           : %7.1f%%\n", 100 * wt_pct$pct_lt5bps))
    cat(sprintf("    < 10 bps          : %7.1f%%\n", 100 * wt_pct$pct_lt10bps))
    cat(sprintf("    Mean |weight|     : %7.4f%%\n", 100 * wt_pct$mean_abs_w))

    # 3.3 Run all concentration versions -----------------------------------------
    all_results  <- list()
    all_perf_ts  <- list()
    version_stats <- list()

    for (vname in names(CONC_VERSIONS)) {
      v <- CONC_VERSIONS[[vname]]
      cat(sprintf("\n  [%s] Applying concentration...\n", vname))

      dt_v <- apply_concentration(
        copy(dt_raw), v$method, v$threshold, v$pct
      )

      # Position & turnover stats (pre-friction)
      pos_stats <- dt_v[, .(
        n_pos     = .N,
        gross_exp = sum(abs(net_weight)),
        net_exp   = sum(net_weight)
      ), by = eom][, .(
        avg_n_pos   = mean(n_pos),
        avg_gross   = mean(gross_exp),
        avg_net_exp = mean(abs(net_exp))
      )]

      cat(sprintf("    Avg positions/month: %5.0f  gross: %.4f  net: %.5f\n",
                  pos_stats$avg_n_pos, pos_stats$avg_gross, pos_stats$avg_net_exp))

      # Apply frictions
      fric <- compute_frictions(copy(dt_v))
      ts   <- fric$perf_ts
      ds   <- fric$drag_summary

      # --- 1-Day Lag correction ------------------------------------------------
      # stock ret_gross uses ret_exc_lead1m (full-month, NO lag) — overstates
      # real-world return by the first-day portfolio return.
      # Lag-adjusted gross ≈ factor-level lag-adjusted return (1-day lag applied
      # at factor level in Python: factor_ret_lag1 = full_month/day1 - 1).
      # This equals what you earn implementing at the CLOSE of day 1 of each month.
      fac_sub <- fac_rets[strategy == strat & lookback == lb, .(eom, ret_lag_adj)]
      ts[fac_sub, on = "eom", ret_gross_lag := i.ret_lag_adj]
      ts[, ret_lag_net_all := ret_gross_lag - drag_tc - drag_sc - drag_tax]
      m_gross_lag <- annualised_metrics(ts$ret_gross_lag,  paste0(vname, "_gross_lag"))
      m_lag_net   <- annualised_metrics(ts$ret_lag_net_all,paste0(vname, "_lag_net_all"))

      # Annualised metrics for each return series
      m_gross <- annualised_metrics(ts$ret_gross,  paste0(vname, "_gross"))
      m_tc    <- annualised_metrics(ts$ret_net_tc, paste0(vname, "_net_tc"))
      m_sc    <- annualised_metrics(ts$ret_net_sc, paste0(vname, "_net_sc"))
      m_all   <- annualised_metrics(ts$ret_net_all,paste0(vname, "_net_all"))

      cat(sprintf("    Gross (no lag): Ret=%+.1f%%  Vol=%.1f%%  SR=%.2f\n",
                  100*m_gross$ann_ret, 100*m_gross$ann_vol, m_gross$sharpe))
      cat(sprintf("    Gross (1d lag): Ret=%+.1f%%  Vol=%.1f%%  SR=%.2f  (day1 drag=%.2f%% pa)\n",
                  100*m_gross_lag$ann_ret, 100*m_gross_lag$ann_vol, m_gross_lag$sharpe,
                  100*(m_gross$ann_ret - m_gross_lag$ann_ret)))
      cat(sprintf("    Net-TC        : Ret=%+.1f%%  (TC drag=%.2f%% pa)\n",
                  100*m_tc$ann_ret, 100*ds$drag_tc_ann))
      cat(sprintf("    Net-SC        : Ret=%+.1f%%  (SC drag=%.2f%% pa)\n",
                  100*m_sc$ann_ret, 100*ds$drag_sc_ann))
      cat(sprintf("    Net-All(no lag): Ret=%+.1f%%  Vol=%.1f%%  SR=%.2f  (Total drag=%.2f%% pa)\n",
                  100*m_all$ann_ret, 100*m_all$ann_vol, m_all$sharpe,
                  100*ds$drag_total_ann))
      cat(sprintf("    Net-All(1d lag): Ret=%+.1f%%  Vol=%.1f%%  SR=%.2f\n",
                  100*m_lag_net$ann_ret, 100*m_lag_net$ann_vol, m_lag_net$sharpe))

      # Build wealth index for plotting
      ts[, `:=`(
        w_gross      = cumprod(1 + fifelse(is.na(ret_gross),      0, ret_gross)),
        w_gross_lag  = cumprod(1 + fifelse(is.na(ret_gross_lag),  0, ret_gross_lag)),
        w_net_tc     = cumprod(1 + fifelse(is.na(ret_net_tc),     0, ret_net_tc)),
        w_net_sc     = cumprod(1 + fifelse(is.na(ret_net_sc),     0, ret_net_sc)),
        w_net_all    = cumprod(1 + fifelse(is.na(ret_net_all),    0, ret_net_all)),
        w_lag_net    = cumprod(1 + fifelse(is.na(ret_lag_net_all),0, ret_lag_net_all)),
        version      = vname
      )]

      all_perf_ts[[vname]]  <- ts
      all_results[[vname]]  <- list(ts = ts, ds = ds,
                                    m_gross = m_gross, m_all = m_all,
                                    m_gross_lag = m_gross_lag, m_lag_net = m_lag_net,
                                    pos_stats = pos_stats)

      version_stats[[vname]] <- data.table(
        version          = vname,
        avg_n_pos        = pos_stats$avg_n_pos,
        avg_turnover     = mean(ts$turnover, na.rm = TRUE),
        ann_ret_gross    = m_gross$ann_ret,
        sharpe_gross     = m_gross$sharpe,
        ann_ret_gross_lag= m_gross_lag$ann_ret,
        sharpe_gross_lag = m_gross_lag$sharpe,
        drag_tc_ann      = ds$drag_tc_ann,
        drag_sc_ann      = ds$drag_sc_ann,
        drag_tax_ann     = ds$drag_tax_ann,
        drag_total       = ds$drag_total_ann,
        ann_ret_net      = m_all$ann_ret,
        sharpe_net       = m_all$sharpe,
        ann_ret_lag_net  = m_lag_net$ann_ret,
        sharpe_lag_net   = m_lag_net$sharpe,
        max_dd_net       = m_all$max_dd
      )
    }

    # 3.4 Print summary table ----------------------------------------------------
    summary_dt <- rbindlist(version_stats)
    cat(sprintf("\n\n  === CONCENTRATION COMPARISON: %s | %s ===\n", strat, lb))
    cat(sprintf("  %-12s %8s %9s %9s %8s %8s %8s %8s %9s %8s %9s %8s\n",
                "Version","N_pos","Ret_NoLag","SR_NoLag","Ret_Lag","SR_Lag",
                "Drag_TC","Drag_SC","Drag_Tax","Net_NoLag","Net_Lag","SR_LagNet"))
    cat(sprintf("  %s\n", paste(rep("-", 120), collapse="")))
    for (i in seq_len(nrow(summary_dt))) {
      r <- summary_dt[i]
      cat(sprintf("  %-12s %8.0f %8.1f%% %8.2f %8.1f%% %8.2f %8.2f%% %8.2f%% %8.2f%% %8.1f%% %8.1f%% %8.2f\n",
                  r$version, r$avg_n_pos,
                  100*r$ann_ret_gross, r$sharpe_gross,
                  100*r$ann_ret_gross_lag, r$sharpe_gross_lag,
                  100*r$drag_tc_ann, 100*r$drag_sc_ann, 100*r$drag_tax_ann,
                  100*r$ann_ret_net, 100*r$ann_ret_lag_net, r$sharpe_lag_net))
    }

    # 3.5 Save results parquet ---------------------------------------------------
    # Full portfolio frictions series
    save_ts  <- rbindlist(lapply(all_perf_ts, function(x) x))
    save_path <- file.path(OUTPUT_DIR,
                           sprintf("phase2_frictions_%s_%s.parquet", strat, lb))
    write_parquet(save_ts, save_path)
    cat(sprintf("\n  Saved: %s\n", save_path))

    # Summary table csv
    csv_path <- file.path(OUTPUT_DIR,
                          sprintf("phase2_frictions_summary_%s_%s.csv", strat, lb))
    fwrite(summary_dt, csv_path)
    cat(sprintf("  Saved: %s\n", csv_path))


    # --- 4. PLOTS ----------------------------------------------------------------
    perf_long <- rbindlist(all_perf_ts)

    # 4.1 Friction decomposition plot (full portfolio only) ----------------------
    ts_full <- all_perf_ts[["full"]]

    p_decomp <- ggplot(ts_full, aes(x = eom)) +
      # Day-1 lag band (gap between no-lag and lag-adjusted gross)
      geom_ribbon(aes(ymin = w_gross_lag, ymax = w_gross,
                      fill = "0. Day-1 Lag"), alpha = 0.45) +
      geom_ribbon(aes(ymin = w_net_tc, ymax = w_gross_lag,
                      fill = "1. Transaction Costs"), alpha = 0.5) +
      geom_ribbon(aes(ymin = w_net_sc, ymax = w_net_tc,
                      fill = "2. Short-Sale Costs"), alpha = 0.5) +
      geom_ribbon(aes(ymin = w_lag_net, ymax = w_net_sc,
                      fill = "3. Dividend Tax"), alpha = 0.5) +
      geom_line(aes(y = w_gross,     color = "Gross (no lag)"),    linewidth = 0.8, linetype = "dashed") +
      geom_line(aes(y = w_gross_lag, color = "Gross (1d lag)"),    linewidth = 1) +
      geom_line(aes(y = w_lag_net,   color = "Net (lag + frict)"), linewidth = 1) +
      scale_fill_manual(
        name   = "Friction / Lag Layers",
        values = c("0. Day-1 Lag"         = "#A6CEE3",
                   "1. Transaction Costs" = "#FDBF6F",
                   "2. Short-Sale Costs"  = "#FB9A99",
                   "3. Dividend Tax"      = "#CAB2D6")
      ) +
      scale_color_manual(
        name   = "Performance",
        values = c("Gross (no lag)"    = "#1F78B4",
                   "Gross (1d lag)"    = "#33A02C",
                   "Net (lag + frict)" = "black")
      ) +
      scale_y_log10(labels = comma) +
      labs(
        title    = sprintf("Full Waterfall — %s %s (Full Portfolio)", strat, lb),
        subtitle = sprintf(
          "Day-1 lag: %.2f%% pa | TC: %.2f%% pa | SC: %.2f%% pa | Div Tax: %.2f%% pa | Total frict: %.2f%% pa",
          100 * (all_results$full$m_gross$ann_ret - all_results$full$m_gross_lag$ann_ret),
          100 * all_results$full$ds$drag_tc_ann,
          100 * all_results$full$ds$drag_sc_ann,
          100 * all_results$full$ds$drag_tax_ann,
          100 * all_results$full$ds$drag_total_ann
        ),
        x = "Date", y = "Wealth Index (log scale)"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "right")

    # 4.2 Concentration comparison: net wealth paths ----------------------------
    net_paths <- rbindlist(lapply(names(all_perf_ts), function(vname) {
      all_perf_ts[[vname]][, .(eom, w_net_all, version = vname)]
    }))

    p_conc <- ggplot(net_paths, aes(x = eom, y = w_net_all, color = version)) +
      geom_line(linewidth = 0.8) +
      scale_y_log10(labels = comma) +
      scale_color_brewer(palette = "Set1", name = "Concentration") +
      labs(
        title    = sprintf("Net Wealth by Concentration Version — %s %s", strat, lb),
        subtitle = "All series: Gross return minus TC + SC + Dividend Tax",
        x = "Date", y = "Wealth Index (log scale)"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "right")

    # 4.3 Positions and turnover by version (bar charts) ------------------------
    pos_turn <- rbindlist(lapply(names(all_results), function(vname) {
      r  <- all_results[[vname]]
      ts <- r$ts
      data.table(
        version      = vname,
        avg_n_pos    = r$pos_stats$avg_n_pos,
        avg_turnover = mean(ts$turnover, na.rm = TRUE) * 100
      )
    }))

    p_pos <- ggplot(pos_turn, aes(x = version, y = avg_n_pos, fill = version)) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Average Positions per Month",
           x = NULL, y = "N positions") +
      theme_minimal(base_size = 11)

    p_turn <- ggplot(pos_turn, aes(x = version, y = avg_turnover, fill = version)) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Average Monthly Turnover (one-way, %)",
           x = NULL, y = "Turnover (%)") +
      theme_minimal(base_size = 11)

    # 4.4 Save plots to PDF -------------------------------------------------------
    pdf_decomp <- file.path(PLOTS_DIR,
                            sprintf("Phase2_Friction_Decomposition_%s_%s.pdf", strat, lb))
    ggsave(pdf_decomp, plot = p_decomp, width = 12, height = 6)
    cat(sprintf("  Plot saved: %s\n", pdf_decomp))

    pdf_conc <- file.path(PLOTS_DIR,
                          sprintf("Phase2_Concentration_Comparison_%s_%s.pdf", strat, lb))
    combined_plot <- (p_conc) / (p_pos | p_turn) +
      plot_annotation(
        title    = sprintf("Position Concentration Analysis — %s | %s", strat, lb),
        subtitle = "Lower concentration = fewer stocks = higher per-stock turnover"
      )
    ggsave(pdf_conc, plot = combined_plot, width = 14, height = 10)
    cat(sprintf("  Plot saved: %s\n", pdf_conc))

  }  # end lookback loop
}  # end strategy loop


cat("\n======================================================================\n")
cat("  DONE.\n")
cat("======================================================================\n\n")
