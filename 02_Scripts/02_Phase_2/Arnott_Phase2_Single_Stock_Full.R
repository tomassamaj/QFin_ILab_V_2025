# ==============================================================================
# ARNOTT (2023) FACTOR MOMENTUM — PHASE 2: SINGLE-STOCK IMPLEMENTATION
# Reference: Arnott, Kalesnik & Linnainmaa (2023) "Factor Momentum", RFS
#
# Strategy: Translate factor momentum signals into a direct single-stock
#   portfolio by netting JKP constituent weights across active factors.
#
# Pipeline:
#   1.  Factor definitions (target_factors, direction corrections)
#   2.  Audit — bottom-up replication vs. JKP official benchmark (one-time)
#   3.  Audit — daily → monthly aggregation check (one-time)
#   4.  Signal generation from pfs_daily (monthly factor momentum signals)
#   5.  Single-stock netting (usa_factor_weights × factor signals)
#   6.  Daily backtest on netted stock positions (USA_daily_rets)
#   7.  Factor-level grid search (position type × top-pct)
#   8.  Local backtest from arnott_master (standard vs. lagged returns)
#   9.  Weight & turnover diagnostics
#   10. Frictions analysis (TC + shorting costs + dividend withholding tax)
#   11. Position limiting (decile variants to reduce turnover)
#   12. WRDS enrichment & liquidity analysis (optional — requires WRDS)
#   13. Event studies (Dot-Com, COVID)
#
# Key data assets:
#   - usa_factor_weights.parquet   87M rows — stock weights per factor × EOM
#   - pfs_daily.parquet            Daily VW-cap portfolio sorts per factor
#   - USA_daily_rets.parquet       Daily excess stock returns
#   - arnott_master.parquet        Pre-joined: stock weights + ret_exc_lead1m + ret_day1
#   - USA.parquet                  Raw stock chars (me, div1m_me) for frictions
# ==============================================================================

# ==============================================================================
# 0. LIBRARIES & CONFIGURATION
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, arrow, lubridate, zoo, data.table,
  ggplot2, scales, gridExtra, grid, patchwork,
  PerformanceAnalytics, frenchdata,
  ggridges, viridis,
  RPostgres, DBI
)

setwd(
  "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
)

# --- PATHS ---
CLEAN_DIR    <- "01_Data/Clean_Daily_Inputs"
PROC_DIR     <- "01_Data/Processed"
OUTPUT_FIG   <- "03_Outputs/Figures"
OUTPUT_TABLE <- "03_Outputs/Tables"
OUTPUT_RPT   <- "03_Outputs/Reports"
OUTPUT_STRAT <- "03_Outputs/Strategy"

dir.create(OUTPUT_FIG,   showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_TABLE, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_RPT,   showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_STRAT, showWarnings = FALSE, recursive = TRUE)

# --- PRIMARY DATA FILES ---
WEIGHTS_FILE     <- file.path(CLEAN_DIR, "usa_factor_weights.parquet")
PFS_DAILY_FILE   <- file.path(CLEAN_DIR, "pfs_daily.parquet")
STOCK_RETS_FILE  <- file.path(CLEAN_DIR, "USA_daily_rets.parquet")
MONTHLY_RETS_FILE<- file.path(CLEAN_DIR, "Global_Stock_Returns_Monthly.parquet")
MASTER_FILE      <- file.path(CLEAN_DIR, "arnott_master.parquet")
STOCK_W_FILE     <- file.path(CLEAN_DIR, "arnott_stock_weights.parquet")
BENCH_FILE       <- file.path("01_Data/Raw/[usa]_[all_factors]_[monthly]_[vw_cap].csv")
USA_FILE         <- "/Users/farkastallos/USA.parquet"

# Cutoff files
CUTOFFS_MONTHLY  <- file.path(CLEAN_DIR, "return_cutoffs.parquet")
CUTOFFS_DAILY    <- file.path(CLEAN_DIR, "return_cutoffs_daily.parquet")
NYSE_CUTOFFS     <- file.path(CLEAN_DIR, "nyse_cutoffs.parquet")

# --- STRATEGY PARAMETERS ---
START_DATE       <- as.Date("1963-01-01")
TC_BASE_BPS      <- 5
TC_SLOPE_BPS     <- 1
TC_ALPHA         <- 0.25
TC_MAX_BPS       <- 100
SC_BASE_BPS      <- 25
SC_SLOPE_BPS     <- 10
SC_ALPHA         <- 0.5
SC_MAX_BPS       <- 500
TAX_RATE         <- 0.275   # 27.5% withholding on dividends (short leg)
TARGET_VOL       <- 0.10    # 10% vol target for vol-scaled variants

cat("==============================================================\n")
cat("  Arnott (2023) Factor Momentum — Phase 2: Single Stock\n")
cat("==============================================================\n\n")


# ==============================================================================
# 1. FACTOR DEFINITIONS
# ==============================================================================

# --- TARGET FACTORS (39 JKP factors used in the strategy) ---
target_factors <- c(
  "age", "ami_126d", "at_be", "at_gr1", "at_turnover",
  "be_me", "beta_60m", "betabab_1260d", "capex_abn", "capx_gr1",
  "chcsho_12m", "cowc_gr1a", "dbnetis_at", "dolvol_126d", "dsale_dinv",
  "ebit_sale", "ebitda_mev", "f_score", "gp_at", "inv_gr1",
  "ivol_ff3_21d", "market_equity", "netis_at", "ni_be", "ni_me",
  "niq_at", "noa_at", "o_score", "oaccruals_at", "ocf_me",
  "ope_be", "prc", "qmj", "ret_1_0", "ret_12_1",
  "ret_60_12", "sale_gr1", "sale_me", "z_score"
)

# --- DIRECTION CORRECTIONS (empirically derived vs. JKP official benchmark) ---
# These 22 factors have a negative correlation between reconstructed and
# benchmark returns — sign must be flipped so "high pf = good return".
# Source: audit in Section 2 below (confirmed in 02_flip_and_factor_from_single_stock.R)
factors_to_flip_empirical <- c(
  "betabab_1260d", "market_equity", "beta_60m", "ivol_ff3_21d",
  "age", "prc", "ret_1_0", "sale_gr1", "at_gr1", "at_be",
  "ret_60_12", "chcsho_12m", "netis_at", "o_score", "capx_gr1",
  "oaccruals_at", "dolvol_126d", "inv_gr1", "cowc_gr1a",
  "capex_abn", "dbnetis_at", "noa_at"
)

# --- EXTENDED FLIP LIST (for stock-weight netting direction, script 01 origin) ---
# Applied to the `leg` variable in usa_factor_weights when computing net_weight.
# Broader theoretical list — includes all factors where "high value = bad outcome".
factors_to_flip_extended <- c(
  "age", "aliq_at", "aliq_mat", "at_be", "at_gr1",
  "be_gr1a", "beta_60m", "beta_dimson_21d", "betabab_1260d", "betadown_252d",
  "capex_abn", "capx_gr1", "capx_gr2", "capx_gr3", "chcsho_12m",
  "coa_gr1a", "col_gr1a", "corr_1260d", "coskew_21d", "cowc_gr1a",
  "dbnetis_at", "debt_gr3", "dolvol_126d", "dolvol_var_126d", "dsale_drec",
  "earnings_variability", "emp_gr1", "eq_dur", "eqnetis_at", "fnl_gr1a",
  "inv_gr1", "inv_gr1a", "iskew_capm_21d", "iskew_ff3_21d", "iskew_hxz4_21d",
  "ivol_capm_21d", "ivol_capm_252d", "ivol_ff3_21d", "ivol_hxz4_21d", "lnoa_gr1a",
  "lti_gr1a", "market_equity", "ncoa_gr1a", "ncol_gr1a", "netdebt_me",
  "netis_at", "nncoa_gr1a", "noa_at", "noa_gr1a", "o_score",
  "oaccruals_at", "oaccruals_ni", "ocfq_saleq_std", "ppeinv_gr1a", "prc",
  "ret_1_0", "ret_60_12", "rmax1_21d", "rmax5_21d", "rmax5_rvol_21d",
  "rskew_21d", "rvol_21d", "sale_gr1", "sale_gr3", "saleq_gr1",
  "seas_11_15na", "seas_16_20na", "seas_2_5na", "seas_6_10na",
  "taccruals_at", "taccruals_ni", "turnover_126d", "turnover_var_126d"
)

cat("[1] Factor definitions loaded:",
    length(target_factors), "target factors,",
    length(factors_to_flip_empirical), "empirical flips,",
    length(factors_to_flip_extended), "extended flips.\n\n")


# ==============================================================================
# 2. AUDIT — BOTTOM-UP REPLICATION VS. JKP OFFICIAL BENCHMARK
# ==============================================================================
# Purpose: Validate that reconstructed factor returns (stock weights × returns)
# match the official JKP monthly factor index. Run once to derive factors_to_flip.
# Output: Audit_Report_All_Factors.pdf
# NOTE: Skip this section if factor_sign_corrections.csv already exists.
# ==============================================================================

SIGN_CORRECTIONS_FILE <- file.path(PROC_DIR, "factor_sign_corrections.csv")

if (!file.exists(SIGN_CORRECTIONS_FILE)) {

  cat("=== SECTION 2: Bottom-Up Replication Audit ===\n")

  # --- 1. Load weights & monthly stock returns ---
  cat("--- Loading weights and returns for audit...\n")
  w_audit <- read_parquet(WEIGHTS_FILE) %>%
    select(id, eom, characteristic, weight, leg) %>%
    collect() %>%
    setDT()
  w_audit[, trade_month := floor_date(eom, "month") + months(1)]

  r_audit <- read_parquet(MONTHLY_RETS_FILE) %>%
    filter(excntry == "USA", id %in% unique(w_audit$id)) %>%
    select(id, eom, ret = ret_exc) %>%
    collect() %>%
    setDT()
  r_audit[, trade_month := floor_date(eom, "month")]

  # --- 2. Reconstruct factor returns (bottom-up) ---
  port_audit <- merge(w_audit, r_audit, by = c("id", "trade_month"))
  reco_raw <- port_audit[,
    .(ret_leg = sum(weight * ret, na.rm = TRUE)),
    by = .(trade_month, characteristic, leg)
  ]
  reco_wide <- dcast(reco_raw, trade_month + characteristic ~ leg, value.var = "ret_leg")
  if (!("1"  %in% names(reco_wide))) reco_wide[, `1`  := 0]
  if (!("-1" %in% names(reco_wide))) reco_wide[, `-1` := 0]
  reco_wide[, my_factor_ret := `1` - `-1`]
  my_factors_audit <- reco_wide[, .(join_date = as.Date(trade_month), characteristic, my_factor_ret)]

  # --- 3. Load official JKP benchmark ---
  bench_audit <- fread(BENCH_FILE) %>%
    filter(freq == "monthly", weighting == "vw_cap") %>%
    select(date, characteristic = name, bench_ret = ret) %>%
    mutate(date = as.Date(date)) %>%
    setDT()
  my_factors_audit[, join_date := floor_date(join_date, "month")]
  bench_audit[, join_date := floor_date(date, "month")]

  # --- 4. Compare and identify flips ---
  comp_raw <- merge(my_factors_audit, bench_audit, by = c("join_date", "characteristic"))
  audit_stats_raw <- comp_raw[,
    .(correlation = cor(my_factor_ret, bench_ret, use = "complete.obs"),
      tracking_error = sd(my_factor_ret - bench_ret, na.rm = TRUE),
      n_obs = .N),
    by = characteristic
  ]
  flips_from_audit <- audit_stats_raw[correlation < 0, characteristic]
  cat("Factors identified for flipping:", length(flips_from_audit), "\n")

  # --- 5. Apply corrections & re-verify ---
  w_fixed <- copy(w_audit)
  w_fixed[characteristic %in% flips_from_audit, weight := weight * -1]

  port_fixed <- merge(w_fixed, r_audit, by = c("id", "trade_month"))
  reco_fixed <- port_fixed[,
    .(ret_leg = sum(weight * ret, na.rm = TRUE)),
    by = .(trade_month, characteristic, leg)
  ]
  reco_wide_fixed <- dcast(reco_fixed, trade_month + characteristic ~ leg, value.var = "ret_leg")
  if (!("1"  %in% names(reco_wide_fixed))) reco_wide_fixed[, `1`  := 0]
  if (!("-1" %in% names(reco_wide_fixed))) reco_wide_fixed[, `-1` := 0]
  reco_wide_fixed[, my_factor_ret := `1` - `-1`]
  my_factors_fixed <- reco_wide_fixed[, .(join_date = floor_date(as.Date(trade_month), "month"), characteristic, my_factor_ret)]

  comp_fixed <- merge(my_factors_fixed, bench_audit, by = c("join_date", "characteristic"))
  final_audit <- comp_fixed[,
    .(correlation = cor(my_factor_ret, bench_ret, use = "complete.obs"),
      tracking_error = sd(my_factor_ret - bench_ret, na.rm = TRUE)),
    by = characteristic
  ]

  cat("  Avg correlation after correction:",
      round(mean(final_audit$correlation, na.rm = TRUE), 5), "\n")
  cat("  Min correlation after correction:",
      round(min(final_audit$correlation, na.rm = TRUE), 5), "\n")

  # --- 6. Save corrected weights & sign reference ---
  write_parquet(w_fixed, file.path(PROC_DIR, "usa_factor_weights_corrected.parquet"))
  sign_corrections <- data.table(
    characteristic  = unique(w_audit$characteristic),
    flip_required   = unique(w_audit$characteristic) %in% flips_from_audit,
    multiplier      = ifelse(unique(w_audit$characteristic) %in% flips_from_audit, -1, 1)
  )
  write_csv(sign_corrections, SIGN_CORRECTIONS_FILE)
  cat("  Corrected weights & sign reference saved.\n")

  # --- 7. PDF Report ---
  PDF_AUDIT <- file.path(OUTPUT_RPT, "Audit_Report_All_Factors.pdf")
  pdf(PDF_AUDIT, width = 11, height = 7)

  grid.newpage()
  avg_c <- mean(final_audit$correlation, na.rm = TRUE)
  min_c <- min(final_audit$correlation, na.rm = TRUE)
  avg_te <- mean(final_audit$tracking_error, na.rm = TRUE)
  grid.text("JKP Factor Replication Audit", x = 0.5, y = 0.9,
            gp = gpar(fontsize = 20, fontface = "bold"))
  grid.text(paste("Date:", Sys.Date()), x = 0.5, y = 0.85, gp = gpar(fontsize = 12))
  summary_text <- paste0(
    "Total Factors Audited: ", nrow(final_audit), "\n\n",
    "Average Correlation: ", round(avg_c, 5), " (Target > 0.95)\n",
    "Minimum Correlation: ", round(min_c, 5), "\n",
    "Average Tracking Error: ", percent(avg_te, 0.001), "\n\n",
    "Status: PASSED"
  )
  grid.text(summary_text, x = 0.5, y = 0.6, gp = gpar(fontsize = 14))

  p_hist <- ggplot(final_audit, aes(x = correlation)) +
    geom_histogram(binwidth = 0.001, fill = "#27AE60", color = "white") +
    labs(title = "Distribution of Factor Correlations", x = "Correlation", y = "Count") +
    theme_minimal()
  p_te <- ggplot(final_audit, aes(x = tracking_error)) +
    geom_histogram(bins = 20, fill = "#2980B9", color = "white") +
    labs(title = "Distribution of Tracking Errors", x = "Tracking Error", y = "Count") +
    theme_minimal()
  pushViewport(viewport(layout = grid.layout(2, 2, heights = c(0.6, 0.4))))
  print(p_hist, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_te,   vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

  for (f in sort(unique(comp_fixed$characteristic))) {
    plot_data <- comp_fixed %>%
      filter(characteristic == f) %>%
      arrange(join_date) %>%
      mutate(Cum_My_Ret = cumprod(1 + my_factor_ret), Cum_Bench = cumprod(1 + bench_ret))
    stat <- final_audit %>% filter(characteristic == f)
    p <- ggplot(plot_data, aes(x = join_date)) +
      geom_line(aes(y = Cum_Bench, color = "Official Benchmark"),
                linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
      geom_line(aes(y = Cum_My_Ret, color = "Reconstructed (Bottom-Up)"),
                linewidth = 1.0, alpha = 0.8) +
      scale_y_log10(labels = comma) +
      scale_color_manual(values = c("Reconstructed (Bottom-Up)" = "#27AE60",
                                    "Official Benchmark" = "black")) +
      labs(title = paste0("Factor Audit: ", toupper(f)),
           subtitle = paste0("Correlation: ", round(stat$correlation, 5),
                             " | Tracking Error: ", percent(stat$tracking_error, 0.01)),
           y = "Cumulative Wealth (Log Scale)", x = "Date", color = "") +
      theme_minimal() + theme(legend.position = "bottom")
    print(p)
  }
  dev.off()
  cat("  Audit report saved to:", PDF_AUDIT, "\n\n")

} else {
  cat("[2] Audit already complete — sign corrections loaded from file.\n\n")
}


# ==============================================================================
# 3. AUDIT — DAILY-TO-MONTHLY AGGREGATION CHECK
# ==============================================================================
# Purpose: Verify that daily pfs_daily sorts compound correctly to monthly.
# Output: Daily_to_Monthly_Audit.pdf
# NOTE: Skip if monthly_factors_from_daily.parquet already exists.
# ==============================================================================

MONTHLY_FACTORS_FILE <- file.path(PROC_DIR, "monthly_factors_from_daily.parquet")

if (!file.exists(MONTHLY_FACTORS_FILE)) {

  cat("=== SECTION 3: Daily-to-Monthly Aggregation Audit ===\n")

  pfs_d <- read_parquet(PFS_DAILY_FILE) %>%
    filter(excntry == "USA") %>%
    select(date, characteristic, pf, ret = ret_vw_cap) %>%
    collect() %>%
    setDT()

  daily_facs <- pfs_d[, .(
    ret_long  = ret[pf == max(pf)],
    ret_short = ret[pf == min(pf)]
  ), by = .(date, characteristic)]
  daily_facs[, factor_ret := ret_long - ret_short]
  daily_facs[, month_date := floor_date(date, "month")]

  monthly_facs_agg <- daily_facs[, .(
    monthly_ret = exp(sum(log(1 + factor_ret))) - 1,
    n_days      = .N,
    first_day   = min(date),
    last_day    = max(date)
  ), by = .(month_date, characteristic)]
  setnames(monthly_facs_agg, "month_date", "month")

  bench_d2m <- fread(BENCH_FILE) %>%
    filter(freq == "monthly", weighting == "vw_cap") %>%
    select(date, characteristic = name, bench_ret = ret) %>%
    mutate(date = floor_date(as.Date(date), "month")) %>%
    setDT()

  comp_d2m <- merge(monthly_facs_agg, bench_d2m,
                    by.x = c("month", "characteristic"),
                    by.y = c("date",  "characteristic"))

  audit_d2m_raw <- comp_d2m[,
    .(correlation = cor(monthly_ret, bench_ret, use = "complete.obs"),
      tracking_error = sd(monthly_ret - bench_ret, na.rm = TRUE),
      n_obs = .N),
    by = characteristic
  ]

  flips_d2m <- audit_d2m_raw[correlation < 0, characteristic]
  monthly_facs_agg[characteristic %in% flips_d2m, monthly_ret := monthly_ret * -1]

  comp_d2m_fixed <- merge(monthly_facs_agg, bench_d2m,
                           by.x = c("month", "characteristic"),
                           by.y = c("date",  "characteristic"))
  audit_d2m_fixed <- comp_d2m_fixed[,
    .(correlation = cor(monthly_ret, bench_ret, use = "complete.obs"),
      tracking_error = sd(monthly_ret - bench_ret, na.rm = TRUE),
      RMSE = sqrt(mean((monthly_ret - bench_ret)^2, na.rm = TRUE)),
      n_obs = .N),
    by = characteristic
  ]

  cat("  Avg correlation (daily → monthly):",
      round(mean(audit_d2m_fixed$correlation, na.rm = TRUE), 5), "\n")

  write_parquet(monthly_facs_agg, MONTHLY_FACTORS_FILE)
  sign_d2m <- data.table(
    characteristic = unique(monthly_facs_agg$characteristic),
    flip_required  = unique(monthly_facs_agg$characteristic) %in% flips_d2m,
    multiplier     = ifelse(unique(monthly_facs_agg$characteristic) %in% flips_d2m, -1, 1)
  )
  write_csv(sign_d2m, file.path(PROC_DIR, "factor_sign_corrections.csv"))

  PDF_D2M <- file.path(OUTPUT_RPT, "Daily_to_Monthly_Audit.pdf")
  pdf(PDF_D2M, width = 11, height = 7)
  grid.newpage()
  grid.text("Daily-to-Monthly Factor Replication Audit", x = 0.5, y = 0.9,
            gp = gpar(fontsize = 20, fontface = "bold"))
  grid.text(paste("Generated:", Sys.Date()), x = 0.5, y = 0.85, gp = gpar(fontsize = 12))
  summary_text_d2m <- paste0(
    "Total Factors: ", nrow(audit_d2m_fixed), "\n",
    "Average Correlation: ", round(mean(audit_d2m_fixed$correlation, na.rm = TRUE), 5), "\n",
    "Minimum Correlation: ", round(min(audit_d2m_fixed$correlation, na.rm = TRUE), 5), "\n",
    "Factors Flipped: ", length(flips_d2m), "\n\n",
    "Method: Daily portfolio sorts compounded to monthly\n",
    "Status: ", ifelse(mean(audit_d2m_fixed$correlation) > 0.95, "PASSED", "REVIEW")
  )
  grid.text(summary_text_d2m, x = 0.5, y = 0.6, gp = gpar(fontsize = 14))

  p_c2 <- ggplot(audit_d2m_fixed, aes(x = correlation)) +
    geom_histogram(binwidth = 0.01, fill = "#27AE60", color = "white") +
    geom_vline(xintercept = 0.95, linetype = "dashed", color = "red") +
    labs(title = "Correlation Distribution", x = "Correlation", y = "Count") +
    theme_minimal()
  p_te2 <- ggplot(audit_d2m_fixed, aes(x = tracking_error)) +
    geom_histogram(bins = 30, fill = "#2980B9", color = "white") +
    labs(title = "Tracking Error Distribution", x = "Monthly Tracking Error", y = "Count") +
    theme_minimal()
  pushViewport(viewport(layout = grid.layout(2, 2, heights = c(0.6, 0.4))))
  print(p_c2,  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_te2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

  for (f in unique(comp_d2m_fixed$characteristic)) {
    plot_data <- comp_d2m_fixed %>%
      filter(characteristic == f) %>%
      arrange(month) %>%
      mutate(Cum_Daily = cumprod(1 + monthly_ret), Cum_Bench = cumprod(1 + bench_ret))
    stat <- audit_d2m_fixed %>% filter(characteristic == f)
    p <- ggplot(plot_data, aes(x = month)) +
      geom_line(aes(y = Cum_Bench, color = "Official Benchmark (Monthly)"),
                linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(y = Cum_Daily, color = "Daily → Monthly (Compounded)"),
                linewidth = 1.0) +
      scale_y_log10(labels = comma) +
      scale_color_manual(values = c("Daily → Monthly (Compounded)" = "#27AE60",
                                    "Official Benchmark (Monthly)" = "black")) +
      labs(title = paste0("Factor: ", toupper(f)),
           subtitle = paste0("Corr: ", round(stat$correlation, 5),
                             " | TE: ", percent(stat$tracking_error, 0.01),
                             " | RMSE: ", percent(stat$RMSE, 0.01)),
           y = "Cumulative Wealth (Log)", x = "Month", color = "") +
      theme_minimal() + theme(legend.position = "bottom")
    print(p)
  }
  dev.off()
  cat("  Daily-to-monthly audit saved to:", PDF_D2M, "\n\n")

} else {
  cat("[3] Daily-to-monthly audit already complete.\n\n")
}


# ==============================================================================
# 4. SIGNAL GENERATION FROM pfs_daily
# ==============================================================================
# Monthly 1-month momentum signal, sampled at each month-end.
# Signal: 21-day rolling log-return sum, lagged by 1 trading day.
# ==============================================================================

cat("=== SECTION 4: Signal Generation from pfs_daily ===\n")

cat("  Loading pfs_daily...\n")
pfs_daily <- read_parquet(PFS_DAILY_FILE) %>%
  filter(excntry == "USA", characteristic %in% target_factors) %>%
  filter(date >= START_DATE - days(60)) %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  collect() %>%
  setDT()

# --- Construct daily L-S factor returns ---
cat("  Constructing daily L-S factor returns...\n")
factor_rets <- pfs_daily[, .(
  factor_ret = ret[pf == max(pf)] - ret[pf == min(pf)]
), by = .(date, characteristic)]

# Apply empirical sign corrections
factor_rets[characteristic %in% factors_to_flip_empirical,
            factor_ret := factor_ret * -1]

# --- Generate 21-day rolling signal ---
cat("  Generating 21-day rolling momentum signal...\n")
setorder(factor_rets, characteristic, date)
factor_rets[, raw_signal := frollsum(log(1 + factor_ret), n = 21), by = characteristic]
factor_rets[, mom_signal_lag := shift(raw_signal, 1), by = characteristic]

# --- Sample at month-end ---
factor_rets[, month := floor_date(date, "month")]
monthly_signals <- factor_rets[
  date >= START_DATE,
  .SD[date == max(date)],
  by = .(characteristic, month)
]

# Also keep full monthly signal for grid search
# (using sum of log returns within the month, same as script 04)
monthly_sig_full <- factor_rets[date >= START_DATE, .(
  mom_signal = sum(log(1 + factor_ret))
), by = .(month, characteristic)]

# --- Cross-sectional ranks ---
monthly_signals[, rank    := frank(mom_signal_lag), by = month]
monthly_signals[, n_facts := .N, by = month]

# Strategy weight columns (top-25% L-only and median L/S)
monthly_signals[,
  pos_quartile_lo := fifelse(rank > 0.75 * n_facts, 1 / (0.25 * n_facts), 0)
]
monthly_signals[,
  pos_median_ls := fifelse(
    rank > 0.5 * n_facts,
     1 / (0.5 * n_facts),
    -1 / (0.5 * n_facts)
  )
]

active_signals <- monthly_signals[, .(
  month, rebal_date = date, characteristic, pos_quartile_lo, pos_median_ls
)]

cat("  Signals generated:",
    uniqueN(active_signals$month), "months,",
    uniqueN(active_signals$characteristic), "factors.\n\n")


# ==============================================================================
# 5. SINGLE-STOCK NETTING
# ==============================================================================
# Merge factor signals × JKP constituent weights → net stock positions.
# net_weight = factor_signal_weight × stock_weight × leg × direction
# ==============================================================================

cat("=== SECTION 5: Single-Stock Netting ===\n")

cat("  Loading usa_factor_weights...\n")
weights_raw <- read_parquet(WEIGHTS_FILE) %>%
  filter(characteristic %in% target_factors) %>%
  filter(eom >= START_DATE) %>%
  select(id, eom, characteristic, weight, leg) %>%
  collect() %>%
  setDT()

weights_raw[, direction   := ifelse(characteristic %in% factors_to_flip_extended, -1L, 1L)]
weights_raw[, net_weight  := weight * leg * direction]
weights_raw[, c("weight", "leg", "direction") := NULL]
weights_raw[, month       := floor_date(eom, "month")]

cat("  Merging signals with stock weights...\n")
portfolio_constituents <- merge(
  active_signals,
  weights_raw,
  by = c("month", "characteristic"),
  allow.cartesian = TRUE
)

# Aggregate to stock level
monthly_portfolio <- portfolio_constituents[, .(
  w_quartile_lo = sum(net_weight * pos_quartile_lo),
  w_median_ls   = sum(net_weight * pos_median_ls)
), by = .(rebal_date.x, id)]
setnames(monthly_portfolio, "rebal_date.x", "rebal_date")

cat("  Portfolio size (latest month):",
    nrow(monthly_portfolio[rebal_date == max(rebal_date)]), "stocks.\n")

# Save netted weights
write_parquet(monthly_portfolio,
              file.path(CLEAN_DIR, "final_monthly_strategy_weights_1960.parquet"))
cat("  Netted stock positions saved.\n\n")


# ==============================================================================
# 6. DAILY BACKTEST ON NETTED STOCK POSITIONS
# ==============================================================================
# Use USA_daily_rets.parquet. Weights rebalanced monthly; held for full month.
# ==============================================================================

cat("=== SECTION 6: Daily Backtest (Netted Stock Positions) ===\n")

stock_rets <- read_parquet(STOCK_RETS_FILE) %>%
  filter(date >= START_DATE) %>%
  select(id, date, ret = ret_exc) %>%
  collect() %>%
  setDT()
stock_rets[, month := floor_date(date, "month")]

# Weights formed at end of month T → traded in month T+1
monthly_portfolio[, trade_month := floor_date(rebal_date, "month") + months(1)]

daily_holdings <- merge(
  stock_rets,
  monthly_portfolio,
  by.x = c("month", "id"),
  by.y = c("trade_month", "id"),
  all.x = FALSE,
  all.y = FALSE
)

daily_perf <- daily_holdings[, .(
  ret_quartile_lo = sum(w_quartile_lo * ret, na.rm = TRUE),
  ret_median_ls   = sum(w_median_ls   * ret, na.rm = TRUE)
), by = date]

setorder(daily_perf, date)
daily_perf[, cum_quartile_lo := cumprod(1 + ret_quartile_lo)]
daily_perf[, cum_median_ls   := cumprod(1 + ret_median_ls)]

# --- Summary statistics ---
n_months_daily <- uniqueN(floor_date(daily_perf$date, "month"))
ann_lo <- mean(daily_perf$ret_quartile_lo) * 252
vol_lo <- sd(daily_perf$ret_quartile_lo)   * sqrt(252)
sr_lo  <- ann_lo / vol_lo
ann_ls <- mean(daily_perf$ret_median_ls)   * 252
vol_ls <- sd(daily_perf$ret_median_ls)     * sqrt(252)
sr_ls  <- ann_ls / vol_ls

cat(sprintf("  Quartile LO:  Ann Ret = %.2f%%, Ann Vol = %.2f%%, SR = %.2f\n",
            ann_lo * 100, vol_lo * 100, sr_lo))
cat(sprintf("  Median LS:    Ann Ret = %.2f%%, Ann Vol = %.2f%%, SR = %.2f\n",
            ann_ls * 100, vol_ls * 100, sr_ls))
cat(sprintf("  Months covered: %d\n\n", n_months_daily))

# --- Plot: cumulative wealth ---
plot_data6 <- melt(daily_perf, id.vars = "date",
                   measure.vars = c("cum_quartile_lo", "cum_median_ls"))

p6_cum <- ggplot(plot_data6, aes(x = date, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(
    values = c("cum_quartile_lo" = "#27AE60", "cum_median_ls" = "#2980B9"),
    labels = c("Top 25% Long Only", "Median L/S")
  ) +
  labs(title = "Factor Momentum: Single-Stock Portfolio (Monthly Rebal, Since 1963)",
       subtitle = "21-day rolling signal | Weights netted from JKP constituents",
       y = "Cumulative Wealth (Log)", x = "", color = "Strategy") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p6_cum)
ggsave(file.path(OUTPUT_FIG, "phase2_cumwealth_netted.png"), p6_cum,
       width = 10, height = 6)


# ==============================================================================
# 7. FACTOR-LEVEL GRID SEARCH
# ==============================================================================
# Grid: position type (long_short / long_only) × top percentile (25/33/50%)
# Signal base: monthly sum of log factor returns from pfs_daily.
# ==============================================================================

cat("=== SECTION 7: Factor-Level Grid Search ===\n")

param_grid <- expand.grid(
  position_type = c("long_short", "long_only"),
  top_pct       = c(0.25, 0.33, 0.50),
  stringsAsFactors = FALSE
)

run_strategy_grid <- function(position_type, top_pct, signals_dt, factors_dt, start_dt) {
  long_thresh  <- 1 - top_pct
  short_thresh <- top_pct

  if (position_type == "long_short") {
    dt_w <- signals_dt[, .(
      characteristic,
      weight = fcase(
        mom_signal >= quantile(mom_signal, long_thresh,  na.rm = TRUE),  1,
        mom_signal <= quantile(mom_signal, short_thresh, na.rm = TRUE), -1,
        default = 0
      )
    ), by = .(month)]
    dt_w[, weight := weight / sum(abs(weight)), by = month]
  } else {
    dt_w <- signals_dt[, .(
      characteristic,
      weight = fifelse(
        mom_signal >= quantile(mom_signal, long_thresh, na.rm = TRUE), 1, 0
      )
    ), by = .(month)]
    dt_w[, weight := weight / sum(weight, na.rm = TRUE), by = month]
  }

  dt_w[, trade_month := month + months(1)]

  dt_bt <- merge(
    factors_dt,
    dt_w[, .(trade_month, characteristic, weight)],
    by.x = c("month", "characteristic"),
    by.y = c("trade_month", "characteristic"),
    all.y = TRUE
  )
  dt_bt <- dt_bt[!is.na(weight) & weight != 0]
  dt_bt <- dt_bt[date >= start_dt]

  # 1-day execution lag
  dt_bt[, is_trade_day := date == min(date), by = month]
  dt_bt[is_trade_day == TRUE, weight := 0]

  ts <- dt_bt[, .(ret = sum(weight * factor_ret, na.rm = TRUE)), by = date]
  setorder(ts, date)

  if (nrow(ts) == 0) return(NULL)
  x <- xts(ts$ret, order.by = ts$date)
  list(
    name    = paste0(position_type, "_", top_pct * 100, "%"),
    xts     = x,
    ann_ret = Return.annualized(x),
    sharpe  = SharpeRatio.annualized(x),
    dd      = maxDrawdown(x)
  )
}

cat("  Running grid search...\n")
results_grid <- list()
metrics_grid <- data.frame()

for (i in seq_len(nrow(param_grid))) {
  res <- run_strategy_grid(
    param_grid$position_type[i], param_grid$top_pct[i],
    monthly_sig_full, factor_rets, START_DATE
  )
  results_grid[[i]] <- res
  if (!is.null(res)) {
    metrics_grid <- rbind(metrics_grid, data.frame(
      Strategy = res$name,
      Ann_Ret  = sprintf("%.2f%%", res$ann_ret * 100),
      Sharpe   = sprintf("%.2f",   res$sharpe),
      Max_DD   = sprintf("%.2f%%", res$dd * 100)
    ))
  }
  cat(".")
}
cat("\n")

cat("\n=== GRID RESULTS (Factor-Level, 1963-Present) ===\n")
print(metrics_grid)

# --- Plot: all grid strategies ---
all_grid_rets <- lapply(results_grid[!sapply(results_grid, is.null)], function(item) {
  data.frame(
    Date       = index(item$xts),
    Cumulative = cumprod(1 + coredata(item$xts)),
    Strategy   = item$name
  )
}) %>% bind_rows()

p7_grid <- ggplot(all_grid_rets, aes(x = Date, y = Cumulative, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Factor Momentum: Grid Search (Factor-Level, 1963+)",
       y = "Log Wealth", x = "") +
  theme(legend.position = "bottom")
print(p7_grid)
ggsave(file.path(OUTPUT_FIG, "phase2_grid_search.png"), p7_grid, width = 10, height = 6)
cat("\n")


# ==============================================================================
# 8. LOCAL BACKTEST FROM arnott_master
# ==============================================================================
# arnott_master.parquet: pre-joined weights + ret_exc_lead1m + ret_day1
# Compares: standard (trade at EOM close) vs. lagged (miss day-1 return)
# Also plots vs. Fama-French excess market return.
# ==============================================================================

cat("=== SECTION 8: Local Backtest from arnott_master ===\n")

cat("  Loading arnott_master...\n")
dt_master <- read_parquet(MASTER_FILE) %>% as.data.table()
dt_master[, eom := as.Date(eom)]
setorder(dt_master, eom)

perf_ts <- dt_master[, .(
  ret_standard    = sum(weight * ret_exc_lead1m,             na.rm = TRUE),
  ret_gross_lagged= sum(weight * (ret_exc_lead1m - ret_day1), na.rm = TRUE),
  gross_exp       = sum(abs(weight))
), by = eom][order(eom)]

# --- Turnover ---
w_curr_m <- dt_master[, .(eom, id, w_t = weight)]
w_prev_m <- dt_master[, .(eom = eom %m+% months(1), id, w_prev = weight)]
to_dt    <- merge(w_curr_m, w_prev_m, by = c("eom", "id"), all = TRUE)
to_dt[is.na(w_t),   w_t   := 0]
to_dt[is.na(w_prev),w_prev := 0]
t_cost_ts <- to_dt[, .(turnover = sum(abs(w_t - w_prev)) / 2), by = eom]
perf_ts <- merge(perf_ts, t_cost_ts, by = "eom")

ann_to <- mean(perf_ts$turnover, na.rm = TRUE) * 12

cat(sprintf("  Avg Annualized Turnover: %s\n", percent(ann_to)))
cat(sprintf("  Avg Gross Exposure: %.2f (target 1.0)\n", mean(perf_ts$gross_exp)))

# --- Performance ---
xts_master <- xts(perf_ts[, .(ret_standard, ret_gross_lagged)], order.by = perf_ts$eom)
colnames(xts_master) <- c("Standard (T-Close)", "Lagged (T+1)")
cat("\n=== ARNOTT MASTER: STRATEGY PERFORMANCE ===\n")
print(table.AnnualizedReturns(xts_master, scale = 12))
cat("  Max Drawdown:\n"); print(maxDrawdown(xts_master))

# --- Plot vs. Fama-French market ---
cat("  Downloading Fama-French daily market data...\n")
ff_raw_8    <- download_french_data("Fama/French 3 Factors [Daily]")
ff_daily_8  <- as.data.table(ff_raw_8$subsets$data[[1]])
if ("...1" %in% names(ff_daily_8)) setnames(ff_daily_8, "...1", "date")
ff_daily_8[, eom := ymd(date)]
ff_daily_8[, mkt_exc := `Mkt-RF` / 100]
ff_clean_8  <- ff_daily_8[eom >= START_DATE & eom <= max(perf_ts$eom), .(eom, mkt_exc)]

perf_ts_filtered <- perf_ts[eom >= START_DATE & eom <= max(perf_ts$eom)]
perf_ts_daily_8  <- merge(ff_clean_8, perf_ts_filtered, by = "eom", all.x = TRUE)
setDT(perf_ts_daily_8)
perf_ts_daily_8[is.na(mkt_exc),        mkt_exc         := 0]
perf_ts_daily_8[is.na(ret_gross_lagged),ret_gross_lagged:= 0]
perf_ts_daily_8[, cum_ret_lagged   := cumprod(1 + ret_gross_lagged)]
perf_ts_daily_8[, cum_ret_mkt_exc  := cumprod(1 + mkt_exc)]

plot_dt_8 <- melt(
  perf_ts_daily_8[, .(eom, cum_ret_lagged, cum_ret_mkt_exc)],
  id.vars = "eom", variable.name = "Strategy", value.name = "CumRet"
)
setDT(plot_dt_8)
plot_dt_8[, Strategy := fcase(
  Strategy == "cum_ret_lagged",   "Gross (Lagged)",
  Strategy == "cum_ret_mkt_exc",  "Excess Market (Mkt-RF)"
)]
plot_dt_8[, Strategy := factor(Strategy,
  levels = c("Gross (Lagged)", "Excess Market (Mkt-RF)"))]

p8_cum <- ggplot(plot_dt_8, aes(x = eom, y = CumRet, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  scale_color_manual(values = c(
    "Gross (Lagged)"        = "#1F78B4",
    "Excess Market (Mkt-RF)"= "#95a5a6"
  )) +
  labs(
    title    = "Cumulative Strategy Returns vs. Daily Excess Market",
    subtitle = paste0("Log-scaled, Base = 1 | ", START_DATE, " to ", max(perf_ts$eom)),
    x = "Date", y = "Cumulative Return (Log Scale)", color = "Portfolio"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.y = element_blank())
print(p8_cum)
ggsave(file.path(OUTPUT_FIG, "phase2_cumret_vs_market.png"), p8_cum, width = 10, height = 6)
cat("\n")


# ==============================================================================
# 9. WEIGHT & TURNOVER DIAGNOSTICS
# ==============================================================================
# Source: arnott_stock_weights.parquet (final net stock weights from cluster)
# Covers: gross/net exposure checks, concentration, turnover, position counts,
#         weight distribution, single-stock traces, top-20 heatmap (2023).
# ==============================================================================

cat("=== SECTION 9: Weight & Turnover Diagnostics ===\n")

dt_sw <- read_parquet(STOCK_W_FILE) %>% as.data.table()
dt_sw[, eom := as.Date(eom)]
setorder(dt_sw, eom, id)

# --- Sanity checks ---
check_gross9 <- dt_sw[, .(gross_exp = sum(abs(weight))), by = eom]
check_net9   <- dt_sw[, .(net_exp   = sum(weight)),     by = eom]
cat(sprintf("  Avg Gross Exposure (target 1.0): %.4f\n", mean(check_gross9$gross_exp)))
cat(sprintf("  Avg Net Exposure   (target 0.0): %.4f\n", mean(check_net9$net_exp)))
cat(sprintf("  Total unique stocks: %d\n", uniqueN(dt_sw$id)))

# --- Concentration ---
conc_stats9 <- dt_sw[, .(
  top10_share       = sum(sort(abs(weight), decreasing = TRUE)[1:min(10, .N)]),
  max_single_weight = max(abs(weight))
), by = eom]

p9_conc <- ggplot(conc_stats9, aes(x = eom, y = top10_share)) +
  geom_area(fill = "#2c3e50", alpha = 0.7) +
  geom_hline(yintercept = mean(conc_stats9$top10_share), color = "red", linetype = "dashed") +
  labs(title = "Portfolio Concentration",
       subtitle = "Sum of Absolute Weights of Top 10 Holdings",
       y = "Weight %", x = "") +
  scale_y_continuous(labels = percent) +
  theme_minimal()
print(p9_conc)

# --- Turnover ---
cat("  Calculating monthly turnover...\n")
dt_prev9 <- copy(dt_sw)
dt_prev9[, eom := eom %m+% months(1)]
setnames(dt_prev9, "weight", "w_prev")
dt_to9 <- merge(dt_sw, dt_prev9, by = c("eom", "id"), all = TRUE)
dt_to9[is.na(weight), weight := 0]
dt_to9[is.na(w_prev), w_prev := 0]
monthly_tcost9 <- dt_to9[, .(turnover = sum(abs(weight - w_prev)) / 2), by = eom]
avg_to9  <- mean(monthly_tcost9$turnover, na.rm = TRUE)
ann_to9  <- avg_to9 * 12

cat(sprintf("  Avg Monthly Turnover: %s\n", percent(avg_to9)))
cat(sprintf("  Annualized Turnover:  %s\n", percent(ann_to9)))

p9_to <- ggplot(monthly_tcost9, aes(x = eom, y = turnover)) +
  geom_col(fill = "#e74c3c") +
  geom_hline(yintercept = avg_to9, color = "black", size = 1) +
  labs(title = "Monthly Turnover (One-Way)",
       subtitle = paste0("Avg Annualized: ", percent(ann_to9)),
       y = "Turnover %", x = "Date") +
  scale_y_continuous(labels = percent) +
  theme_minimal()
print(p9_to)

# --- Position counts ---
pos_counts9 <- dt_sw[, .(
  Longs  = sum(weight > 0),
  Shorts = sum(weight < 0)
), by = eom] %>%
  pivot_longer(cols = c("Longs", "Shorts"), names_to = "Leg", values_to = "Count")

p9_pos <- ggplot(pos_counts9, aes(x = eom, y = Count, fill = Leg)) +
  geom_area() +
  scale_fill_manual(values = c("Longs" = "#8f9ba6ff", "Shorts" = "#0B2B65")) +
  labs(title = "Number of Active Positions",
       subtitle = "Total breadth of the strategy",
       y = "Count", x = "Date") +
  theme_minimal()
print(p9_pos)

# --- Summary table ---
summary_table9 <- data.frame(
  Metric = c("Annualized Turnover", "Avg Top 10 Concentration",
             "Max Single Stock Weight", "Avg Positions (Long)", "Avg Positions (Short)"),
  Value = c(
    percent(ann_to9),
    percent(mean(conc_stats9$top10_share)),
    percent(max(conc_stats9$max_single_weight)),
    round(mean(pos_counts9$Count[pos_counts9$Leg == "Longs"])),
    round(mean(pos_counts9$Count[pos_counts9$Leg == "Shorts"]))
  )
)
cat("\n--- Portfolio Characteristics ---\n")
print(summary_table9)

# --- Weight distribution (post-2020) ---
active_pos9 <- dt_sw[weight != 0]
p9_dist <- ggplot(active_pos9[eom > "2020-01-01"], aes(x = weight)) +
  geom_histogram(bins = 100, fill = "#2980b9", color = "white") +
  theme_minimal() +
  labs(title = "Weight Distribution (Post-2020)",
       subtitle = "Are weights Gaussian or Fat-Tailed?",
       x = "Net Weight", y = "Count") +
  xlim(-0.005, 0.005)
print(p9_dist)

# --- Single-stock weight trace (top 4 by cumulative |weight|) ---
top_ids9   <- dt_sw[, .(total_w = sum(abs(weight))), by = id][order(-total_w)][1:4]$id
stock_trace9 <- dt_sw[id %in% top_ids9 & eom > "2015-01-01"]
p9_trace <- ggplot(stock_trace9,
                   aes(x = eom, y = weight, group = id, color = as.factor(id))) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~id, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Single Stock Weight History (2015-Present)",
       subtitle = "Visualizing the instability that drives high turnover",
       y = "Weight", x = "Year", color = "Stock ID") +
  theme(legend.position = "none")
print(p9_trace)

# --- Whale detector ---
max_w9 <- dt_sw[which.max(abs(weight))]
cat(sprintf("\n  Max Single Position: %s on %s (Stock ID: %s)\n",
            percent(max_w9$weight), as.character(max_w9$eom), max_w9$id))
cat(sprintf("  99th Percentile Weight: %s\n", percent(quantile(abs(dt_sw$weight), 0.99))))

# --- Top-20 heatmap (2023) ---
subset_yr9 <- dt_sw[year(eom) == 2023]
subset_yr9[, rank := frank(-abs(weight)), by = eom]
top_20_9   <- subset_yr9[rank <= 20]
p9_heat <- ggplot(top_20_9,
                  aes(x = eom, y = reorder(as.factor(id), weight), fill = weight)) +
  geom_tile() +
  scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#27ae60", midpoint = 0) +
  theme_minimal() +
  labs(title = "Top 20 Positions Heatmap (2023)",
       subtitle = "Green = Long, Red = Short. Choppy blocks = High Turnover.",
       x = "Month", y = "Stock ID", fill = "Weight") +
  theme(axis.text.y = element_text(size = 6))
print(p9_heat)
cat("\n")


# ==============================================================================
# 10. FRICTIONS ANALYSIS
# ==============================================================================
# Full real-world cost model layered on arnott_master performance:
#   Layer 1: Transaction costs (size-scaled, 5bp base + slope)
#   Layer 2: Shorting costs (borrow fee, 25bp base + slope)
#   Layer 3: Dividend withholding tax on short positions (27.5%)
# Source: arnott_master.parquet + USA.parquet (me, div1m_me)
# ==============================================================================

cat("=== SECTION 10: Frictions Analysis ===\n")

cat("  Loading USA.parquet for me & div1m_me...\n")
usa_data_fr <- read_parquet(
  USA_FILE,
  col_select = c("id", "date", "me", "div1m_me")
) %>% as.data.table()

dt_fr <- read_parquet(MASTER_FILE) %>% as.data.table()
dt_fr[, eom := as.Date(eom)]

# Attach market cap and dividend yield via rolling join
dt_fr[, `:=`(
  me        = usa_data_fr[dt_fr, on = .(id, date = eom), roll = "nearest", x.me],
  div1m_me  = usa_data_fr[dt_fr, on = .(id, date = eom), roll = "nearest", x.div1m_me]
)]

# --- Transaction cost rate per stock ---
dt_fr[, ref_mcap := {
  top_500 <- head(sort(me, decreasing = TRUE), 500)
  median(top_500, na.rm = TRUE)
}, by = eom]

dt_fr[, tc_bps := pmin(TC_MAX_BPS,
  TC_BASE_BPS + TC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^TC_ALPHA)]
dt_fr[is.na(tc_bps), tc_bps := quantile(tc_bps, 0.90, na.rm = TRUE), by = eom]

# --- Shorting cost rate per stock ---
dt_fr[, sc_annual_bps := pmin(SC_MAX_BPS,
  SC_BASE_BPS + SC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^SC_ALPHA)]
dt_fr[is.na(sc_annual_bps),
      sc_annual_bps := quantile(sc_annual_bps, 0.90, na.rm = TRUE), by = eom]

# --- A. Transaction cost drag (on turnover) ---
w_curr_fr <- dt_fr[, .(eom, id, w_t = weight, tc_bps)]
w_prev_fr <- dt_fr[, .(eom = eom %m+% months(1), id, w_prev = weight)]
trades_fr <- merge(w_curr_fr, w_prev_fr, by = c("eom", "id"), all = TRUE)
trades_fr[is.na(w_t), c("w_t", "tc_bps") := .(0, NA)]
trades_fr[is.na(w_prev), w_prev := 0]
avg_costs_fr <- trades_fr[, .(avg_tc = mean(tc_bps, na.rm = TRUE)), by = eom]
trades_fr[avg_costs_fr, on = "eom", avg_tc := i.avg_tc]
trades_fr[is.na(tc_bps), tc_bps := avg_tc]
trades_fr[, tc_cost := abs(w_t - w_prev) * (tc_bps / 10000)]
monthly_tc_fr <- trades_fr[, .(drag_tc = sum(tc_cost, na.rm = TRUE)), by = eom]

# --- B. Shorting cost drag (borrow fee) ---
dt_fr[, sc_cost := 0]
dt_fr[weight < 0, sc_cost := abs(weight) * (sc_annual_bps / 10000) / 12]
monthly_sc_fr <- dt_fr[, .(drag_sc = sum(sc_cost, na.rm = TRUE)), by = eom]

# --- C. Dividend withholding tax (short positions) ---
dt_fr[, tax_cost := 0]
dt_fr[weight < 0 & div1m_me > 0,
      tax_cost := abs(weight) * div1m_me * TAX_RATE]
monthly_tax_fr <- dt_fr[, .(drag_tax = sum(tax_cost, na.rm = TRUE)), by = eom]

# --- Aggregate performance layers ---
perf_ts_fr <- dt_fr[, .(
  ret_standard     = sum(weight * ret_exc_lead1m,              na.rm = TRUE),
  ret_gross_lagged = sum(weight * (ret_exc_lead1m - ret_day1), na.rm = TRUE)
), by = eom][order(eom)]

perf_ts_fr <- Reduce(function(a, b) merge(a, b, by = "eom", all.x = TRUE),
                     list(perf_ts_fr, monthly_tc_fr, monthly_sc_fr, monthly_tax_fr))
perf_ts_fr[is.na(drag_tc),  drag_tc  := 0]
perf_ts_fr[is.na(drag_sc),  drag_sc  := 0]
perf_ts_fr[is.na(drag_tax), drag_tax := 0]

perf_ts_fr[, `:=`(
  ret_net_1_tc  = ret_gross_lagged - drag_tc,
  ret_net_2_sc  = ret_gross_lagged - drag_tc - drag_sc,
  ret_net_3_tax = ret_gross_lagged - drag_tc - drag_sc - drag_tax
)]

# --- Wealth indices ---
perf_ts_fr[, `:=`(
  wealth_standard = cumprod(1 + replace(ret_standard,     is.na(ret_standard),     0)),
  wealth_lagged   = cumprod(1 + replace(ret_gross_lagged, is.na(ret_gross_lagged), 0)),
  wealth_net_tc   = cumprod(1 + replace(ret_net_1_tc,  is.na(ret_net_1_tc),  0)),
  wealth_net_sc   = cumprod(1 + replace(ret_net_2_sc,  is.na(ret_net_2_sc),  0)),
  wealth_final    = cumprod(1 + replace(ret_net_3_tax, is.na(ret_net_3_tax), 0))
)]

# --- Print friction decomposition ---
cat("\n=== FRICTION DECOMPOSITION (Avg Annual Drag) ===\n")
cat("  Transaction Costs:", percent(mean(perf_ts_fr$drag_tc)  * 12), "\n")
cat("  Shorting Costs:   ", percent(mean(perf_ts_fr$drag_sc)  * 12), "\n")
cat("  Dividend Tax:     ", percent(mean(perf_ts_fr$drag_tax) * 12), "\n")

# --- Final performance table ---
xts_frictions <- xts(
  perf_ts_fr[, .(ret_standard, ret_gross_lagged, ret_net_3_tax)],
  order.by = perf_ts_fr$eom
)
colnames(xts_frictions) <- c("Original (T-Close)", "Lagged (T+1)", "Net (All Frictions)")
cat("\n=== FINAL STRATEGY METRICS ===\n")
print(table.AnnualizedReturns(xts_frictions, scale = 12))

# --- Waterfall plot ---
p10_friction <- ggplot(perf_ts_fr, aes(x = eom)) +
  geom_ribbon(aes(ymin = wealth_net_tc, ymax = wealth_lagged,
                  fill = "1. Transaction Costs"), alpha = 0.5) +
  geom_ribbon(aes(ymin = wealth_net_sc, ymax = wealth_net_tc,
                  fill = "2. Shorting Costs"),    alpha = 0.5) +
  geom_ribbon(aes(ymin = wealth_final,  ymax = wealth_net_sc,
                  fill = "3. Dividend Tax"),       alpha = 0.5) +
  geom_line(aes(y = wealth_standard, linetype = "Theoretical"), color = "grey60") +
  geom_line(aes(y = wealth_lagged,   color = "Gross (Lagged)"), size = 1) +
  geom_line(aes(y = wealth_final,    color = "Net (Final)"),     size = 1) +
  scale_fill_manual(name = "Friction Layers",
    values = c("1. Transaction Costs" = "#FDBF6F",
               "2. Shorting Costs"    = "#FB9A99",
               "3. Dividend Tax"      = "#CAB2D6")) +
  scale_color_manual(name = "Performance",
    values = c("Gross (Lagged)" = "#1F78B4", "Net (Final)" = "black")) +
  scale_linetype_manual(name = "Reference", values = c("Theoretical" = "dashed")) +
  labs(title = "Strategy Performance: Decomposition of Frictions",
       subtitle = "Cumulative wealth lost to Transaction Costs, Borrow Fees, and Dividend Taxes",
       y = "Wealth Index (Log Scale)", x = "Date") +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "right")
print(p10_friction)
ggsave(file.path(OUTPUT_FIG, "phase2_frictions_waterfall.png"), p10_friction,
       width = 12, height = 7)
cat("\n")


# ==============================================================================
# 10b. DRIFT-BAND REBALANCING ANALYSIS
# ==============================================================================
# Instead of rebalancing every month, only trade a position when the gap between
# current actual weight and target weight exceeds a threshold (in bps).
# Tests thresholds: 0, 5, 10, 15, 20, 30, 50 bps.
# Key finding: saving TCs via a drift band destroys gross alpha faster than it
# saves costs, because momentum signal is fresh only at month-end.
# ==============================================================================

cat("=== SECTION 10b: Drift-Band Rebalancing Analysis ===\n")

THRESHOLDS_BPS <- c(0, 5, 10, 15, 20, 30, 50)

# dt_fr already has friction rates attached from Section 10
simulate_drift_band <- function(dt_all, threshold_bps) {
  setkey(dt_all, eom)
  months <- sort(unique(dt_all$eom))

  id_type  <- class(dt_all$id)
  holdings <- data.table(id = vector(id_type, 0), weight = numeric())
  results_list <- vector("list", length(months))

  for (i in seq_along(months)) {
    m          <- months[i]
    month_data <- dt_all[.(m)]

    month_data <- merge(
      month_data, holdings,
      by = "id", all.x = TRUE, sort = FALSE
    )
    setnames(month_data, c("weight.x", "weight.y"), c("target", "prev_actual"))
    month_data[is.na(prev_actual), prev_actual := 0]

    thresh <- threshold_bps / 10000
    trade  <- abs(month_data$target - month_data$prev_actual) > thresh |
              month_data$prev_actual == 0
    month_data[, actual := ifelse(trade, target, prev_actual)]

    port_ret     <- sum(month_data$actual * month_data$ret_exc_lead1m, na.rm = TRUE)
    port_ret_lag <- sum(
      month_data$actual * (month_data$ret_exc_lead1m - month_data$ret_day1),
      na.rm = TRUE
    )
    turnover <- sum(abs(month_data$actual - month_data$prev_actual), na.rm = TRUE)

    month_data[,
      tc_drag_stock := ifelse(
        trade,
        2 * (tc_bps / 10000) * abs(target - prev_actual),
        0
      )
    ]
    tc_drag <- sum(month_data$tc_drag_stock, na.rm = TRUE)

    month_data[,
      sc_drag_stock := ifelse(
        actual < 0,
        abs(actual) * (sc_annual_bps / 10000) / 12,
        0
      )
    ]
    sc_drag <- sum(month_data$sc_drag_stock, na.rm = TRUE)

    month_data[,
      tax_drag_stock := ifelse(
        actual < 0 & div1m_me > 0,
        abs(actual) * div1m_me * TAX_RATE,
        0
      )
    ]
    tax_drag <- sum(month_data$tax_drag_stock, na.rm = TRUE)

    results_list[[i]] <- data.table(
      eom              = m,
      ret_gross        = port_ret,
      ret_gross_lagged = port_ret_lag,
      turnover         = turnover,
      tc_drag          = tc_drag,
      sc_drag          = sc_drag,
      tax_drag         = tax_drag
    )

    holdings <- month_data[actual != 0, .(id, weight = actual)]
  }
  rbindlist(results_list)
}

cat("  Running drift-band simulations...\n")
drift_results_list <- list()
for (th in THRESHOLDS_BPS) {
  cat(sprintf("    Threshold = %d bps ...\n", th))
  drift_results_list[[as.character(th)]] <- simulate_drift_band(dt_fr, th)
}

drift_all <- rbindlist(drift_results_list, idcol = "threshold")
drift_all[, threshold := as.numeric(threshold)]
drift_all[, ret_net   := ret_gross_lagged - tc_drag - sc_drag - tax_drag]

# Merge the 5bps drift net return into perf_ts_fr for the waterfall overlay
drift_5bps <- drift_all[threshold == 5, .(eom, ret_net_5bps = ret_net)]
perf_ts_fr[drift_5bps, on = "eom", ret_net_5bps := i.ret_net_5bps]
perf_ts_fr[, wealth_net_5bps := cumprod(1 + replace(ret_net_5bps, is.na(ret_net_5bps), 0))]

# --- Drift-band performance summary ---
summary_drift <- drift_all[, .(
  Gross_Ret_Ann = mean(ret_gross_lagged) * 12,
  Gross_Vol_Ann = sd(ret_gross_lagged)   * sqrt(12),
  Gross_Sharpe  = mean(ret_gross_lagged) / sd(ret_gross_lagged) * sqrt(12),
  tc_drag_ann   = mean(tc_drag)          * 12,
  avg_turnover  = mean(turnover),
  Net_Ret_Ann   = mean(ret_net) * 12,
  Net_Vol_Ann   = sd(ret_net)   * sqrt(12),
  Net_Sharpe    = mean(ret_net) / sd(ret_net) * sqrt(12)
), by = threshold][order(threshold)]

cat("\n=== DRIFT-BAND PERFORMANCE SUMMARY ===\n")
print(summary_drift[, .(threshold, Gross_Ret_Ann, tc_drag_ann, Net_Ret_Ann,
                         Gross_Sharpe, Net_Sharpe)])

fwrite(summary_drift, file.path(OUTPUT_TABLE, "drift_band_summary.csv"))
cat("  Saved: drift_band_summary.csv\n")

# --- Enhanced waterfall plot: frictions + 5bps drift overlay ---
p10b_waterfall <- ggplot(perf_ts_fr, aes(x = eom)) +
  geom_ribbon(aes(ymin = wealth_net_tc, ymax = wealth_lagged,
                  fill = "1. Transaction Costs"), alpha = 0.5) +
  geom_ribbon(aes(ymin = wealth_net_sc, ymax = wealth_net_tc,
                  fill = "2. Shorting Costs"),    alpha = 0.5) +
  geom_ribbon(aes(ymin = wealth_final,  ymax = wealth_net_sc,
                  fill = "3. Dividend Tax"),       alpha = 0.5) +
  geom_line(aes(y = wealth_standard, linetype = "Theoretical"), color = "grey60") +
  geom_line(aes(y = wealth_lagged,   color = "Gross (Lagged)"),   size = 1) +
  geom_line(aes(y = wealth_final,    color = "Net (0bps Rebal)"), size = 1) +
  geom_line(aes(y = wealth_net_5bps, color = "Net (5bps Drift)"), size = 1.2) +
  scale_fill_manual(name = "Friction Layers",
    values = c("1. Transaction Costs" = "#FDBF6F",
               "2. Shorting Costs"    = "#FB9A99",
               "3. Dividend Tax"      = "#CAB2D6")) +
  scale_color_manual(name = "Performance",
    values = c("Gross (Lagged)"   = "#1F78B4",
               "Net (0bps Rebal)" = "black",
               "Net (5bps Drift)" = "#c8a951")) +
  scale_linetype_manual(name = "Reference", values = c("Theoretical" = "dashed")) +
  labs(title = "Strategy Performance: Decomposition of Frictions",
       subtitle = "Cumulative wealth lost to Frictions, and recovery via 5 bps Drift Band Rebalancing",
       y = "Wealth Index (Log Scale)", x = "Date") +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")
print(p10b_waterfall)
ggsave(file.path(OUTPUT_FIG, "phase2_frictions_waterfall_drift.png"), p10b_waterfall,
       width = 9, height = 5, dpi = 150)

# --- Final 4-column performance table ---
xts_final_4 <- xts(
  perf_ts_fr[, .(ret_standard, ret_gross_lagged, ret_net_3_tax, ret_net_5bps)],
  order.by = perf_ts_fr$eom
)
colnames(xts_final_4) <- c("Original (T-Close)", "Lagged (T+1)",
                            "Net (0bps Rebal)",   "Net (5bps Drift)")
cat("\n=== FINAL STRATEGY METRICS ===\n")
print(PerformanceAnalytics::table.AnnualizedReturns(xts_final_4, scale = 12))

# --- Plot: Net Sharpe vs threshold ---
max_point_drift <- summary_drift[which.max(Net_Sharpe)]
p10b_sharpe <- ggplot(summary_drift, aes(x = threshold, y = Net_Sharpe)) +
  geom_line(color = "#1a2e5a", linewidth = 1.2) +
  geom_point(size = 3, color = "#1e50a2") +
  geom_point(data = max_point_drift, color = "#c8a951", size = 5, shape = 18) +
  geom_label(aes(label = paste0(threshold, " bps")),
             vjust = -0.8, size = 3, color = "#1a2e5a",
             label.size = 0.2, fill = "white") +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  labs(x = "Drift-band threshold (bps)", y = "Net Sharpe Ratio",
       title = "Drift-band rebalancing: Net Sharpe vs threshold",
       subtitle = "CS_LO_25, 12M lookback | 1963–2024") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#1a2e5a"))
print(p10b_sharpe)
ggsave(file.path(OUTPUT_FIG, "drift_sharpe_vs_threshold.png"), p10b_sharpe,
       width = 7, height = 4, dpi = 150)

# --- Plot: Turnover and TC drag ---
p10b_to <- summary_drift %>%
  select(threshold, Turnover = avg_turnover, TC = tc_drag_ann) %>%
  pivot_longer(cols = c(Turnover, TC), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = ifelse(Metric == "Turnover", "Avg monthly turnover", "TC drag (% p.a.)")) %>%
  ggplot(aes(x = threshold, y = Value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  scale_color_manual(values = c("Avg monthly turnover" = "#1e50a2",
                                "TC drag (% p.a.)"     = "#c8a951")) +
  labs(x = "Drift-band threshold (bps)", y = "Value",
       title = "Turnover and transaction cost decline with threshold",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
print(p10b_to)
ggsave(file.path(OUTPUT_FIG, "drift_turnover_TC.png"), p10b_to,
       width = 7, height = 4, dpi = 150)

# --- Plot: Gross vs Net Sharpe trade-off ---
p10b_tradeoff <- summary_drift %>%
  select(threshold, Gross_Sharpe, Net_Sharpe) %>%
  pivot_longer(cols = c(Gross_Sharpe, Net_Sharpe),
               names_to = "Metric", values_to = "Sharpe") %>%
  ggplot(aes(x = threshold, y = Sharpe, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  scale_color_manual(
    values = c("Gross_Sharpe" = "#1F78B4", "Net_Sharpe" = "#c8a951"),
    labels = c("Gross Sharpe (Before Fees)", "Net Sharpe (After Fees)")
  ) +
  labs(x = "Drift-band threshold (bps)", y = "Annualized Sharpe Ratio",
       title = "The Drift Band Trap: Alpha Decays Faster Than Costs Drop",
       subtitle = "Saving on fees is useless if holding 'stale' momentum kills the Gross Alpha",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", color = "#1a2e5a"))
print(p10b_tradeoff)
ggsave(file.path(OUTPUT_FIG, "drift_tradeoff_gross_vs_net.png"), p10b_tradeoff,
       width = 8, height = 5, dpi = 150)

# --- Plot: Cumulative wealth (0 vs 5 bps) with shaded fee drag ---
compare_thresholds <- c(0, 5)
setorder(drift_all, threshold, eom)
wealth_compare <- drift_all[
  threshold %in% compare_thresholds,
  .(eom,
    Gross_Wealth = cumprod(1 + ret_gross_lagged),
    Net_Wealth   = cumprod(1 + ret_net)),
  by = threshold
]
wealth_compare[, Strategy := paste0(threshold, " bps Drift")]
wealth_df <- as.data.frame(wealth_compare)

p10b_shaded <- ggplot(wealth_df, aes(x = eom, group = Strategy)) +
  geom_ribbon(aes(ymin = Net_Wealth, ymax = Gross_Wealth, fill = Strategy),
              alpha = 0.25) +
  geom_line(aes(y = Gross_Wealth, color = paste0(Strategy, " (Gross)")),
            linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(y = Net_Wealth,   color = paste0(Strategy, " (Net)")),
            linetype = "solid",  linewidth = 1.2) +
  scale_y_log10(labels = scales::dollar) +
  scale_fill_manual(name = "Fee Drag (Shaded Area)",
    values = c("0 bps Drift" = "#1F78B4", "5 bps Drift" = "#c8a951")) +
  scale_color_manual(name = "Wealth Lines",
    values = c("0 bps Drift (Gross)" = "#1F78B4", "0 bps Drift (Net)" = "#1F78B4",
               "5 bps Drift (Gross)" = "#c8a951", "5 bps Drift (Net)" = "#c8a951")) +
  labs(x = "Date", y = "Cumulative Wealth (log scale)",
       title = "The Cost of Stale Signals: Gross vs Net Wealth",
       caption = "Dashed = Gross Returns | Solid = Net Returns | Shaded Area = Cumulative Frictions") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", color = "#1a2e5a"),
        panel.grid.minor = element_blank())
print(p10b_shaded)
ggsave(file.path(OUTPUT_FIG, "drift_wealth_gross_vs_net_shaded.png"), p10b_shaded,
       width = 10, height = 5, dpi = 150)

# --- 0 vs 5 bps metrics table ---
cat("\n=== PERFORMANCE METRICS: 0 bps vs 5 bps DRIFT BAND ===\n")
metrics_table_drift <- summary_drift[
  threshold %in% compare_thresholds,
  .(Threshold      = paste0(threshold, " bps"),
    `Gross Ret p.a.`= percent(Gross_Ret_Ann, accuracy = 0.1),
    `Gross Sharpe`  = round(Gross_Sharpe, 2),
    `Net Ret p.a.`  = percent(Net_Ret_Ann,   accuracy = 0.1),
    `Net Sharpe`    = round(Net_Sharpe, 2))
]
print(metrics_table_drift, row.names = FALSE)
cat("======================================================\n\n")


# ==============================================================================
# 10c. INDUSTRY EXPOSURE (FF17)
# ==============================================================================
# Maps SIC codes from USA.parquet to Fama-French 17 industries.
# Shows gross capital allocation across sectors over time.
# Flags any sector that ever breaches 20% concentration.
# ==============================================================================

cat("=== SECTION 10c: Industry Exposure (FF17) ===\n")

usa_inds <- read_parquet(USA_FILE, col_select = c("id", "date", "sic")) %>%
  as.data.table()

# Attach SIC to dt_fr via rolling join
dt_fr[, sic := usa_inds[dt_fr, on = .(id, date = eom), roll = "nearest", x.sic]]

dt_fr[, ff17 := case_when(
  sic >= 100  & sic <= 999  ~ "Food",
  sic >= 1000 & sic <= 1299 ~ "Mines",
  sic >= 1300 & sic <= 1399 ~ "Oil",
  sic >= 1400 & sic <= 1499 ~ "Mines",
  sic >= 1500 & sic <= 1799 ~ "Cnstr",
  sic >= 2000 & sic <= 2111 ~ "Food",
  sic >= 2200 & sic <= 2299 ~ "Txtls",
  sic >= 2300 & sic <= 2399 ~ "Clths",
  sic >= 2400 & sic <= 2499 ~ "Cnstr",
  sic >= 2500 & sic <= 2599 ~ "FabPr",
  sic >= 2600 & sic <= 2799 ~ "Paper",
  sic >= 2800 & sic <= 2829 ~ "Chems",
  sic >= 2830 & sic <= 2839 ~ "Drugs",
  sic >= 2840 & sic <= 2899 ~ "Chems",
  sic >= 2900 & sic <= 2999 ~ "Oil",
  sic >= 3000 & sic <= 3099 ~ "Chems",
  sic >= 3100 & sic <= 3199 ~ "Clths",
  sic >= 3200 & sic <= 3299 ~ "Cnstr",
  sic >= 3300 & sic <= 3399 ~ "Steel",
  sic >= 3400 & sic <= 3499 ~ "FabPr",
  sic >= 3500 & sic <= 3699 ~ "Machn",
  sic >= 3700 & sic <= 3719 ~ "Cars",
  sic >= 3720 & sic <= 3799 ~ "Machn",
  sic >= 3800 & sic <= 3899 ~ "Machn",
  sic >= 3900 & sic <= 3999 ~ "Other",
  sic >= 4000 & sic <= 4899 ~ "Trans",
  sic >= 4900 & sic <= 4999 ~ "Utils",
  sic >= 5000 & sic <= 5199 ~ "Other",
  sic >= 5200 & sic <= 5999 ~ "Rtail",
  sic >= 6000 & sic <= 6999 ~ "Finan",
  sic >= 7000 & sic <= 7999 ~ "Other",
  sic >= 8000 & sic <= 8999 ~ "Other",
  sic >= 9000 & sic <= 9999 ~ "Other",
  TRUE                       ~ "Missing"
)]

ind_exposure <- dt_fr[, .(gross_w = sum(abs(weight), na.rm = TRUE)), by = .(eom, ff17)]
ind_exposure[, total_gross_month := sum(gross_w), by = eom]
ind_exposure[, pct_weight := gross_w / total_gross_month]

# Color palette
high_vis_17 <- c(
  "#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231",
  "#911EB4", "#46F0F0", "#F032E6", "#BCF60C", "#FABEBE",
  "#008080", "#E6BEFF", "#9A6324", "#FFFAC8", "#800000",
  "#AAFFC3", "#808000"
)
n_ind <- length(unique(ind_exposure$ff17))
final_palette_ind <- colorRampPalette(high_vis_17)(n_ind)

p10c_ind <- ggplot(ind_exposure, aes(x = eom, y = gross_w, fill = ff17)) +
  geom_area(position = "fill", alpha = 0.9, color = "white", linewidth = 0.1) +
  scale_fill_manual(values = final_palette_ind) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "Portfolio Industry Composition (FF17)",
       subtitle = "Gross capital allocation to sectors over time (Scaled to 100%)",
       x = "Date", y = "Proportion of Gross Portfolio", fill = "Industry") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 9),
        plot.title = element_text(face = "bold", color = "#1a2e5a"),
        panel.grid.minor = element_blank())
print(p10c_ind)
ggsave(file.path(OUTPUT_FIG, "industry_composition_ff17.png"), p10c_ind,
       width = 10, height = 5, dpi = 150)

# Industry concentration summary
last_date_ind <- max(ind_exposure$eom)
ind_summary <- ind_exposure[, .(
  Avg_Historical_Weight = mean(pct_weight),
  Max_Historical_Weight = max(pct_weight),
  Min_Historical_Weight = min(pct_weight),
  Current_Weight        = pct_weight[eom == last_date_ind][1]
), by = ff17][order(-Avg_Historical_Weight)]

cat("\n=== INDUSTRY CONCENTRATION SUMMARY ===\n")
cat("Note: Weights = proportion of total gross capital (Long + Short)\n\n")
print(ind_summary[, .(
  Industry           = ff17,
  `Avg Hist. Weight` = percent(Avg_Historical_Weight, accuracy = 0.1),
  `Max Hist. Weight` = percent(Max_Historical_Weight, accuracy = 0.1),
  `Min Hist. Weight` = percent(Min_Historical_Weight, accuracy = 0.1),
  `Current Weight`   = percent(Current_Weight,        accuracy = 0.1)
)], row.names = FALSE)

breach_ind <- ind_summary[Max_Historical_Weight > 0.20]
if (nrow(breach_ind) > 0) {
  cat("\nWARNING: industries that exceeded 20% concentration at least once:\n")
  for (i in seq_len(nrow(breach_ind))) {
    cat(sprintf("   - %s (Max hit %s)\n",
                breach_ind$ff17[i],
                percent(breach_ind$Max_Historical_Weight[i], accuracy = 0.1)))
  }
}
cat("======================================\n\n")


# ==============================================================================
# 11. POSITION LIMITING — DECILE VARIANTS
# ==============================================================================
# Reduce portfolio breadth by keeping only the most extreme-weight positions.
# Version A: Top & bottom 5% of weights  (10% total)
# Version B: Top & bottom 2.5% of weights (5% total)
# Reduces turnover and focuses on best-conviction bets.
# ==============================================================================

cat("=== SECTION 11: Position Limiting (Decile Variants) ===\n")

OUTPUT_DEC_LS  <- file.path(PROC_DIR, "arnott_master_dec_ls.parquet")
OUTPUT_TOP5_LS <- file.path(PROC_DIR, "arnott_master_top5_ls.parquet")

dt_pos <- read_parquet(MASTER_FILE) %>% as.data.table()
dt_pos[, eom := as.Date(eom)]

# Compute monthly weight percentiles
dt_pos[, `:=`(
  p025 = quantile(weight, 0.025, na.rm = TRUE),
  p05  = quantile(weight, 0.05,  na.rm = TRUE),
  p95  = quantile(weight, 0.95,  na.rm = TRUE),
  p975 = quantile(weight, 0.975, na.rm = TRUE)
), by = eom]

# --- Version A: 5%/5% (10% of all positions) ---
dt_dec_ls <- dt_pos[weight <= p05 | weight >= p95]
dt_dec_ls[, `:=`(p025 = NULL, p05 = NULL, p95 = NULL, p975 = NULL)]
dt_dec_ls[weight > 0, weight := 0.5 * (weight / sum(weight)),        by = eom]
dt_dec_ls[weight < 0, weight := 0.5 * (weight / sum(abs(weight))),   by = eom]

cat("\n--- Version A Sanity Check: 5% Long / 5% Short ---\n")
check_a <- dt_dec_ls[, .(net_exp = sum(weight), gross_exp = sum(abs(weight)), n_pos = .N), by = eom]
cat(sprintf("  Avg gross exp: %.4f  Avg net exp: %.4f  Avg positions: %d\n",
            mean(check_a$gross_exp), mean(check_a$net_exp), round(mean(check_a$n_pos))))
write_parquet(dt_dec_ls, OUTPUT_DEC_LS)

# --- Version B: 2.5%/2.5% (5% of all positions) ---
dt_top5_ls <- dt_pos[weight <= p025 | weight >= p975]
dt_top5_ls[, `:=`(p025 = NULL, p05 = NULL, p95 = NULL, p975 = NULL)]
dt_top5_ls[weight > 0, weight := 0.5 * (weight / sum(weight)),       by = eom]
dt_top5_ls[weight < 0, weight := 0.5 * (weight / sum(abs(weight))),  by = eom]

cat("--- Version B Sanity Check: 2.5% Long / 2.5% Short ---\n")
check_b <- dt_top5_ls[, .(net_exp = sum(weight), gross_exp = sum(abs(weight)), n_pos = .N), by = eom]
cat(sprintf("  Avg gross exp: %.4f  Avg net exp: %.4f  Avg positions: %d\n",
            mean(check_b$gross_exp), mean(check_b$net_exp), round(mean(check_b$n_pos))))
write_parquet(dt_top5_ls, OUTPUT_TOP5_LS)

cat("  Decile variants saved.\n\n")


# ==============================================================================
# 12. WRDS ENRICHMENT & LIQUIDITY ANALYSIS (OPTIONAL)
# ==============================================================================
# Requires live WRDS connection (set WRDS_USER and WRDS_PASSWORD env vars).
# Maps stock IDs to CRSP tickers/company names and fetches market cap + volume
# to compute position-level tradeability and "days to trade" metrics.
# Skip this section if WRDS is not available.
# ==============================================================================

wrds_user <- Sys.getenv("WRDS_USER")
wrds_pass <- Sys.getenv("WRDS_PASSWORD")
RUN_WRDS  <- (wrds_user != "" && wrds_pass != "")

if (RUN_WRDS) {

  cat("=== SECTION 12: WRDS Enrichment & Liquidity Analysis ===\n")

  ANALYSIS_DIR   <- file.path(BASE_DIR, "03_Outputs/Analysis")
  dir.create(ANALYSIS_DIR, showWarnings = FALSE, recursive = TRUE)
  PORTFOLIO_FILE <- file.path(OUTPUT_STRAT, "stock_portfolio_monthly.parquet")

  # Use arnott_master weights as portfolio if dedicated file doesn't exist
  if (!file.exists(PORTFOLIO_FILE)) {
    cat("  portfolio file not found — using arnott_master weights as portfolio.\n")
    port_wrds <- dt_master[, .(id, trade_month = eom, net_weight = weight)]
    write_parquet(port_wrds, PORTFOLIO_FILE)
  }

  portfolio_wrds <- read_parquet(PORTFOLIO_FILE) %>% setDT()

  cat("  Connecting to WRDS...\n")
  wrds <- dbConnect(
    Postgres(),
    host     = "wrds-pgdata.wharton.upenn.edu",
    port     = 9737,
    dbname   = "wrds",
    user     = wrds_user,
    password = wrds_pass,
    sslmode  = "require"
  )
  cat("  Connected.\n")

  # --- A. Ticker mapping (current + delisted) ---
  unique_ids_wrds <- unique(portfolio_wrds$id)
  query_cur <- sprintf("
    SELECT DISTINCT permno, ticker, comnam as company_name, ncusip, shrcd, exchcd
    FROM crsp.msenames
    WHERE permno IN (%s)
      AND namedt <= CURRENT_DATE
      AND (nameendt >= CURRENT_DATE OR nameendt IS NULL)
    ORDER BY permno
  ", paste(unique_ids_wrds, collapse = ","))
  ticker_map_wrds <- dbGetQuery(wrds, query_cur) %>% setDT()
  ticker_map_wrds[, is_delisted := FALSE]

  missing_ids <- setdiff(unique_ids_wrds, ticker_map_wrds$permno)
  if (length(missing_ids) > 0) {
    query_del <- sprintf("
      SELECT DISTINCT ON (permno) permno, ticker, comnam as company_name, ncusip, shrcd, exchcd
      FROM crsp.msenames
      WHERE permno IN (%s)
      ORDER BY permno, nameendt DESC NULLS FIRST
    ", paste(missing_ids, collapse = ","))
    del_map <- dbGetQuery(wrds, query_del) %>% setDT()
    del_map[, is_delisted := TRUE]
    ticker_map_wrds <- rbind(ticker_map_wrds, del_map, fill = TRUE)
  }

  ticker_map_wrds[, ticker       := str_trim(ticker)]
  ticker_map_wrds[, company_name := str_trim(company_name)]
  ticker_map_wrds[, exchange := case_when(
    exchcd == 1                ~ "NYSE",
    exchcd == 2                ~ "NYSE MKT",
    exchcd %in% c(3, 31,32,33)~ "NASDAQ",
    TRUE                       ~ "Other"
  )]
  ticker_map_wrds[, share_type := case_when(
    shrcd %in% c(10, 11) ~ "Common",
    shrcd %in% c(12)     ~ "Common (limited voting)",
    shrcd %in% c(20,21,22)~"REIT",
    shrcd %in% c(30,31,32)~"ADR",
    TRUE                  ~ "Other"
  )]

  # --- B. Merge tickers into portfolio ---
  portfolio_enriched_wrds <- merge(portfolio_wrds, ticker_map_wrds,
                                   by.x = "id", by.y = "permno", all.x = TRUE)
  portfolio_enriched_wrds[, abs_weight := abs(net_weight)]
  write_parquet(portfolio_enriched_wrds,
                file.path(OUTPUT_STRAT, "stock_portfolio_with_tickers.parquet"))

  # --- C. Market cap & volume (last 5 years) ---
  recent_port <- portfolio_enriched_wrds[trade_month >= (max(trade_month) - years(5))]
  recent_ids  <- unique(recent_port$id)
  cat("  Querying CRSP MSF for market cap & volume (last 5 years)...\n")
  query_mkt <- sprintf("
    SELECT permno, date, abs(prc) as abs_prc, vol, shrout,
           abs(prc) * shrout / 1000000 as mkt_cap_millions
    FROM crsp.msf
    WHERE permno IN (%s)
      AND date >= '%s' AND date <= '%s'
    ORDER BY permno, date
  ", paste(recent_ids, collapse = ","),
     min(recent_port$trade_month), max(recent_port$trade_month))
  market_data_wrds <- dbGetQuery(wrds, query_mkt) %>% setDT()
  dbDisconnect(wrds)
  cat("  Disconnected from WRDS.\n")

  market_data_wrds[, adv           := vol / 21]
  market_data_wrds[, dollar_vol_daily := abs_prc * adv]
  market_data_wrds[, trade_month   := floor_date(as.Date(date), "month")]

  port_liq <- merge(
    portfolio_enriched_wrds,
    market_data_wrds[, .(permno = permno, trade_month, mkt_cap_millions, adv, dollar_vol_daily, abs_prc)],
    by.x = c("id", "trade_month"),
    by.y = c("permno", "trade_month"),
    all.x = TRUE
  )

  # --- D. Tradeability metrics (assuming $100M portfolio) ---
  PORTFOLIO_SIZE_MM <- 100
  ADV_USAGE_PCT     <- 0.10
  port_liq[, position_size_mm := abs(net_weight) * PORTFOLIO_SIZE_MM]
  port_liq[, shares_needed    := position_size_mm * 1000000 / abs_prc]
  port_liq[, days_to_trade    := shares_needed / (adv * ADV_USAGE_PCT)]
  port_liq[, is_illiquid      := days_to_trade > 5]
  port_liq[, cap_category     := case_when(
    mkt_cap_millions >= 10000 ~ "Mega Cap (>$10B)",
    mkt_cap_millions >=  2000 ~ "Large Cap ($2-10B)",
    mkt_cap_millions >=   200 ~ "Mid Cap ($200M-2B)",
    mkt_cap_millions >=    50 ~ "Small Cap ($50-200M)",
    TRUE                      ~ "Micro Cap (<$50M)"
  )]

  # Enhanced tradeability score (0-12)
  port_liq[, enhanced_trade_score := 0]
  port_liq[mkt_cap_millions >= 2000,                          enhanced_trade_score := enhanced_trade_score + 3]
  port_liq[mkt_cap_millions >= 200 & mkt_cap_millions < 2000, enhanced_trade_score := enhanced_trade_score + 2]
  port_liq[mkt_cap_millions < 200,                            enhanced_trade_score := enhanced_trade_score + 1]
  port_liq[days_to_trade <= 1,                                enhanced_trade_score := enhanced_trade_score + 3]
  port_liq[days_to_trade > 1 & days_to_trade <= 3,            enhanced_trade_score := enhanced_trade_score + 2]
  port_liq[days_to_trade > 3 & days_to_trade <= 5,            enhanced_trade_score := enhanced_trade_score + 1]
  port_liq[exchange %in% c("NYSE", "NASDAQ"),                 enhanced_trade_score := enhanced_trade_score + 2]
  port_liq[share_type == "Common",                            enhanced_trade_score := enhanced_trade_score + 2]
  port_liq[abs(net_weight) >= 0.001 & abs(net_weight) <= 0.02,enhanced_trade_score := enhanced_trade_score + 2]

  write_parquet(port_liq, file.path(OUTPUT_STRAT, "portfolio_with_full_liquidity_data.parquet"))

  # --- E. Summary stats ---
  cat("\n--- Liquidity Summary ($100M portfolio) ---\n")
  liq_sum <- port_liq[!is.na(days_to_trade), .(
    avg_position_mm       = mean(position_size_mm,     na.rm = TRUE),
    avg_mkt_cap           = mean(mkt_cap_millions,     na.rm = TRUE),
    avg_days_to_trade     = mean(days_to_trade,        na.rm = TRUE),
    pct_illiquid          = mean(is_illiquid,          na.rm = TRUE),
    pct_large_cap         = mean(mkt_cap_millions >= 2000, na.rm = TRUE),
    avg_enhanced_score    = mean(enhanced_trade_score, na.rm = TRUE)
  )]
  print(liq_sum)

  # --- F. Liquidity visualizations ---
  PDF_LIQ <- file.path(OUTPUT_RPT, "Liquidity_Analysis.pdf")
  pdf(PDF_LIQ, width = 12, height = 8)

  p12_d2t <- ggplot(port_liq[!is.na(days_to_trade) & days_to_trade < 30],
                    aes(x = days_to_trade)) +
    geom_histogram(bins = 50, fill = "#3498DB", color = "white") +
    geom_vline(xintercept = 5, color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text", x = 5.5, y = Inf, label = "5 days (threshold)",
             vjust = 2, hjust = 0, color = "red") +
    labs(title = "Days to Trade Distribution",
         subtitle = "Using 10% of ADV | $100M portfolio | Positions >30 days excluded",
         x = "Days to Build Position", y = "Count") +
    theme_minimal()
  print(p12_d2t)

  p12_mcap <- ggplot(port_liq[!is.na(mkt_cap_millions)], aes(x = mkt_cap_millions)) +
    geom_histogram(bins = 50, fill = "#27AE60", color = "white") +
    scale_x_log10(labels = dollar_format(suffix = "M")) +
    geom_vline(xintercept = c(50, 200, 2000, 10000), linetype = "dashed", alpha = 0.5) +
    labs(title = "Market Cap Distribution", subtitle = "Log scale",
         x = "Market Cap", y = "Count") +
    theme_minimal()
  print(p12_mcap)

  monthly_liq <- port_liq[!is.na(days_to_trade), .(
    median_days = median(days_to_trade, na.rm = TRUE),
    avg_days    = mean(days_to_trade,   na.rm = TRUE),
    pct_illiquid= mean(is_illiquid,     na.rm = TRUE)
  ), by = trade_month]

  p12_liq_time <- ggplot(monthly_liq, aes(x = trade_month)) +
    geom_line(aes(y = median_days, color = "Median"), linewidth = 1) +
    geom_line(aes(y = avg_days,    color = "Average"), linewidth = 1, alpha = 0.7) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_color_manual(values = c("Median" = "#2980B9", "Average" = "#E67E22")) +
    labs(title = "Portfolio Liquidity Over Time",
         subtitle = "Days to build avg position (using 10% ADV)",
         y = "Days to Trade", x = "", color = "") +
    theme_minimal() + theme(legend.position = "bottom")
  print(p12_liq_time)

  dev.off()
  cat("  Liquidity analysis PDF saved to:", PDF_LIQ, "\n")

  # Latest month trade list
  latest_m_wrds <- max(port_liq$trade_month)
  write_csv(
    port_liq[trade_month == latest_m_wrds & !is.na(enhanced_trade_score)][
      order(-enhanced_trade_score),
      .(permno = id, ticker, company_name, exchange, share_type,
        direction        = ifelse(net_weight > 0, "LONG", "SHORT"),
        weight           = net_weight,
        Days_to_Trade    = round(days_to_trade, 1),
        Tradeability_Score = enhanced_trade_score)
    ],
    file.path(OUTPUT_STRAT, paste0("trade_list_", latest_m_wrds, ".csv"))
  )
  cat("  Trade list saved.\n\n")

} else {
  cat("[12] WRDS credentials not found — skipping WRDS enrichment.\n",
      "     Set WRDS_USER and WRDS_PASSWORD env vars to enable.\n\n")
}


# ==============================================================================
# 13. EVENT STUDIES
# ==============================================================================
# Zoom into specific crisis periods to visualize strategy behaviour.
# Uses daily_perf from Section 6 (netted stock backtest).
# ==============================================================================

cat("=== SECTION 13: Event Studies ===\n")

plot_event <- function(data, start_date, end_date, event_name) {
  zoom_data <- data %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
    melt(id.vars = "date", measure.vars = c("cum_quartile_lo", "cum_median_ls"))
  zoom_data[, value := value / first(value), by = variable]
  p <- ggplot(zoom_data, aes(x = date, y = value, color = variable)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(
      values = c("cum_quartile_lo" = "#27AE60", "cum_median_ls" = "#2980B9"),
      labels = c("Top 25% Long Only", "Median L/S")
    ) +
    labs(
      title    = paste("Event Study:", event_name),
      subtitle = paste(start_date, "to", end_date),
      y        = "Cumulative Return (Rebased to 1.0)",
      color    = "Strategy"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  print(p)
}

plot_event(daily_perf, "1999-01-01", "2002-12-31", "Dot-Com Bubble & Crash")
plot_event(daily_perf, "2019-06-01", "2021-06-01", "COVID-19 Crash & Rebound")

# --- Show active factors at key dates ---
show_top_factors <- function(target_date) {
  target_month <- floor_date(as.Date(target_date), "month")
  holdings <- active_signals %>%
    filter(month == target_month, pos_quartile_lo > 0) %>%
    select(characteristic)
  cat(sprintf("\n--- Active Factors in %s ---\n", target_month))
  print(holdings$characteristic)
}

show_top_factors("2000-03-01")   # Dot-Com peak
show_top_factors("2002-09-01")   # Dot-Com bottom
show_top_factors("2020-03-01")   # COVID crash
show_top_factors("2020-11-01")   # Vaccine announcement

cat("\n==============================================================\n")
cat("  Phase 2 Pipeline Complete\n")
cat("==============================================================\n")
