#!/usr/bin/env Rscript
# =============================================================================
#  Phase2_DriftBand_Analysis.R  (OPTIMISED VERSION)
#  Drift-Band Rebalancing Sensitivity Analysis
#  Strategy: CS_LO_25, Lookback: 12M (best net performer after frictions)
#
#  OPTIMISATIONS:
#    - Pre‑split data by month → no repeated filtering
#    - Parallel execution over thresholds (optional, configurable)
#    - Pre‑compute per‑month mean TC
# =============================================================================

cat("=============================================================\n")
cat("  DRIFT-BAND REBALANCING ANALYSIS (OPTIMISED)\n")
cat("  Strategy: CS_LO_25, Lookback: 12M\n")
cat("=============================================================\n\n")

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(readr)
})

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
MASTER_PATH <- "01_Data/Processed/Phase2/phase2_master.parquet"
FRIC_PATH <- "01_Data/Processed/Phase2/phase2_frictions_CS_LO_25_12M.parquet"
OUT_DIR <- "01_Data/Processed/Phase2"
FIG_DIR <- "03_Outputs/Figures"

STRATEGY <- "CS_LO_25"
LOOKBACK <- "12M"

# Thresholds in basis points
THRESHOLDS_BPS <- c(0, 5, 10, 15, 20, 30, 50)

# TC parameters (same as frictions script)
TC_BASE <- 5 # bps
TC_SLOPE <- 1 # bps
TC_ALPHA <- 0.25
TC_CAP <- 100 # bps max

# Annual SC and Tax drag (from frictions summary)
SC_ANN <- 0.0022 # 0.22% pa
TAX_ANN <- 0.0029 # 0.29% pa

# Set to TRUE to run thresholds in parallel (requires parallel package)
USE_PARALLEL <- TRUE
N_CORES <- parallel::detectCores() - 1 # leave one core free

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# [1] Load data
# ---------------------------------------------------------------------------
cat("[1/5] Loading master parquet (CS_LO_25, 12M)...\n")
t0 <- proc.time()

df_raw <- open_dataset(MASTER_PATH) |>
  filter(strategy == STRATEGY, lookback == LOOKBACK) |>
  select(eom_signal, eom_hold, id, net_weight, me, ret_exc_lead1m) |>
  collect() |>
  arrange(eom_signal, id)

cat(sprintf(
  "      Loaded %s rows | %d unique stocks | %d unique months\n",
  formatC(nrow(df_raw), big.mark = ","),
  n_distinct(df_raw$id),
  n_distinct(df_raw$eom_signal)
))
cat(sprintf(
  "      Date range: %s -> %s\n",
  min(df_raw$eom_signal),
  max(df_raw$eom_signal)
))
cat(sprintf("      Load time: %.1f sec\n", (proc.time() - t0)["elapsed"]))

# Load lag-adjusted factor returns for correction
fric_lag <- tryCatch(
  {
    read_parquet(FRIC_PATH) |>
      filter(version == "full") |>
      select(eom = eom, ret_gross_lag, ret_gross) |>
      mutate(lag_adj = ret_gross_lag - ret_gross)
  },
  error = function(e) {
    cat(
      "      Warning: frictions parquet not found; using approx lag correction\n"
    )
    NULL
  }
)

# ---------------------------------------------------------------------------
# [2] Compute top-500 reference ME for TC model
# ---------------------------------------------------------------------------
cat("[2/5] Computing TC reference (top-500 ME per month)...\n")

me_ref <- df_raw |>
  group_by(eom_signal) |>
  summarise(
    ref_me = median(me[rank(-me) <= 500], na.rm = TRUE),
    .groups = "drop"
  )

df <- df_raw |>
  left_join(me_ref, by = "eom_signal") |>
  mutate(
    tc_bps_ow = pmin(
      TC_CAP,
      TC_BASE + TC_SLOPE * (ref_me / pmax(me, 1))^TC_ALPHA
    ),
    tc_rate = tc_bps_ow / 10000
  )

cat(sprintf(
  "      Median TC rate (one-way): %.1f bps\n",
  median(df$tc_bps_ow, na.rm = TRUE)
))

# ---------------------------------------------------------------------------
# [3] Compute previous target weight for each stock
# ---------------------------------------------------------------------------
cat(
  "[3/5] Computing lagged target weights (Δw = target - previous target)...\n"
)

df <- df |>
  arrange(id, eom_signal) |>
  group_by(id) |>
  mutate(
    prev_target_w = lag(net_weight, 1L),
    prev_target_w = coalesce(prev_target_w, 0)
  ) |>
  ungroup()

delta_summary <- df |>
  mutate(delta_abs_bps = abs(net_weight - prev_target_w) * 10000) |>
  summarise(
    p05 = quantile(delta_abs_bps, 0.05),
    p25 = quantile(delta_abs_bps, 0.25),
    p50 = quantile(delta_abs_bps, 0.50),
    p75 = quantile(delta_abs_bps, 0.75),
    p90 = quantile(delta_abs_bps, 0.90),
    p99 = quantile(delta_abs_bps, 0.99)
  )

cat("      Distribution of |Δweight| (bps):\n")
cat(sprintf(
  "        p05=%.1f | p25=%.1f | p50=%.1f | p75=%.1f | p90=%.1f | p99=%.1f\n",
  delta_summary$p05,
  delta_summary$p25,
  delta_summary$p50,
  delta_summary$p75,
  delta_summary$p90,
  delta_summary$p99
))

# ---------------------------------------------------------------------------
# Pre-split data by month (critical optimisation!)
# ---------------------------------------------------------------------------
cat("[4/5] Pre-splitting data by month...\n")
month_list <- split(df, df$eom_signal) # list of data frames, one per month
# Ensure months are in chronological order
month_list <- month_list[order(as.Date(names(month_list)))]

# Pre-compute per-month mean TC rate (used for exit trades)
month_mean_tc <- sapply(month_list, function(d) mean(d$tc_rate, na.rm = TRUE))

cat(sprintf("      Split into %d months\n", length(month_list)))

# ---------------------------------------------------------------------------
# [4b] Simulation function (now uses pre-split month_list)
# ---------------------------------------------------------------------------
simulate_drift_band <- function(thresh_bps, month_list, month_mean_tc) {
  thresh <- thresh_bps / 10000
  held_w <- setNames(numeric(0), character(0))
  n_months <- length(month_list)
  monthly_results <- vector("list", n_months)

  for (i in seq_len(n_months)) {
    month_data <- month_list[[i]]
    mean_tc <- month_mean_tc[i]

    ids_target <- as.character(month_data$id)
    ids_held <- names(held_w)

    # Exits: stocks in held but not in target
    ids_exit <- setdiff(ids_held, ids_target)
    exit_w <- held_w[ids_exit]

    # For target stocks, get previous held weight (0 if new)
    prev_w_vec <- held_w[ids_target]
    prev_w_vec[is.na(prev_w_vec)] <- 0

    # Determine trades
    delta_w <- month_data$net_weight - prev_w_vec
    do_trade <- abs(delta_w) > thresh | prev_w_vec == 0 # always trade new entries

    # New actual weight after applying drift-band rule
    new_actual <- ifelse(do_trade, month_data$net_weight, prev_w_vec)

    # Portfolio return
    port_ret <- sum(new_actual * month_data$ret_exc_lead1m, na.rm = TRUE)

    # TC drag: on trades + exits
    traded_amt <- ifelse(do_trade, abs(delta_w), 0)
    tc_drag <- sum(traded_amt * month_data$tc_rate * 2, na.rm = TRUE) +
      sum(exit_w, na.rm = TRUE) * mean_tc * 2 # exits: round-trip

    # Turnover (one-way)
    turnover <- sum(abs(traded_amt)) + sum(exit_w)

    monthly_results[[i]] <- data.frame(
      eom = as.Date(names(month_list)[i]),
      threshold = thresh_bps,
      ret = port_ret,
      tc_drag = tc_drag,
      turnover = turnover,
      n_trades = sum(do_trade) + length(exit_w),
      n_stocks = nrow(month_data)
    )

    # Update held weights for next month
    held_df <- data.frame(id = ids_target, w = new_actual)
    held_df <- held_df[held_df$w != 0, ]
    held_w <- setNames(held_df$w, as.character(held_df$id))
  }

  do.call(rbind, monthly_results)
}

# ---------------------------------------------------------------------------
# [5] Run simulations (optionally in parallel)
# ---------------------------------------------------------------------------
cat("[5/5] Simulating drift-band portfolios...\n")

if (USE_PARALLEL && requireNamespace("parallel", quietly = TRUE)) {
  cat(sprintf("      Using parallel processing with %d cores\n", N_CORES))
  cl <- parallel::makeCluster(N_CORES)
  # Export both data AND the function to workers
  parallel::clusterExport(
    cl,
    varlist = c("month_list", "month_mean_tc", "simulate_drift_band"),
    envir = environment()
  )
  # Load dplyr on workers (though not strictly needed inside function)
  parallel::clusterEvalQ(cl, library(dplyr))

  all_results <- parallel::parLapply(cl, THRESHOLDS_BPS, function(th) {
    simulate_drift_band(th, month_list, month_mean_tc)
  })
  parallel::stopCluster(cl)
} else {
  # ... sequential version remains unchanged ...
  cat("      Running sequentially...\n")
  all_results <- lapply(THRESHOLDS_BPS, function(th) {
    cat(sprintf("      Threshold = %d bps...\n", th))
    t_start <- proc.time()
    res <- simulate_drift_band(th, month_list, month_mean_tc)
    elapsed <- (proc.time() - t_start)["elapsed"]
    cat(sprintf(
      "        Done in %.1f sec | Avg turnover: %.3f | Avg TC: %.5f\n",
      elapsed,
      mean(res$turnover),
      mean(res$tc_drag)
    ))
    res
  })
}

results_df <- do.call(rbind, all_results)

# ---------------------------------------------------------------------------
# [6] Compute performance metrics
# ---------------------------------------------------------------------------
cat("[6/5] Computing performance summary...\n")

# Lag correction
if (!is.null(fric_lag)) {
  lag_correction_monthly <- mean(fric_lag$lag_adj, na.rm = TRUE)
} else {
  lag_correction_monthly <- -0.0149 / 12
}
cat(sprintf(
  "      Lag correction (monthly avg): %.5f (%.2f%% pa)\n",
  lag_correction_monthly,
  lag_correction_monthly * 12 * 100
))

SC_MONTHLY <- SC_ANN / 12
TAX_MONTHLY <- TAX_ANN / 12

# Proper SR from monthly return series
sr_from_monthly <- results_df |>
  mutate(
    ret_net = ret - tc_drag - SC_MONTHLY - TAX_MONTHLY,
    ret_lag_net = ret -
      tc_drag -
      SC_MONTHLY -
      TAX_MONTHLY +
      lag_correction_monthly
  ) |>
  group_by(threshold) |>
  summarise(
    gross_sr = mean(ret) / sd(ret) * sqrt(12),
    net_sr = mean(ret_net) / sd(ret_net) * sqrt(12),
    net_sr_lag = mean(ret_lag_net) / sd(ret_lag_net) * sqrt(12),
    gross_ret_ann = mean(ret) * 12,
    net_ret_ann = mean(ret_net) * 12,
    net_ret_lag_ann = mean(ret_lag_net) * 12,
    vol_ann = sd(ret) * sqrt(12),
    .groups = "drop"
  )

# Summary with drag components
summary_stats <- results_df |>
  group_by(threshold) |>
  summarise(
    n_months = n(),
    avg_turnover = mean(turnover),
    avg_tc_month = mean(tc_drag),
    drag_tc_ann = mean(tc_drag) * 12,
    drag_sc_ann = SC_ANN,
    drag_tax_ann = TAX_ANN,
    drag_total_ann = drag_tc_ann + drag_sc_ann + drag_tax_ann,
    .groups = "drop"
  )

summary_final <- summary_stats |>
  left_join(sr_from_monthly, by = "threshold") |>
  arrange(threshold)

cat("\n  Results Summary:\n")
cat("  ", paste(rep("-", 80), collapse = ""), "\n")
cat(sprintf(
  "  %-12s  %-10s  %-10s  %-10s  %-10s  %-10s\n",
  "Threshold",
  "Avg TO",
  "TC drag pa",
  "Net SR",
  "Net SR lag",
  "vs Base"
))
base_sr <- summary_final$net_sr_lag[summary_final$threshold == 0]
for (i in seq_len(nrow(summary_final))) {
  r <- summary_final[i, ]
  diff_sr <- if (r$threshold == 0) {
    "---"
  } else {
    sprintf("%+.3f", r$net_sr_lag - base_sr)
  }
  cat(sprintf(
    "  %-12s  %-10.3f  %-10.4f  %-10.3f  %-10.3f  %-10s\n",
    paste0(r$threshold, " bps"),
    r$avg_turnover,
    r$drag_tc_ann,
    r$net_sr,
    r$net_sr_lag,
    diff_sr
  ))
}

# ---------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------
out_csv <- file.path(OUT_DIR, "drift_band_results_CS_LO_25_12M.csv")

export_df <- summary_final |>
  mutate(
    threshold_label = paste0(threshold, " bps"),
    avg_turnover_pct = avg_turnover * 100,
    drag_tc_ann_pct = drag_tc_ann * 100,
    drag_total_ann_pct = drag_total_ann * 100,
    gross_ret_pct = gross_ret_ann * 100,
    net_ret_lag_pct = net_ret_lag_ann * 100,
    vs_baseline = net_sr_lag - base_sr
  ) |>
  select(
    `Threshold (bps)` = threshold,
    `Avg Monthly Turnover` = avg_turnover_pct,
    `TC Drag (pa)` = drag_tc_ann_pct,
    `Total Drag (pa)` = drag_total_ann_pct,
    `Gross SR` = gross_sr,
    `Net SR` = net_sr,
    `Net SR (lag-adj)` = net_sr_lag,
    `vs Baseline` = vs_baseline,
    `Net Ret pa (lag-adj)` = net_ret_lag_pct
  )

write_csv(export_df, out_csv)
cat(sprintf("\n  Saved: %s\n", out_csv))

# ---------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------
WU_NAVY <- "#1a2e5a"
WU_GOLD <- "#c8a951"
WU_BLUE <- "#1e50a2"

fig1 <- ggplot(export_df, aes(`Avg Monthly Turnover`, `Net SR (lag-adj)`)) +
  geom_line(colour = WU_NAVY, linewidth = 1.2) +
  geom_point(
    aes(size = `Threshold (bps)`),
    colour = WU_NAVY,
    fill = WU_BLUE,
    shape = 21
  ) +
  geom_point(
    data = export_df |> slice_max(`Net SR (lag-adj)`, n = 1),
    colour = WU_GOLD,
    size = 5,
    shape = 18
  ) +
  geom_label(
    aes(label = paste0(`Threshold (bps)`, "bps")),
    vjust = -0.8,
    size = 2.8,
    colour = WU_NAVY,
    label.size = 0.2,
    fill = "white"
  ) +
  scale_x_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  scale_size_continuous(guide = "none") +
  labs(
    title = "Drift-Band Rebalancing: Net SR (lag-adj) vs. Monthly Turnover",
    subtitle = "CS_LO_25 (12M) | 743 months (1963–2024) | Gold diamond = optimal threshold",
    x = "Average Monthly Turnover (%)",
    y = "Net Sharpe Ratio (lag-adjusted)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", colour = WU_NAVY),
    plot.subtitle = element_text(colour = "gray40", size = 9),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(FIG_DIR, "DriftBand_SR_vs_Turnover.pdf"),
  fig1,
  width = 8,
  height = 5
)
ggsave(
  file.path(FIG_DIR, "DriftBand_SR_vs_Turnover.png"),
  fig1,
  width = 8,
  height = 5,
  dpi = 150
)
cat(sprintf(
  "  Saved: %s\n",
  file.path(FIG_DIR, "DriftBand_SR_vs_Turnover.pdf")
))

fig2 <- export_df |>
  select(`Threshold (bps)`, `TC Drag (pa)`, `Total Drag (pa)`) |>
  pivot_longer(-`Threshold (bps)`, names_to = "Drag", values_to = "pct") |>
  ggplot(aes(`Threshold (bps)`, pct, colour = Drag, group = Drag)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c(`TC Drag (pa)` = WU_BLUE, `Total Drag (pa)` = WU_NAVY)
  ) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 0.01)) +
  labs(
    title = "Friction Drag vs. Drift-Band Threshold",
    subtitle = "CS_LO_25 (12M) | SC (0.22%) and Tax (0.29%) are threshold-independent",
    x = "Threshold (bps)",
    y = "Annual Drag (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", colour = WU_NAVY),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  file.path(FIG_DIR, "DriftBand_TC_vs_Threshold.pdf"),
  fig2,
  width = 8,
  height = 4
)
cat(sprintf(
  "  Saved: %s\n",
  file.path(FIG_DIR, "DriftBand_TC_vs_Threshold.pdf")
))

cat("\n======================================================\n")
cat("  DRIFT-BAND ANALYSIS COMPLETE\n")
cat("======================================================\n")
cat(sprintf(
  "  Optimal threshold: %d bps (highest net SR lag-adj = %.3f)\n",
  export_df$`Threshold (bps)`[which.max(export_df$`Net SR (lag-adj)`)],
  max(export_df$`Net SR (lag-adj)`)
))
cat(sprintf("  Baseline (0 bps):  net SR lag-adj = %.3f\n", base_sr))
cat(sprintf(
  "  SR improvement at optimum: %+.3f\n",
  max(export_df$`Net SR (lag-adj)`) - base_sr
))
cat(sprintf(
  "  TC drag reduction at optimum: %.2f%%pa -> %.2f%%pa\n",
  export_df$`TC Drag (pa)`[export_df$`Threshold (bps)` == 0],
  export_df$`TC Drag (pa)`[which.max(export_df$`Net SR (lag-adj)`)]
))

# -----------------------------------------------------------------------------
# Plot drift-band results from CSV
# -----------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)

# Adjust path if needed
csv_path <- "01_Data/Processed/Phase2/drift_band_results_CS_LO_25_12M.csv"
df_plot <- readr::read_csv(csv_path)

# Ensure thresholds are numeric and sorted
df_plot <- df_plot %>%
  mutate(Threshold = as.numeric(`Threshold (bps)`)) %>%
  arrange(Threshold)

# 1. Net SR (lag‑adj) vs. Threshold
p1 <- ggplot(df_plot, aes(x = Threshold, y = `Net SR (lag-adj)`)) +
  geom_line(color = "#1a2e5a", linewidth = 1.2) +
  geom_point(size = 3, color = "#1e50a2") +
  geom_point(
    data = filter(df_plot, `Net SR (lag-adj)` == max(`Net SR (lag-adj)`)),
    color = "#c8a951",
    size = 5,
    shape = 18
  ) +
  geom_label(
    aes(label = paste0(Threshold, " bps")),
    vjust = -0.8,
    size = 3,
    color = "#1a2e5a",
    label.size = 0.2,
    fill = "white"
  ) +
  scale_x_continuous(breaks = df_plot$Threshold) +
  labs(
    x = "Drift‑band threshold (bps)",
    y = "Net Sharpe ratio (lag‑adjusted)",
    title = "Optimal rebalancing threshold: 0 bps",
    subtitle = "CS_LO_25, 12M lookback | 1963–2024"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#1a2e5a"))

print(p1)
ggsave("drift_netSR_vs_threshold.png", p1, width = 7, height = 4, dpi = 150)

# 2. Turnover and TC drag together (dual axis)
p2 <- df_plot %>%
  select(Threshold, Turnover = `Avg Monthly Turnover`, TC = `TC Drag (pa)`) %>%
  pivot_longer(
    cols = c(Turnover, TC),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = ifelse(
      Metric == "Turnover",
      "Monthly turnover (%)",
      "TC drag (% p.a.)"
    )
  ) %>%
  ggplot(aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = df_plot$Threshold) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "TC drag (% p.a.)")) + # dummy, actually both on same scale
  scale_color_manual(
    values = c(
      "Monthly turnover (%)" = "#1e50a2",
      "TC drag (% p.a.)" = "#c8a951"
    )
  ) +
  labs(
    x = "Drift‑band threshold (bps)",
    y = "Value",
    title = "Turnover and transaction cost decline with threshold",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p2)
ggsave("drift_turnover_TC.png", p2, width = 7, height = 4, dpi = 150)

# 3. Bar chart of net SR (lag‑adj) vs threshold
p3 <- ggplot(df_plot, aes(x = factor(Threshold), y = `Net SR (lag-adj)`)) +
  geom_col(fill = "#1a2e5a", alpha = 0.8) +
  geom_text(
    aes(label = round(`Net SR (lag-adj)`, 3)),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    x = "Threshold (bps)",
    y = "Net Sharpe ratio (lag‑adjusted)",
    title = "Net SR peaks at full monthly rebalancing"
  ) +
  theme_minimal(base_size = 12)

print(p3)
ggsave("drift_netSR_bar.png", p3, width = 6, height = 4, dpi = 150)
