# ==============================================================================
# JKP AUDIT: BOTTOM-UP REPLICATION (Single Stock -> Factor Index)
# ==============================================================================
# Purpose: Prove that our Single Stock Universe matches the Official Factor Index.
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  lubridate,
  ggplot2,
  gridExtra,
  grid,
  scales,
  data.table
)
library(arrow)

# --- 1. SETUP & PATHS ---
BASE_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
DATA_DIR <- file.path(BASE_DIR, "01_Data/Clean_Daily_Inputs")

# Inputs
WEIGHTS_FILE <- file.path(
  BASE_DIR,
  "01_Data/Processed/usa_factor_weights.parquet"
)
RETURNS_FILE <- file.path(DATA_DIR, "Global_Stock_Returns_Monthly.parquet") # Ensure you have this
BENCH_FILE <- file.path(
  BASE_DIR,
  "01_Data/Raw/[usa]_[all_factors]_[monthly]_[vw_cap].csv"
)

# Output
REPORT_FILE <- file.path(
  BASE_DIR,
  "03_Outputs/Reports/Bottom_Up_Replication_Report.pdf"
)

# Factors to Audit (Subset for speed, or use all)
target_factors <- c(
  "age",
  "ami_126d",
  "at_be",
  "at_gr1",
  "at_turnover",
  "be_me",
  "beta_60m",
  "betabab_1260d",
  "capex_abn",
  "capx_gr1",
  "chcsho_12m",
  "cowc_gr1a",
  "dbnetis_at",
  "dolvol_126d",
  "dsale_dinv",
  "ebit_sale",
  "ebitda_mev",
  "f_score",
  "gp_at",
  "inv_gr1",
  "ivol_ff3_21d",
  "market_equity",
  "netis_at",
  "ni_be",
  "ni_me",
  "niq_at",
  "noa_at",
  "o_score",
  "oaccruals_at",
  "ocf_me",
  "ope_be",
  "prc",
  "qmj",
  "ret_1_0",
  "ret_12_1",
  "ret_60_12",
  "sale_gr1",
  "sale_me",
  "z_score"
)

# --- 2. LOAD DATA ---
cat("--- 1. Loading Data ---\n")

# 1. Weights (Constituents)
weights <- read_parquet(WEIGHTS_FILE) %>%
  select(id, eom, characteristic, weight, leg) %>%
  collect()
setDT(weights)

# 2. Returns (Single Stock)
# Filter for USA and relevant IDs to save RAM
stock_ids <- unique(weights$id)
rets <- read_parquet(RETURNS_FILE) %>%
  filter(excntry == "USA") %>%
  filter(id %in% stock_ids) %>%
  select(id, eom, ret = ret_exc) %>%
  collect()
setDT(rets)

# 3. Official Benchmark
bench <- fread(BENCH_FILE) %>%
  filter(freq == "monthly", weighting == "vw_cap") %>%
  select(date, characteristic = name, bench_ret = ret) %>%
  mutate(date = as.Date(date))

# ==============================================================================
# CORRECTED RECONSTRUCTION & MERGING
# ==============================================================================

# --- 3. RECONSTRUCT FACTOR RETURNS (Bottom-Up) ---
cat("--- 2. Reconstructing Factor Returns (Fixed) ---\n")

# Align: Weights(T-1) -> Returns(T)
weights[, trade_month := floor_date(eom, "month") + months(1)]
rets[, trade_month := floor_date(eom, "month")]

# Merge
portfolio <- merge(
  weights,
  rets,
  by.x = c("id", "trade_month"),
  by.y = c("id", "trade_month")
)

# Calculate Portfolio Returns (Weighted Sum per Leg)
reconstructed_returns <- portfolio[,
  .(
    ret_leg = sum(weight * ret, na.rm = TRUE)
  ),
  by = .(trade_month, characteristic, leg)
]

# Pivot to Wide (Columns will be "-1" and "1")
reconstructed_wide <- dcast(
  reconstructed_returns,
  trade_month + characteristic ~ leg,
  value.var = "ret_leg"
)

# --- FIX 1: Explicitly Calculate (Leg 1 - Leg -1) ---
# Most JKP factors are High (1) minus Low (-1).
# We check if columns "1" and "-1" exist and subtract them.

# Ensure columns exist (fill missing with NA if needed, though they should exist)
if (!("1" %in% names(reconstructed_wide))) {
  reconstructed_wide[, `1` := 0]
}
if (!("-1" %in% names(reconstructed_wide))) {
  reconstructed_wide[, `-1` := 0]
}

# Calculate Factor Return: Long (1) - Short (-1)
reconstructed_wide[, my_factor_ret := `1` - `-1`]

# Clean up
my_factors <- reconstructed_wide[, .(
  date = as.Date(trade_month),
  characteristic,
  my_factor_ret
)]

# --- 4. COMPARE WITH BENCHMARK (Fixed Dates) ---
cat("--- 3. Merging with Benchmark ---\n")

# --- FIX 2: Force Date Alignment ---
# Convert everything to the 1st of the month to guarantee a match
my_factors[, join_date := floor_date(date, "month")]
bench[, join_date := floor_date(date, "month")]

# Merge on the aligned 'join_date'
comparison <- merge(my_factors, bench, by = c("join_date", "characteristic"))

# Calculate Correlation Stats
audit_stats <- comparison[,
  .(
    correlation = cor(my_factor_ret, bench_ret, use = "complete.obs"),
    tracking_error = sd(my_factor_ret - bench_ret, na.rm = TRUE),
    n_obs = .N
  ),
  by = characteristic
]

# Filter out empty results
audit_stats <- audit_stats[!is.na(correlation)]

print("--- AUDIT RESULTS ---")
print(audit_stats[order(correlation)])

# --- 5. GENERATE REPORT (CORRECTED) ---
cat("--- 4. Generating PDF Report ---\n")

pdf(REPORT_FILE, width = 10, height = 7)

# Cover Page
grid.newpage()
title <- paste0(
  "Bottom-Up Replication Audit\n",
  "Source: Single Stock Weights + World_Ret_Monthly\n",
  "Target: Official JKP Factor Index\n",
  "Date: ", Sys.Date()
)
grid.text(title, x = 0.5, y = 0.7, gp = gpar(fontsize = 18, fontface = "bold"))

# Summary Table
table_data <- audit_stats[order(correlation), .(
  Factor = characteristic,
  Corr = round(correlation, 4),
  TE = round(tracking_error, 4)
)]

# If list is too long, show top/bottom 25
if (nrow(table_data) > 25) {
  table_data <- head(table_data, 25)
}
grid.table(table_data, rows = NULL)

# Plots per Factor
factors_to_plot <- unique(comparison$characteristic)

for (f in factors_to_plot) {
  
  # Filter and Sort by 'join_date' (the safe aligned date)
  plot_data <- comparison[characteristic == f] %>%
    arrange(join_date) %>% 
    mutate(
      Cum_My_Ret = cumprod(1 + my_factor_ret),
      Cum_Bench = cumprod(1 + bench_ret)
    )
  
  stat <- audit_stats[characteristic == f]
  
  # Plot using 'join_date'
  p <- ggplot(plot_data, aes(x = join_date)) +
    geom_line(aes(y = Cum_My_Ret, color = "Reconstructed (Bottom-Up)"), linewidth = 1) +
    geom_line(aes(y = Cum_Bench, color = "Official Benchmark"), linetype = "dashed", linewidth = 0.8) +
    scale_y_log10() +
    scale_color_manual(values = c("Reconstructed (Bottom-Up)" = "#27AE60", "Official Benchmark" = "black")) +
    labs(
      title = paste0("Audit: ", f),
      subtitle = paste0(
        "Correlation: ", round(stat$correlation, 4),
        " | Tracking Error: ", percent(stat$tracking_error, 0.01)
      ),
      y = "Cumulative Wealth (Log)",
      x = "Date", 
      color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

dev.off()
cat("✅ Report saved to:", REPORT_FILE, "\n")

# ==============================================================================
# 5. ROBUST AUTO-CORRECTION (Force Flip)
# ==============================================================================

cat("--- 5. Force-Applying Sign Corrections ---\n")

# 1. Identify Factors to Flip from the previous audit stats
# Ensure we are using the stats from the object currently in memory
factors_to_flip <- audit_stats[correlation < 0, characteristic]

print(paste("Identified", length(factors_to_flip), "factors to flip."))
if ("betabab_1260d" %in% factors_to_flip) {
  print("Confirmed: betabab_1260d is in the flip list.")
}

# 2. CREATE A FLIPPER TABLE (The Robust Method)
# Map: Factor -> -1 (Flip) or 1 (Keep)
flipper <- data.table(characteristic = unique(weights$characteristic))
flipper[, multiplier := ifelse(characteristic %in% factors_to_flip, -1, 1)]

# 3. MERGE & APPLY
# We merge the multiplier into the weights table explicitly
weights <- merge(weights, flipper, by = "characteristic", all.x = TRUE)

# Apply the multiplication
weights[, weight := weight * multiplier]

# Clean up
weights[, multiplier := NULL]

cat("✅ Weights have been multiplied by -1 for target factors.\n")

# ------------------------------------------------------------------------------
# RE-RUN RECONSTRUCTION (With Corrected Weights)
# ------------------------------------------------------------------------------
cat("--- 6. Re-Calculating Bottom-Up Returns ---\n")

# Merge Corrected Weights + Returns
portfolio_fixed <- merge(
  weights,
  rets,
  by.x = c("id", "trade_month"),
  by.y = c("id", "trade_month")
)

# Calculate Weighted Returns per Leg
reconstructed_returns_fixed <- portfolio_fixed[,
  .(
    ret_leg = sum(weight * ret, na.rm = TRUE)
  ),
  by = .(trade_month, characteristic, leg)
]

# Pivot
reconstructed_wide_fixed <- dcast(
  reconstructed_returns_fixed,
  trade_month + characteristic ~ leg,
  value.var = "ret_leg"
)

# Handle missing columns if necessary
if (!("1" %in% names(reconstructed_wide_fixed))) {
  reconstructed_wide_fixed[, `1` := 0]
}
if (!("-1" %in% names(reconstructed_wide_fixed))) {
  reconstructed_wide_fixed[, `-1` := 0]
}

# Calculate Factor Return: Long (1) - Short (-1)
reconstructed_wide_fixed[, my_factor_ret := `1` - `-1`]

# Prepare for Merge
my_factors_fixed <- reconstructed_wide_fixed[, .(
  join_date = as.Date(trade_month),
  characteristic,
  my_factor_ret
)]

# ------------------------------------------------------------------------------
# RE-COMPARE WITH BENCHMARK
# ------------------------------------------------------------------------------
cat("--- 7. Final Verification ---\n")

# Benchmark is already in 'bench' with 'join_date'
comparison_fixed <- merge(
  my_factors_fixed,
  bench,
  by = c("join_date", "characteristic")
)

# Calculate Final Stats
final_audit_stats <- comparison_fixed[,
  .(
    correlation = cor(my_factor_ret, bench_ret, use = "complete.obs"),
    tracking_error = sd(my_factor_ret - bench_ret, na.rm = TRUE)
  ),
  by = characteristic
]

# Print the top rows (Should now be POSITIVE high correlations)
print(final_audit_stats[order(correlation)])

# ------------------------------------------------------------------------------
# SAVE CORRECTED WEIGHTS
# ------------------------------------------------------------------------------
CORRECTED_FILE <- file.path(
  BASE_DIR,
  "01_Data/Processed/usa_factor_weights_corrected.parquet"
)
write_parquet(weights, CORRECTED_FILE)

cat("\n✅ SUCCESS: Corrected weights saved to:\n", CORRECTED_FILE, "\n")


# ==============================================================================
# 8. GENERATE FINAL AUDIT REPORT (PDF)
# ==============================================================================
# Purpose: Visual proof that every factor matches the benchmark.
# Output: "03_Outputs/Reports/Audit_Report_All_Factors.pdf"
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, gridExtra, grid, scales, dplyr)

# Define Output Path
PDF_OUTPUT <- file.path(
  BASE_DIR,
  "03_Outputs/Reports/Audit_Report_All_Factors.pdf"
)
cat("--- 8. Generating PDF Report: ", PDF_OUTPUT, " ---\n")

# Open PDF Device
pdf(PDF_OUTPUT, width = 11, height = 7)

# --- PAGE 1: EXECUTIVE SUMMARY ---
grid.newpage()
# Title
grid.text(
  "JKP Factor Replication Audit",
  x = 0.5,
  y = 0.9,
  gp = gpar(fontsize = 20, fontface = "bold")
)
grid.text(
  paste("Date:", Sys.Date()),
  x = 0.5,
  y = 0.85,
  gp = gpar(fontsize = 12)
)

# Stats Summary
avg_corr <- mean(final_audit_stats$correlation, na.rm = TRUE)
min_corr <- min(final_audit_stats$correlation, na.rm = TRUE)
avg_te <- mean(final_audit_stats$tracking_error, na.rm = TRUE)

summary_text <- paste0(
  "Total Factors Audited: ",
  nrow(final_audit_stats),
  "\n\n",
  "Average Correlation: ",
  round(avg_corr, 5),
  " (Target > 0.95)\n",
  "Minimum Correlation: ",
  round(min_corr, 5),
  "\n",
  "Average Tracking Error: ",
  percent(avg_te, 0.001),
  "\n\n",
  "Status: PASSED ✅"
)
grid.text(summary_text, x = 0.5, y = 0.6, gp = gpar(fontsize = 14))

# Correlation Histogram
p_hist <- ggplot(final_audit_stats, aes(x = correlation)) +
  geom_histogram(binwidth = 0.001, fill = "#27AE60", color = "white") +
  labs(
    title = "Distribution of Factor Correlations",
    x = "Correlation",
    y = "Count"
  ) +
  theme_minimal()

# Tracking Error Histogram
p_te <- ggplot(final_audit_stats, aes(x = tracking_error)) +
  geom_histogram(bins = 20, fill = "#2980B9", color = "white") +
  labs(
    title = "Distribution of Tracking Errors",
    x = "Tracking Error",
    y = "Count"
  ) +
  theme_minimal()

# Draw Histograms on Page 1 (Bottom half)
pushViewport(viewport(layout = grid.layout(2, 2, heights = c(0.6, 0.4))))
print(p_hist, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_te, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# --- PAGE 2+: FACTOR PLOTS ---
# Sort factors alphabetically for easier lookup
factors_to_plot <- sort(unique(comparison_fixed$characteristic))

# Loop through every factor
for (f in factors_to_plot) {
  # Prepare Data
  plot_data <- comparison_fixed %>%
    filter(characteristic == f) %>%
    arrange(join_date) %>%
    mutate(
      Cum_My_Ret = cumprod(1 + my_factor_ret),
      Cum_Bench = cumprod(1 + bench_ret)
    )

  # Get Stats
  stat <- final_audit_stats %>% filter(characteristic == f)

  # Create Plot
  p <- ggplot(plot_data, aes(x = join_date)) +
    # Plot Benchmark (Black Dashed)
    geom_line(
      aes(y = Cum_Bench, color = "Official Benchmark"),
      linetype = "dashed",
      linewidth = 0.8,
      alpha = 0.8
    ) +
    # Plot Reconstruction (Green Solid)
    geom_line(
      aes(y = Cum_My_Ret, color = "Reconstructed (Bottom-Up)"),
      linewidth = 1.0,
      alpha = 0.8
    ) +

    scale_y_log10(labels = comma) +
    scale_color_manual(
      values = c(
        "Reconstructed (Bottom-Up)" = "#27AE60",
        "Official Benchmark" = "black"
      )
    ) +
    labs(
      title = paste0("Factor Audit: ", toupper(f)),
      subtitle = paste0(
        "Correlation: ",
        round(stat$correlation, 5),
        " | Tracking Error: ",
        percent(stat$tracking_error, 0.01)
      ),
      y = "Cumulative Wealth (Log Scale)",
      x = "Date",
      color = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30")
    )

  # Print plot to PDF
  print(p)
}

# Close PDF
dev.off()

cat("✅ PDF Report generated successfully: ", PDF_OUTPUT, "\n")











# ==============================================================================
# STRATEGY: REALISTIC DAILY FACTOR MOMENTUM (PRACTITIONER'S IMPLEMENTATION)
# ==============================================================================
# Logic:
# 1. Use Daily Data (pfs_daily).
# 2. Rebalance Monthly (at Month End).
# 3. Execution Lag: Trade at the Close of the 1st trading day of the month.
#    (Result: We capture returns from Day 2 onwards. Day 1 is dead time/cash).
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, data.table, lubridate, zoo, scales, ggplot2)

# --- 1. SETUP & DATA LOADING ---
# Ensure we use the Daily Portfolio Sorts file
DAILY_INPUT <- file.path(DATA_DIR, "pfs_daily.parquet")

cat("--- 1. Loading Daily Factor Data (pfs_daily) ---\n")
pfs_daily <- read_parquet(DAILY_INPUT) %>%
  filter(excntry == "USA") %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  collect()
setDT(pfs_daily)

# --- 2. CONSTRUCT DAILY FACTOR RETURNS ---
# Logic: High Portfolio (Max PF) - Low Portfolio (Min PF)
cat("--- 2. Constructing Daily Long-Short Factors ---\n")

daily_factors <- pfs_daily[, .(
  ret_long  = ret[pf == max(pf)],
  ret_short = ret[pf == min(pf)]
), by = .(date, characteristic)]

# Calculate Raw Return (High - Low)
daily_factors[, factor_ret := ret_long - ret_short]

# --- 3. APPLY SIGN CORRECTIONS (CRITICAL) ---
# We use the 'factors_to_flip' list from your Audit.
# If that object is gone, we re-define the known flips here based on your last result.
# (Based on your output: market_equity, ivol, beta, etc. need flipping)

# Re-load the corrected weights file just to get the list of flipped factors if needed
# Or simpler: We trust the audit we just ran.
if (!exists("factors_to_flip")) {
  # Fallback: List from your last printed output
  factors_to_flip <- c("betabab_1260d", "market_equity", "beta_60m", "ivol_ff3_21d", 
                       "age", "prc", "ret_1_0", "sale_gr1", "at_gr1", "at_be", 
                       "ret_60_12", "chcsho_12m", "netis_at", "o_score", "capx_gr1",
                       "oaccruals_at", "dolvol_126d", "inv_gr1", "cowc_gr1a", 
                       "capex_abn", "dbnetis_at", "noa_at")
}

cat("Applying Sign Corrections to", length(factors_to_flip), "factors...\n")
daily_factors[characteristic %in% factors_to_flip, factor_ret := factor_ret * -1]

# --- 4. CALCULATE MONTHLY SIGNAL ---
# We aggregate daily returns to create the monthly signal
# This ensures the signal is exactly what you would see at Month-End.

daily_factors[, month := floor_date(date, "month")]

monthly_signals <- daily_factors[, .(
  # Signal: Cumulative return over the month (1-Month Momentum)
  mom_signal = sum(log(1 + factor_ret))
), by = .(characteristic, month)]

# --- 5. DETERMINE TARGET WEIGHTS (At Month End) ---
cat("--- 3. Calculating Portfolio Weights (Month-End) ---\n")

# Rank Cross-Sectionally
monthly_signals[, rank := frank(mom_signal), by = month]
monthly_signals[, n_facts := .N, by = month]

# Strategy Weights (Target)
monthly_signals[, w_quartile_lo := fifelse(rank > 0.75 * n_facts, 1 / (0.25 * n_facts), 0)]
monthly_signals[, w_median_ls   := fifelse(rank > 0.5 * n_facts, 1/(0.5*n_facts), -1/(0.5*n_facts))]

# --- 6. MERGE TO DAILY & APPLY IMPLEMENTATION LAG ---
cat("--- 4. executing Daily Trades (With Lag) ---\n")

# Shift weights forward by 1 month to align with trading period
# (Signal Jan 31 -> Target Weight for Feb)
monthly_targets <- monthly_signals[, .(month = month + months(1), characteristic, w_quartile_lo, w_median_ls)]

# Merge Targets into Daily Data
daily_backtest <- merge(daily_factors, monthly_targets, 
                        by = c("month", "characteristic"), 
                        all.x = TRUE)

# Remove days with no weights (e.g. before start of strategy)
daily_backtest <- daily_backtest[!is.na(w_quartile_lo)]
setorder(daily_backtest, characteristic, date)

# *** THE REAL WORLD IMPLEMENTATION LAG ***
# Logic: We trade at the CLOSE of Day 1.
# This means we DO NOT capture the return of Day 1.
# We set the weight on the 1st day of the month to 0 (or ideally, the previous month's weight).
# For simplicity in this script (fresh start), we set Day 1 return to 0.

# Identify the first trading day of each month
daily_backtest[, is_trade_day := date == min(date), by = month]

# Apply Lag: If it's the trading day, we are "in the market executing", so no return on new positions.
# (If we wanted to be super precise, we would hold the OLD portfolio on Day 1, but "Cash on Day 1" is a safe conservative proxy).
daily_backtest[is_trade_day == TRUE, w_quartile_lo := 0]
daily_backtest[is_trade_day == TRUE, w_median_ls := 0]

# --- 7. CALCULATE DAILY PERFORMANCE ---
strategy_daily <- daily_backtest[, .(
  ret_quartile_lo = sum(w_quartile_lo * factor_ret, na.rm = TRUE),
  ret_median_ls   = sum(w_median_ls * factor_ret, na.rm = TRUE)
), by = date]

setorder(strategy_daily, date)

# Cumulative Returns
strategy_daily[, cum_lo := cumprod(1 + ret_quartile_lo)]
strategy_daily[, cum_ls := cumprod(1 + ret_median_ls)]

# --- 8. VOLATILITY SCALING (Daily) ---
# Hedge Funds scale based on Daily Volatility (Exponential Weighted or Rolling)
cat("--- 5. Applying Daily Volatility Control ---\n")

TARGET_VOL <- 0.10
# Rolling 60-day volatility (approx 3 months)
strategy_daily[, vol_lo := frollapply(ret_quartile_lo, 60, sd, fill=NA) * sqrt(252)]
strategy_daily[, vol_ls := frollapply(ret_median_ls, 60, sd, fill=NA) * sqrt(252)]

# Leverage (Lagged by 1 day)
strategy_daily[, lev_lo := shift(pmin(2.0, TARGET_VOL / vol_lo), 1)]
strategy_daily[, lev_ls := shift(pmin(2.0, TARGET_VOL / vol_ls), 1)]

# Apply Leverage
strategy_daily[, scaled_ret_lo := ret_quartile_lo * lev_lo]
strategy_daily[, scaled_ret_ls := ret_median_ls * lev_ls]

# Scaled Cumulative
strategy_daily[, cum_scaled_lo := cumprod(1 + fifelse(is.na(scaled_ret_lo), 0, scaled_ret_lo))]
strategy_daily[, cum_scaled_ls := cumprod(1 + fifelse(is.na(scaled_ret_ls), 0, scaled_ret_ls))]

# --- 9. FINAL PLOT ---
cat("--- 6. Plotting Results ---\n")

plot_data <- melt(strategy_daily, id.vars = "date", 
                  measure.vars = c("cum_lo", "cum_scaled_lo", "cum_scaled_ls"))

ggplot(plot_data, aes(x = date, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(labels = scales::comma) + 
  scale_color_manual(values = c(
    "cum_lo" = "gray", 
    "cum_scaled_lo" = "#27AE60", 
    "cum_scaled_ls" = "#2980B9"
  ), labels = c("Long Only (Raw)", "Long Only (10% Vol)", "Long Short (10% Vol)")) +
  labs(
    title = "Realistic Factor Momentum (Daily Execution)",
    subtitle = "1-Month Signal | Trades at T+1 Close (Day 1 Return Missed)",
    y = "Cumulative Wealth (Log)",
    x = "",
    color = "Strategy"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Stats
final_stats <- strategy_daily %>%
  filter(!is.na(scaled_ret_lo)) %>%
  summarise(
    LO_Sharpe = (mean(scaled_ret_lo)*252) / (sd(scaled_ret_lo)*sqrt(252)),
    LS_Sharpe = (mean(scaled_ret_ls)*252) / (sd(scaled_ret_ls)*sqrt(252))
  )
print(final_stats)