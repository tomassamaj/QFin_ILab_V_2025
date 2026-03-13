# ==============================================================================
# DAILY DATA → MONTHLY FACTOR RETURNS
# ==============================================================================
# Verify that daily portfolio sorts correctly replicate monthly benchmarks
# Flow: Daily PFS → Monthly Aggregation → Compare with Official Monthly Index
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, data.table, lubridate, ggplot2, gridExtra, grid, scales)

# --- CONFIGURATION ---
BASE_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
DATA_DIR <- file.path(BASE_DIR, "01_Data/Clean_Daily_Inputs")

# Inputs
DAILY_PFS_FILE <- file.path(DATA_DIR, "pfs_daily.parquet")
BENCH_FILE <- file.path(BASE_DIR, "01_Data/Raw/[usa]_[all_factors]_[monthly]_[vw_cap].csv")

# Output
AUDIT_REPORT <- file.path(BASE_DIR, "03_Outputs/Reports/Daily_to_Monthly_Audit.pdf")

# ==============================================================================
# STEP 1: LOAD DAILY PORTFOLIO SORTS
# ==============================================================================

cat("=== Loading Daily Portfolio Sorts ===\n")

pfs_daily <- read_parquet(DAILY_PFS_FILE) %>%
  filter(excntry == "USA") %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  collect()
setDT(pfs_daily)

cat("Loaded", nrow(pfs_daily), "daily observations\n")
cat("Factors:", uniqueN(pfs_daily$characteristic), "\n")
cat("Date range:", as.character(min(pfs_daily$date)), "to", as.character(max(pfs_daily$date)), "\n")

# ==============================================================================
# STEP 2: CONSTRUCT DAILY LONG-SHORT RETURNS
# ==============================================================================

cat("\n=== Constructing Daily Factor Returns ===\n")

daily_factors <- pfs_daily[, .(
  ret_long = ret[pf == max(pf)],
  ret_short = ret[pf == min(pf)]
), by = .(date, characteristic)]

# Calculate raw factor return (High - Low)
daily_factors[, factor_ret := ret_long - ret_short]

# Add month identifier
daily_factors[, month_date := floor_date(date, "month")]

# ==============================================================================
# STEP 3: AGGREGATE TO MONTHLY (Compound Daily Returns)
# ==============================================================================

cat("\n=== Aggregating to Monthly Frequency ===\n")

# Compound daily returns within each month to get monthly return
monthly_factors <- daily_factors[, .(
  monthly_ret = exp(sum(log(1 + factor_ret))) - 1,
  n_days = .N,
  first_day = min(date),
  last_day = max(date)
), by = .(month_date, characteristic)]

# Rename for consistency
setnames(monthly_factors, "month_date", "month")

cat("Monthly factor returns calculated\n")
cat("Months covered:", uniqueN(monthly_factors$month), "\n")

# ==============================================================================
# STEP 4: LOAD OFFICIAL BENCHMARK (MONTHLY)
# ==============================================================================

cat("\n=== Loading Official Benchmark ===\n")

bench <- fread(BENCH_FILE) %>%
  filter(freq == "monthly", weighting == "vw_cap") %>%
  select(date, characteristic = name, bench_ret = ret) %>%
  mutate(date = floor_date(as.Date(date), "month"))

cat("Benchmark loaded:", uniqueN(bench$characteristic), "factors\n")

# ==============================================================================
# STEP 5: COMPARE (WITHOUT Sign Corrections)
# ==============================================================================

cat("\n=== Initial Comparison (Before Sign Correction) ===\n")

comparison_raw <- merge(
  monthly_factors,
  bench,
  by.x = c("month", "characteristic"),
  by.y = c("date", "characteristic")
)

audit_stats_raw <- comparison_raw[, .(
  correlation = cor(monthly_ret, bench_ret, use = "complete.obs"),
  tracking_error = sd(monthly_ret - bench_ret, na.rm = TRUE),
  mean_diff = mean(monthly_ret - bench_ret, na.rm = TRUE),
  n_obs = .N
), by = characteristic]

cat("\n--- Factors Needing Sign Correction (Correlation < 0) ---\n")
factors_to_flip <- audit_stats_raw[correlation < 0, characteristic]
print(audit_stats_raw[correlation < 0][order(correlation)])

cat("\nTotal factors to flip:", length(factors_to_flip), "\n")

# ==============================================================================
# STEP 6: APPLY SIGN CORRECTIONS
# ==============================================================================

cat("\n=== Applying Sign Corrections ===\n")

monthly_factors[characteristic %in% factors_to_flip, monthly_ret := monthly_ret * -1]

# Re-compare
comparison_corrected <- merge(
  monthly_factors,
  bench,
  by.x = c("month", "characteristic"),
  by.y = c("date", "characteristic")
)

audit_stats_corrected <- comparison_corrected[, .(
  correlation = cor(monthly_ret, bench_ret, use = "complete.obs"),
  tracking_error = sd(monthly_ret - bench_ret, na.rm = TRUE),
  mean_diff = mean(monthly_ret - bench_ret, na.rm = TRUE),
  RMSE = sqrt(mean((monthly_ret - bench_ret)^2, na.rm = TRUE)),
  n_obs = .N
), by = characteristic]

setorder(audit_stats_corrected, -correlation)

cat("\n--- Corrected Audit Statistics (Top 10) ---\n")
print(head(audit_stats_corrected, 10))

cat("\n--- Summary Stats ---\n")
cat("Average Correlation:", round(mean(audit_stats_corrected$correlation, na.rm = TRUE), 5), "\n")
cat("Median Correlation:", round(median(audit_stats_corrected$correlation, na.rm = TRUE), 5), "\n")
cat("Min Correlation:", round(min(audit_stats_corrected$correlation, na.rm = TRUE), 5), "\n")

# ==============================================================================
# STEP 7: PDF REPORT
# ==============================================================================

cat("\n=== Generating PDF Report ===\n")

pdf(AUDIT_REPORT, width = 11, height = 7)

# --- PAGE 1: EXECUTIVE SUMMARY ---
grid.newpage()

grid.text(
  "Daily-to-Monthly Factor Replication Audit",
  x = 0.5, y = 0.9,
  gp = gpar(fontsize = 20, fontface = "bold")
)

grid.text(
  paste("Generated:", Sys.Date()),
  x = 0.5, y = 0.85,
  gp = gpar(fontsize = 12)
)

summary_text <- paste0(
  "Total Factors: ", nrow(audit_stats_corrected), "\n",
  "Average Correlation: ", round(mean(audit_stats_corrected$correlation, na.rm = TRUE), 5), "\n",
  "Minimum Correlation: ", round(min(audit_stats_corrected$correlation, na.rm = TRUE), 5), "\n",
  "Factors Flipped: ", length(factors_to_flip), "\n\n",
  "Method: Daily portfolio sorts compounded to monthly\n",
  "Status: ", ifelse(mean(audit_stats_corrected$correlation) > 0.95, "PASSED", "REVIEW")
)

grid.text(summary_text, x = 0.5, y = 0.6, gp = gpar(fontsize = 14))

# Correlation Distribution
p_corr <- ggplot(audit_stats_corrected, aes(x = correlation)) +
  geom_histogram(binwidth = 0.01, fill = "#27AE60", color = "white") +
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "red") +
  labs(title = "Correlation Distribution", x = "Correlation", y = "Count") +
  theme_minimal()

# Tracking Error Distribution  
p_te <- ggplot(audit_stats_corrected, aes(x = tracking_error)) +
  geom_histogram(bins = 30, fill = "#2980B9", color = "white") +
  labs(title = "Tracking Error Distribution", x = "Monthly Tracking Error", y = "Count") +
  theme_minimal()

pushViewport(viewport(layout = grid.layout(2, 2, heights = c(0.6, 0.4))))
print(p_corr, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_te, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# --- INDIVIDUAL FACTOR PLOTS ---
factors_to_plot <- unique(comparison_corrected$characteristic)

for (f in factors_to_plot) {
  
  plot_data <- comparison_corrected %>%
    filter(characteristic == f) %>%
    arrange(month) %>%
    mutate(
      Cum_Daily = cumprod(1 + monthly_ret),
      Cum_Bench = cumprod(1 + bench_ret)
    )
  
  stat <- audit_stats_corrected %>% filter(characteristic == f)
  
  p <- ggplot(plot_data, aes(x = month)) +
    geom_line(aes(y = Cum_Bench, color = "Official Benchmark (Monthly)"), 
             linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = Cum_Daily, color = "Daily → Monthly (Compounded)"), 
             linewidth = 1.0) +
    scale_y_log10(labels = comma) +
    scale_color_manual(values = c(
      "Daily → Monthly (Compounded)" = "#27AE60",
      "Official Benchmark (Monthly)" = "black"
    )) +
    labs(
      title = paste0("Factor: ", toupper(f)),
      subtitle = paste0(
        "Corr: ", round(stat$correlation, 5),
        " | TE: ", percent(stat$tracking_error, 0.01),
        " | RMSE: ", percent(stat$RMSE, 0.01)
      ),
      y = "Cumulative Wealth (Log)",
      x = "Month",
      color = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

dev.off()

cat("Audit report saved to:", AUDIT_REPORT, "\n")

# ==============================================================================
# STEP 8: SAVE CORRECTED MONTHLY FACTORS
# ==============================================================================

OUTPUT_FILE <- file.path(
  BASE_DIR,
  "01_Data/Processed/monthly_factors_from_daily.parquet"
)

write_parquet(monthly_factors, OUTPUT_FILE)

cat("\n Corrected monthly factor returns saved to:\n", OUTPUT_FILE, "\n")

# ==============================================================================
# STEP 9: CREATE SIGN CORRECTION REFERENCE
# ==============================================================================

sign_corrections <- data.table(
  characteristic = unique(monthly_factors$characteristic),
  flip_required = unique(monthly_factors$characteristic) %in% factors_to_flip,
  multiplier = ifelse(unique(monthly_factors$characteristic) %in% factors_to_flip, -1, 1)
)

write_csv(
  sign_corrections,
  file.path(BASE_DIR, "01_Data/Processed/factor_sign_corrections.csv")
)

cat("\n Sign correction reference saved\n")
cat("\n=== AUDIT COMPLETE ===\n")
