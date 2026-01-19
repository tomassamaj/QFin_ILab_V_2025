# ==============================================================================
# JKP REPLICATION: CORRECTED AUDIT (Long-Short Construction)
# Purpose: Manually construct L/S returns from buckets and compare to benchmark
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, ggplot2, lubridate, gridExtra)

# CONFIG
setwd("/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code")
BENCH_FILE <- "[usa]_[all_factors]_[monthly]_[vw_cap].csv"
REPL_FILE  <- "pfs.parquet"

# 1. LOAD & CLEAN REPLICATION DATA
cat("--- Loading Replication Data ---\n")
repl_raw <- read_parquet(REPL_FILE)

# Construct Long-Short Returns
cat("--- Constructing Long-Short Factors ---\n")
repl_ls <- repl_raw %>%
  # 1. Filter for USA using the correct column 'excntry'
  filter(excntry == "USA") %>%
  
  # 2. Select relevant columns
  select(date = eom, characteristic, pf, ret = ret_vw_cap) %>%
  
  # 3. Pivot: We need 'pf' to become columns (e.g., pf_1, pf_3, pf_5)
  # Assumption: High Portfolio (Long) is the max 'pf', Low (Short) is min 'pf'
  group_by(date, characteristic) %>%
  mutate(
    is_long = (pf == max(pf)),
    is_short = (pf == min(pf))
  ) %>%
  ungroup() %>%
  
  # Filter only the top and bottom buckets
  filter(is_long | is_short) %>%
  select(date, characteristic, is_long, ret) %>%
  
  # Pivot to wide format
  pivot_wider(names_from = is_long, values_from = ret, values_fn = mean) %>%
  rename(ret_short = `FALSE`, ret_long = `TRUE`) %>%
  
  # 4. Calculate Long - Short Return
  mutate(my_ret = ret_long - ret_short) %>%
  mutate(date = as.Date(date))

cat("Replication Rows (USA L/S):", format(nrow(repl_ls), big.mark=","), "\n")

# 2. LOAD BENCHMARK
cat("--- Loading Benchmark ---\n")
bench <- read_csv(BENCH_FILE, show_col_types = FALSE) %>%
  filter(freq == "monthly", weighting == "vw_cap") %>%
  transmute(
    date = as.Date(date),
    characteristic = name,
    bench_ret = ret
  )

# 3. MERGE & COMPARE
cat("--- Merging ---\n")
audit <- inner_join(repl_ls, bench, by = c("date", "characteristic"))

cat("Matched Observations:", format(nrow(audit), big.mark=","), "\n")

# Stats
factor_stats <- audit %>%
  group_by(characteristic) %>%
  summarise(
    correlation = cor(my_ret, bench_ret, use = "complete.obs"),
    mae = mean(abs(my_ret - bench_ret), na.rm = TRUE)
  ) %>%
  arrange(correlation)

cat("\nMedian Correlation:", median(factor_stats$correlation, na.rm=T), "\n")
cat("Factors < 0.90:", sum(factor_stats$correlation < 0.90, na.rm=T), "\n")

# 4. PLOT PDF
cat("--- Generating PDF Report ---\n")
pdf("Corrected_Replication_Report.pdf", width = 10, height = 6)

for(f in factor_stats$characteristic) {
  plot_data <- audit %>% 
    filter(characteristic == f, date >= "1980-01-01") %>%
    mutate(
      cum_bench = cumprod(1 + bench_ret) - 1,
      cum_my    = cumprod(1 + my_ret) - 1
    )
  
  curr_corr <- round(factor_stats$correlation[factor_stats$characteristic == f], 4)
  
  # Cumulative Plot
  p <- ggplot(plot_data, aes(x = date)) +
    geom_line(aes(y = cum_bench, color = "Benchmark"), linewidth = 0.8) +
    geom_line(aes(y = cum_my, color = "Replication"), linewidth = 0.6, linetype = "dashed") +
    scale_color_manual(values = c("Benchmark" = "black", "Replication" = "red")) +
    labs(title = paste0("Factor: ", f), subtitle = paste0("Corr: ", curr_corr), y = "Cumulative Return") +
    theme_minimal()
  
  print(p)
}
dev.off()
cat("Done!\n")