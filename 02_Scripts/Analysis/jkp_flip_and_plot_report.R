# ==============================================================================
# JKP DATA PIPELINE: AUDIT & CLEANING (FINAL)
# Purpose: Construct L/S, Correct Signs, Verify, and Save for Analysis
# Author: QFin Factor Momentum Lead
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, ggplot2, lubridate, gridExtra, grid)

# CONFIGURATION
# ------------------------------------------------------------------------------
# Update this path if necessary
setwd("/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code")

BENCH_FILE <- "01_Data/Raw/[usa]_[all_factors]_[monthly]_[vw_cap].csv"
REPL_FILE  <- "01_Data/Raw/pfs.parquet"
OUTPUT_FILE <- "01_Data/Processed/USA_Valid_Factor_Returns.parquet"
PDF_REPORT  <- "03_Outputs/Reports/Final_Audit_Report.pdf"

# ==============================================================================
# 1. LOAD DATA & CONSTRUCT INITIAL PORTFOLIOS
# ==============================================================================
cat("--- 1. Loading and Constructing Portfolios ---\n")

# Load Benchmark
bench <- read_csv(BENCH_FILE, show_col_types = FALSE) %>%
  filter(freq == "monthly", weighting == "vw_cap") %>%
  transmute(date = as.Date(date), characteristic = name, bench_ret = ret)

# Load Replication Data
repl_raw <- read_parquet(REPL_FILE)

# Construct Long-Short (Default: High - Low)
repl_ls <- repl_raw %>%
  filter(excntry == "USA") %>%
  select(date = eom, characteristic, pf, ret = ret_vw_cap) %>%
  group_by(date, characteristic) %>%
  mutate(
    is_long  = (pf == max(pf)),
    is_short = (pf == min(pf))
  ) %>%
  filter(is_long | is_short) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(date, characteristic), 
              names_from = is_long, 
              values_from = ret, 
              values_fn = mean) %>%
  rename(ret_short = `FALSE`, ret_long = `TRUE`) %>%
  mutate(
    raw_ret = ret_long - ret_short, # Initial calculation
    date = as.Date(date)
  ) %>%
  na.omit()

# ==============================================================================
# 2. DIAGNOSTIC PHASE: IDENTIFY SIGN FLIPS
# ==============================================================================
cat("--- 2. Diagnosing Factor Directions ---\n")

# Merge with Benchmark to check correlations
diagnostic <- inner_join(repl_ls, bench, by = c("date", "characteristic"))

diag_stats <- diagnostic %>%
  group_by(characteristic) %>%
  summarise(corr_raw = cor(raw_ret, bench_ret)) %>%
  mutate(
    action = case_when(
      corr_raw > 0.90  ~ "Keep",
      corr_raw < -0.90 ~ "Flip",
      TRUE ~ "Check"
    )
  )

# Extract list of factors to flip
factors_to_flip <- diag_stats %>% filter(action == "Flip") %>% pull(characteristic)

cat("Found", length(factors_to_flip), "factors that need sign flipping (Low-High).\n")
cat("Found", sum(diag_stats$action == "Keep"), "factors correct (High-Low).\n")

# ==============================================================================
# 3. CORRECTION PHASE: APPLY SIGN FLIPS
# ==============================================================================
cat("--- 3. Applying Corrections ---\n")

clean_data <- repl_ls %>%
  mutate(
    # Apply the flip
    final_ret = if_else(characteristic %in% factors_to_flip, -1 * raw_ret, raw_ret),
    # Tag the direction for metadata
    direction = if_else(characteristic %in% factors_to_flip, "Low Minus High", "High Minus Low")
  ) %>%
  select(date, characteristic, direction, final_ret)

# ==============================================================================
# 4. FINAL VALIDATION (RE-CHECK)
# ==============================================================================
cat("--- 4. Final Validation ---\n")

validation <- inner_join(clean_data, bench, by = c("date", "characteristic"))

final_stats <- validation %>%
  group_by(characteristic, direction) %>%
  summarise(
    final_corr = cor(final_ret, bench_ret),
    mae = mean(abs(final_ret - bench_ret)),
    .groups = 'drop'
  ) %>%
  arrange(final_corr)

cat("Minimum Final Correlation:", min(final_stats$final_corr), "\n")
cat("Median Final Correlation: ", median(final_stats$final_corr), "\n")

# ==============================================================================
# 5. GENERATE PDF REPORT
# ==============================================================================
cat("--- 5. Generating Report: ", PDF_REPORT, " ---\n")

pdf(PDF_REPORT, width = 10, height = 7)

# --- PAGE 1: Executive Summary ---
grid.newpage()
title_text <- paste0("Jenson/Kelly/Pedersen Replication Audit\n",
                     "Date: ", Sys.Date(), "\n",
                     "Total Factors: ", nrow(final_stats), "\n",
                     "Median Correlation: ", round(median(final_stats$final_corr), 4))
grid.text(title_text, x = 0.5, y = 0.8, gp = gpar(fontsize = 18, fontface = "bold"))

# Summary Table of Directions
dir_summary <- final_stats %>% count(direction)
table_grob <- tableGrob(dir_summary, rows = NULL)
grid.draw(table_grob)

# --- PAGE 2: List of Flipped Factors ---
if(length(factors_to_flip) > 0) {
  grid.newpage()
  grid.text("Factors Corrected (Sign Flipped)", x=0.5, y=0.9, gp=gpar(fontsize=16))
  
  # Split into chunks if too many for one page
  flipped_df <- data.frame(Flipped_Factors = factors_to_flip)
  max_rows <- 25
  pages <- ceiling(nrow(flipped_df) / max_rows)
  
  for(i in 1:pages) {
    if(i > 1) grid.newpage()
    start_row <- (i-1) * max_rows + 1
    end_row <- min(i * max_rows, nrow(flipped_df))
    sub_df <- flipped_df[start_row:end_row, , drop=FALSE]
    grid.table(sub_df, rows=NULL)
  }
}

# --- PAGE 3+: Cumulative Return Plots ---
# We iterate through factors (sorted by name)
params <- unique(final_stats$characteristic)

for(f in params) {
  # Prepare data
  plot_dat <- validation %>%
    filter(characteristic == f, date >= "1980-01-01") %>%
    arrange(date) %>%
    mutate(
      cum_bench = cumprod(1 + bench_ret) - 1,
      cum_rep   = cumprod(1 + final_ret) - 1
    )
  
  # Stats for Title
  f_stat <- final_stats %>% filter(characteristic == f)
  
  p <- ggplot(plot_dat, aes(x = date)) +
    geom_line(aes(y = cum_bench, color = "Benchmark (JKP)"), size = 0.8, alpha = 0.6) +
    geom_line(aes(y = cum_rep, color = "Replicated (Corrected)"), size = 0.6, linetype = "dashed") +
    scale_color_manual(values = c("Benchmark (JKP)" = "black", "Replicated (Corrected)" = "blue")) +
    labs(
      title = paste0("Factor: ", f),
      subtitle = paste0("Correlation: ", round(f_stat$final_corr, 4), 
                        " | Direction: ", f_stat$direction),
      y = "Cumulative Return", x = "Date"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

dev.off()

# ==============================================================================
# 6. SAVE OUTPUT
# ==============================================================================
write_parquet(clean_data, OUTPUT_FILE)

cat("\nSUCCESS: Data saved to", OUTPUT_FILE, "\n")
cat("Report saved to", PDF_REPORT, "\n")
cat("Ready for Phase 1: Cluster Analysis.\n")