# ==============================================================================
# JKP DATA PIPELINE: TARGETED REPLICATION AUDIT (FINAL)
# Purpose: Rigorous verification of 36 factors against JKP Benchmarks
# Author: QFin Factor Momentum Lead
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, ggplot2, lubridate, gridExtra, grid)

# CONFIGURATION
# ------------------------------------------------------------------------------
setwd("/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code")

BENCH_FILE  <- "01_Data/Raw/[usa]_[all_factors]_[monthly]_[vw_cap].csv"
REPL_FILE   <- "01_Data/Raw/pfs.parquet"
OUTPUT_FILE <- "01_Data/Processed/USA_Targeted_Factor_Returns.parquet"
PDF_REPORT  <- "03_Outputs/Reports/Targeted_Factor_Audit_Report.pdf"

# 36 Targeted Factors Mapping
factor_rename_map <- c(
  "Size_SMB" = "market_equity", "Book_to_Market_HML" = "be_me",
  "Operating_Profitability_RMW" = "ope_be", "Asset_Growth_CMA" = "at_gr1",
  "Long_Term_Reversals_LTREV" = "ret_60_12", "Residual_Variance_RVAR" = "ivol_ff3_21d",
  "Quality_Minus_Junk_QMJ" = "qmj", "Low_Beta_BAB" = "betabab_1260d",
  "Amihud_Illiquidity" = "ami_126d", "Firm_Age" = "age", "Nominal_Price" = "prc",
  "High_Volume_Premium" = "dolvol_126d", "Gross_Profitability" = "gp_at",
  "Return_on_Equity" = "ni_be", "Return_on_Assets" = "niq_at",
  "Profit_Margin" = "ebit_sale", "Change_in_Asset_Turnover" = "at_turnover",
  "Accruals_Factor" = "oaccruals_at", "Net_Operating_Assets" = "noa_at",
  "Net_Working_Capital_Changes" = "cowc_gr1a", "Cash_Flow_to_Price" = "ocf_me",
  "Earnings_to_Price" = "ni_me", "Enterprise_Multiple" = "ebitda_mev",
  "Sales_to_Price" = "sale_me", "Growth_in_Inventory" = "inv_gr1",
  "Sales_Growth" = "sale_gr1", "Growth_in_Sales_Inventory" = "dsale_dinv",
  "Abnormal_Investment" = "capex_abn", "CAPX_Growth_Rate" = "capx_gr1",
  "Debt_Issuance_Factor" = "dbnetis_at", "Leverage_Factor" = "at_be",
  "One_Year_Share_Issuance" = "chcsho_12m", "Total_External_Financing" = "netis_at",
  "Ohlson_O_Score" = "o_score", "Altman_Z_Score" = "z_score", "Piotroski_F_Score" = "f_score"
)
target_ids <- unname(factor_rename_map)

# ==============================================================================
# 1. DATA ALIGNMENT & CLEANING
# ==============================================================================
cat("--- 1. Aligning and Cleaning Data ---\n")

# Load Benchmark
bench <- read_csv(BENCH_FILE, show_col_types = FALSE) %>%
  filter(freq == "monthly", weighting == "vw_cap", name %in% target_ids) %>%
  mutate(characteristic_clean = names(factor_rename_map)[match(name, target_ids)]) %>%
  transmute(date = as.Date(date), characteristic = characteristic_clean, bench_ret = ret)

# Load Replication & Construct L/S
repl_raw <- read_parquet(REPL_FILE) %>%
  filter(excntry == "USA", characteristic %in% target_ids) %>%
  mutate(characteristic = names(factor_rename_map)[match(characteristic, target_ids)])

repl_ls <- repl_raw %>%
  group_by(eom, characteristic) %>%
  summarise(ret_ls = ret_vw_cap[pf == max(pf)] - ret_vw_cap[pf == min(pf)], .groups = 'drop') %>%
  rename(date = eom) %>%
  inner_join(bench, by = c("date", "characteristic"))

# Diagnose Sign Flips (Vectorized approach)
audit_stats <- repl_ls %>%
  group_by(characteristic) %>%
  summarise(
    raw_corr = cor(ret_ls, bench_ret, use = "complete.obs"),
    direction = if_else(raw_corr < 0, "Low Minus High", "High Minus Low"),
    .groups = 'drop'
  )

# Apply corrections
clean_data <- repl_ls %>%
  inner_join(audit_stats, by = "characteristic") %>%
  mutate(final_ret = ret_ls * sign(raw_corr)) %>%
  # Re-calculate final audit metrics
  group_by(characteristic) %>%
  mutate(final_corr = cor(final_ret, bench_ret)) %>%
  ungroup()

# ==============================================================================
# 2. VISUALIZATION ENGINE (2x3 GRIDS)
# ==============================================================================
cat("--- 2. Generating Audit Plots ---\n")

char_list <- unique(clean_data$characteristic)
all_plots <- list()

for(f in char_list) {
  p_dat <- clean_data %>% 
    filter(characteristic == f, date >= "1980-01-01") %>%
    arrange(date) %>%
    mutate(cum_bench = cumprod(1 + bench_ret) - 1,
           cum_rep   = cumprod(1 + final_ret) - 1)
  
  f_stat <- audit_stats %>% filter(characteristic == f)
  
  all_plots[[f]] <- ggplot(p_dat, aes(x = date)) +
    geom_line(aes(y = cum_bench, color = "Benchmark"), size = 0.7, alpha = 0.4) +
    geom_line(aes(y = cum_rep, color = "Replicated"), size = 0.5, linetype = "dashed") +
    scale_color_manual(values = c("Benchmark" = "black", "Replicated" = "blue")) +
    labs(title = f, 
         subtitle = paste0("Corr: ", round(abs(f_stat$raw_corr), 4), " (", f_stat$direction, ")"), 
         x = NULL, y = "Cum. Ret") +
    theme_minimal(base_size = 8) + 
    theme(legend.position = "none", plot.title = element_text(face="bold"))
}


# 1. Split the list of 36 plots into chunks of 6
plot_chunks <- split(all_plots, ceiling(seq_along(all_plots) / 6))

# 2. Loop through chunks and print to the plot pane
for(i in seq_along(plot_chunks)) {
  
  # Optional: Pause to see each grid (remove if you just want to scroll back)
  # cat("Showing Page", i, "of", length(plot_chunks), "- Press [Enter] for next")
  # readline() 
  
  do.call(grid.arrange, c(plot_chunks[[i]], ncol = 3, nrow = 2, 
                          top = paste("Audit Grids - Page", i)))
}


# ==============================================================================
# 3. EXPORT LOOP: PDF & RSTUDIO PLOT PANE
# ==============================================================================
cat("--- 3. Exporting to PDF and Plot Pane ---\n")

n_plots <- length(all_plots)
plots_per_page <- 6
n_pages <- ceiling(n_plots / plots_per_page)

pdf(PDF_REPORT, width = 11, height = 8.5)

# --- PDF PAGE 1: AUDIT SUMMARY TABLE ---
grid.newpage()
grid.text("Targeted Universe Audit Summary", y=0.9, gp=gpar(fontsize=16, fontface="bold"))
summary_table <- audit_stats %>% 
  transmute(Factor = characteristic, Direction = direction, `Abs Correlation` = round(abs(raw_corr), 4)) %>%
  arrange(`Abs Correlation`)
grid.draw(tableGrob(summary_table, rows=NULL, theme=ttheme_default(base_size = 7)))

# --- GRID LOOP ---
for(i in 1:n_pages) {
  start_idx <- (i-1)*6 + 1
  end_idx   <- min(i*6, n_plots)
  
  # Create the 2x3 Grid
  grid_page <- arrangeGrob(grobs = all_plots[start_idx:end_idx], 
                           ncol = 3, nrow = 2,
                           top = textGrob(paste("Data Integrity Audit - Page", i, "of", n_pages), 
                                          gp=gpar(fontsize=12, fontface="bold")))
  
  # 1. Output to PDF
  grid.newpage()
  grid.draw(grid_page)
  
  # 2. Output to RStudio Plots Pane (Looping)
  grid.arrange(grid_page)
}

dev.off()

# ==============================================================================
# 4. FINAL SAVE
# ==============================================================================
write_parquet(clean_data %>% select(date, characteristic, direction, final_ret), OUTPUT_FILE)
cat("\nAUDIT COMPLETE.\nReport saved to:", PDF_REPORT, "\nData saved to:", OUTPUT_FILE, "\n")