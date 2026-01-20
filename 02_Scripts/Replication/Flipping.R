# ==============================================================================
# JKP DATA PIPELINE: CORE FACTOR GENERATION (FINAL)
# Purpose: Construct L/S, Correct Signs, Filter for Core 35, and Save
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, arrow, lubridate, gridExtra, grid)

# --- CONFIGURATION ---
setwd(
  "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
)

# Inputs
BENCH_FILE <- "01_Data/Raw/[usa]_[all_factors]_[monthly]_[vw_cap].csv"
REPL_FILE <- "01_Data/Raw/pfs.parquet"

# Output
OUTPUT_FILE <- "01_Data/Processed/USA_Valid_Factor_Returns.parquet"

# --- 0. DEFINE CORE FACTOR UNIVERSE ---
factor_rename_map <- c(
  "Size_SMB" = "market_equity",
  "Book_to_Market_HML" = "be_me",
  "Operating_Profitability_RMW" = "ope_be",
  "Asset_Growth_CMA" = "at_gr1",
  "Long_Term_Reversals_LTREV" = "ret_60_12",
  "Residual_Variance_RVAR" = "ivol_ff3_21d",
  "Quality_Minus_Junk_QMJ" = "qmj",
  "Low_Beta_BAB" = "betabab_1260d",
  "Amihud_Illiquidity" = "ami_126d",
  "Firm_Age" = "age",
  "Nominal_Price" = "prc",
  "High_Volume_Premium" = "dolvol_126d",
  "Gross_Profitability" = "gp_at",
  "Return_on_Equity" = "ni_be",
  "Return_on_Assets" = "niq_at",
  "Profit_Margin" = "ebit_sale",
  "Change_in_Asset_Turnover" = "at_turnover",
  "Accruals_Factor" = "oaccruals_at",
  "Net_Operating_Assets" = "noa_at",
  "Net_Working_Capital_Changes" = "cowc_gr1a",
  "Cash_Flow_to_Price" = "ocf_me",
  "Earnings_to_Price" = "ni_me",
  "Enterprise_Multiple" = "ebitda_mev",
  "Sales_to_Price" = "sale_me",
  "Growth_in_Inventory" = "inv_gr1",
  "Sales_Growth" = "sale_gr1",
  "Growth_in_Sales_Inventory" = "dsale_dinv",
  "Abnormal_Investment" = "capex_abn",
  "CAPX_Growth_Rate" = "capx_gr1",
  "Debt_Issuance_Factor" = "dbnetis_at",
  "Leverage_Factor" = "at_be",
  "One_Year_Share_Issuance" = "chcsho_12m",
  "Total_External_Financing" = "netis_at",
  "Ohlson_O_Score" = "o_score",
  "Altman_Z_Score" = "z_score",
  "Piotroski_F_Score" = "f_score"
)

# ==============================================================================
# 1. LOAD DATA & CONSTRUCT INITIAL PORTFOLIOS
# ==============================================================================
cat("--- 1. Loading and Constructing Portfolios ---\n")

# Load Benchmark (Needed for sign correction)
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
    # Dynamic buckets (works for 3 or 5 buckets)
    is_long = (pf == max(pf, na.rm = TRUE)),
    is_short = (pf == min(pf, na.rm = TRUE))
  ) %>%
  filter(is_long | is_short) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(date, characteristic),
    names_from = is_long,
    values_from = ret,
    values_fn = mean
  ) %>%
  rename(ret_short = `FALSE`, ret_long = `TRUE`) %>%
  mutate(
    raw_ret = ret_long - ret_short, # Initial calculation (Always High - Low)
    date = as.Date(date)
  ) %>%
  na.omit()

# ==============================================================================
# 2. DIAGNOSTIC PHASE: IDENTIFY SIGN FLIPS
# ==============================================================================
cat("--- 2. Diagnosing Factor Directions (Correlation Check) ---\n")

# Merge with Benchmark to check correlations
diagnostic <- inner_join(repl_ls, bench, by = c("date", "characteristic"))

diag_stats <- diagnostic %>%
  group_by(characteristic) %>%
  summarise(corr_raw = cor(raw_ret, bench_ret, use = "complete.obs")) %>%
  mutate(
    # If correlation is negative, it means JKP uses "Low-High" but we calculated "High-Low"
    action = case_when(
      corr_raw < -0.50 ~ "Flip", # Threshold for flipping
      TRUE ~ "Keep"
    )
  )

# Extract list of factors to flip
factors_to_flip <- diag_stats %>%
  filter(action == "Flip") %>%
  pull(characteristic)

cat("Found", length(factors_to_flip), "factors that need sign flipping.\n")

# ==============================================================================
# 3. CORRECTION & FILTERING PHASE
# ==============================================================================
cat("--- 3. Applying Corrections and Filtering for CORE 35 ---\n")

# Create a mapping dataframe for the 35 factors
core_map_df <- tibble(
  characteristic = unname(factor_rename_map),
  Readable_Name = names(factor_rename_map)
)

final_data <- repl_ls %>%
  # 1. Apply the Sign Flip
  mutate(
    final_ret = if_else(
      characteristic %in% factors_to_flip,
      -1 * raw_ret,
      raw_ret
    ),
    direction = if_else(
      characteristic %in% factors_to_flip,
      "Low Minus High",
      "High Minus Low"
    )
  ) %>%
  # 2. FILTER: Keep ONLY the 35 Core Factors
  inner_join(core_map_df, by = "characteristic") %>%
  select(date, characteristic, Readable_Name, direction, final_ret) %>%
  arrange(characteristic, date)

# ==============================================================================
# 4. FINAL VALIDATION & SAVE
# ==============================================================================
cat("--- 4. Validation ---\n")
cat("Total Factors Retained:", length(unique(final_data$characteristic)), "\n")
cat("Expected: 35\n")

# Check if we missed any
missing_factors <- setdiff(
  unname(factor_rename_map),
  unique(final_data$characteristic)
)
if (length(missing_factors) > 0) {
  cat(
    "⚠️ WARNING: The following requested factors were NOT found in the data:\n"
  )
  print(missing_factors)
} else {
  cat("✅ All 35 Core Factors found and processed.\n")
}

# Save
write_parquet(final_data, OUTPUT_FILE)
cat("\nSUCCESS: Cleaned Core Factor Data saved to", OUTPUT_FILE, "\n")
