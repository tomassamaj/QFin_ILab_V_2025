# --- 1. Load All Required Libraries ---
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(frenchdata)

print("--- Starting Data Prep Script ---")

# --- 2. Define Parameters ---
start_date <- ymd("1963-07-01")
end_date <- ymd("2024-12-31")

# --- 3. Load LOCAL CSV Files ---
# !!! IMPORTANT: Update these paths to your local files !!!
factor_csv_path <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/[usa]_[all_factors]_[monthly]_[vw_cap].csv"
themes_csv_path <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/[usa]_[all_themes]_[monthly]_[vw_cap].csv"
industry_csv_path <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/17_Industry_Portfolios.csv"

all_factors_wide <- read.csv(factor_csv_path) %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

all_themes_wide <- read.csv(themes_csv_path) %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

ind_ports <- read.csv(industry_csv_path)
industry_names <- ind_ports[7:23, 1]  # Logic from your original script
industry_cols <- str_to_lower(industry_names)

print("Local CSVs loaded and processed.")

# --- 4. Download and Process FRENCHDATA Files ---

# --- 4a. 17 Industry Portfolios ---
ff_17_industry_monthly_raw <- download_french_data("17 Industry Portfolios")
ff_17_industry_monthly <- ff_17_industry_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(all_of(industry_names), ~ as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  filter(date >= start_date & date <= end_date) |>
  mutate(date = ceiling_date(date, "month") - days(1))

print("Fama-French 17 Industry data downloaded and processed.")

# --- 4b. Fama-French Regression Factors (FF5 + Mom) ---
factors_ff5_monthly_raw <- download_french_data("Fama/French 5 Factors (2x3)")
factors_ff5_monthly <- factors_ff5_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |>
  filter(date >= start_date & date <= end_date) |>
  mutate(date = ceiling_date(date, "month") - days(1))

factors_mom_monthly_raw <- download_french_data("Momentum Factor (Mom)")
factors_mom_monthly <- factors_mom_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(Mom), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  filter(date >= start_date & date <= end_date) |>
  mutate(date = ceiling_date(date, "month") - days(1))

# This is the final, pre-loaded object for regressions
ff_reg_factors_loaded <- inner_join(factors_ff5_monthly, factors_mom_monthly, by = "date")

print("Fama-French regression factors downloaded and joined.")

# --- 5. Merge and Rename All Factor Data ---
merged_factors <- inner_join(all_themes_wide, all_factors_wide, by = "date") %>%
  mutate(date = ceiling_date(date, "month") - days(1))

final_merged <- inner_join(ff_17_industry_monthly, merged_factors, by = "date")

# Your renaming map
factor_rename_map <- c(
  "Size_SMB" = "market_equity", "Book_to_Market_HML" = "be_me",
  "Operating_Profitability_RMW" = "ope_be", "Asset_Growth_CMA" = "at_gr1",
  "Long_Term_Reversals_LTREV" = "ret_60_12", "Residual_Variance_RVAR" = "ivol_ff3_21d",
  "Quality_Minus_Junk_QMJ" = "qmj", "Low_Beta_BAB" = "betabab_1260d",
  "Amihud_Illiquidity" = "ami_126d", "Firm_Age" = "age",
  "Nominal_Price" = "prc", "High_Volume_Premium" = "dolvol_126d",
  "Gross_Profitability" = "gp_at", "Return_on_Equity" = "ni_be",
  "Return_on_Assets" = "niq_at", "Profit_Margin" = "ebit_sale",
  "Change_in_Asset_Turnover" = "at_turnover",
  "Accruals_Factor" = "oaccruals_at", "Net_Operating_Assets" = "noa_at",
  "Net_Working_Capital_Changes" = "cowc_gr1a", "Cash_Flow_to_Price" = "ocf_me",
  "Earnings_to_Price" = "ni_me", "Enterprise_Multiple" = "ebitda_mev",
  "Sales_to_Price" = "sale_me",
  "Growth_in_Inventory" = "inv_gr1", "Sales_Growth" = "sale_gr1",
  "Growth_in_Sales_Inventory" = "dsale_dinv", "Abnormal_Investment" = "capex_abn",
  "CAPX_Growth_Rate" = "capx_gr1",
  "Debt_Issuance_Factor" = "dbnetis_at", "Leverage_Factor" = "at_be",
  "One_Year_Share_Issuance" = "chcsho_12m", "Total_External_Financing" = "netis_at",
  "Ohlson_O_Score" = "o_score", "Altman_Z_Score" = "z_score",
  "Piotroski_F_Score" = "f_score"
)

final_merged_renamed <- final_merged %>%
  rename(any_of(factor_rename_map))

renamed_factor_cols <- intersect(names(factor_rename_map), colnames(final_merged_renamed))

print("All data merged and renamed.")

# --- 6. Create the Final Data List ---
# This list will contain EVERYTHING our Shiny app needs
app_data <- list(
  data = final_merged_renamed,
  industry_cols = industry_cols,
  factor_cols = renamed_factor_cols,
  ff_reg_factors_loaded = ff_reg_factors_loaded
)

# --- 7. Save the List to an RData file ---
save(app_data, file = "quant_data.RData")

print("--- Successfully created 'quant_data.RData' ---")
