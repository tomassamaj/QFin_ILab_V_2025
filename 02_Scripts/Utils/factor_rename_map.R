# ==============================================================================
# Shared Factor Rename Map
# Maps JKP internal column names -> human-readable display names.
# Source this file from any plotting or analysis script that needs factor names.
#
# Usage:  source("02_Scripts/Utils/factor_rename_map.R")
# ==============================================================================

factor_rename_map <- c(
  # Common factors
  "Size_SMB"                    = "market_equity",
  "Book_to_Market_HML"          = "be_me",
  "Operating_Profitability_RMW" = "ope_be",
  "Asset_Growth_CMA"            = "at_gr1",
  "Long_Term_Reversals_LTREV"   = "ret_60_12",
  "Residual_Variance_RVAR"      = "ivol_ff3_21d",
  "Quality_Minus_Junk_QMJ"      = "qmj",
  "Low_Beta_BAB"                = "betabab_1260d",
  # Non-fundamental
  "Amihud_Illiquidity"          = "ami_126d",
  "Firm_Age"                    = "age",
  "Nominal_Price"               = "prc",
  "High_Volume_Premium"         = "dolvol_126d",
  # Profitability
  "Gross_Profitability"         = "gp_at",
  "Return_on_Equity"            = "ni_be",
  "Return_on_Assets"            = "niq_at",
  "Profit_Margin"               = "ebit_sale",
  "Change_in_Asset_Turnover"    = "at_turnover",
  # Earnings quality
  "Accruals_Factor"             = "oaccruals_at",
  "Net_Operating_Assets"        = "noa_at",
  "Net_Working_Capital_Changes" = "cowc_gr1a",
  "Cash_Flow_to_Price"          = "ocf_me",
  "Earnings_to_Price"           = "ni_me",
  "Enterprise_Multiple"         = "ebitda_mev",
  "Sales_to_Price"              = "sale_me",
  # Investment and growth
  "Growth_in_Inventory"         = "inv_gr1",
  "Sales_Growth"                = "sale_gr1",
  "Growth_in_Sales_Inventory"   = "dsale_dinv",
  "Abnormal_Investment"         = "capex_abn",
  "CAPX_Growth_Rate"            = "capx_gr1",
  # Financing
  "Debt_Issuance_Factor"        = "dbnetis_at",
  "Leverage_Factor"             = "at_be",
  "One_Year_Share_Issuance"     = "chcsho_12m",
  "Total_External_Financing"    = "netis_at",
  # Distress
  "Ohlson_O_Score"              = "o_score",
  "Altman_Z_Score"              = "z_score",
  # Composite
  "Piotroski_F_Score"           = "f_score"
)
