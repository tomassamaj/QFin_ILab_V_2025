# --- 0. Load Required Libraries ---
# Data manipulation
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)     # For loops/mapping
library(zoo)       # For rollapply
library(broom)     # For tidying models

# Fama-French data
library(frenchdata)

# Plotting
library(ggplot2)
library(scales)
library(corrplot)
library(RColorBrewer)

# --- 1. Load and Prepare Factor/Theme Data ---
# !!! IMPORTANT: SET YOUR FILE PATHS HERE !!!
path_to_factors_csv <- "[usa]_[all_factors]_[monthly]_[vw_cap].csv"
path_to_themes_csv <- "[usa]_[all_themes]_[monthly]_[vw_cap].csv"
path_to_industry_csv <- "17_Industry_Portfolios.csv"
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Check if files exist before trying to read
if (!file.exists(path_to_factors_csv) | !file.exists(path_to_themes_csv) | !file.exists(path_to_industry_csv)) {
  stop("One or more file paths are incorrect. Please update the path variables at the top of the script.")
}

# Define start and end dates for analysis
start_date <- ymd("1963-07-01") # Match factor data availability
end_date <- ymd("2024-12-31")   # Match factor data availability

# Load factor returns from CSV
all_factors_monthly_vw_cap <- read.csv(path_to_factors_csv)
all_factors_wide <- all_factors_monthly_vw_cap %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>% # Convert date early
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

# Load theme returns from CSV
all_themes_monthly_vw_cap <- read.csv(path_to_themes_csv)
all_themes_wide <- all_themes_monthly_vw_cap %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>% # Convert date early
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

# Merge themes and factors
merged_factors <- inner_join(all_themes_wide, all_factors_wide, by = "date")

ind_ports <- read.csv(path_to_industry_csv) # Load industry names

# --- 2. Load and Prepare Fama-French Industry Data ---
# (This is needed for the 'final_merged' step, even if not plotted)
industry_names <- ind_ports[7:23, 1]

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


# --- 3. Merge All Data ---
final_merged <- inner_join(ff_17_industry_monthly, merged_factors, by = "date")

# --- 4. Rename Factors to Readable Names ---
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

# --- 5. Define Columns for Analysis ---
renamed_factor_cols <- intersect(names(factor_rename_map), colnames(final_merged_renamed))


# --- 6. Generalized Momentum Strategy Function ---

#' Calculates a momentum strategy with flexible lookback, holding, lag, and quantiles.
#'
calculate_momentum_strategy <- function(df, target_cols, 
                                        lookback_months = 1, 
                                        holding_months = 1, 
                                        signal_lag = 1, 
                                        long_quantile = 0.5, 
                                        short_quantile = 0.5) {
  
  # Ensure valid columns
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) return(NULL)
  
  # 1. Select data and calculate rolling lookback returns (SIGNAL)
  momentum_signals <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    # Calculate rolling lookback returns (sum of log returns)
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1 + x), 
                                 width = lookback_months, 
                                 FUN = sum, 
                                 fill = NA, 
                                 align = "right"),
                  .names = "{.col}_signal")) %>%
    # Lag the signal
    mutate(across(ends_with("_signal"), \(x) lag(x, n = signal_lag))) %>%
    na.omit() %>%
    select(date, ends_with("_signal"))
  
  # 2. Get forward returns for the holding period (PAYOFF)
  forward_returns <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    # Calculate *forward* holding period returns
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1 + x), 
                                 width = holding_months, 
                                 FUN = sum, 
                                 fill = NA, 
                                 align = "left"), # Align left for forward return
                  .names = "{.col}_fwd_ret")) %>%
    select(date, ends_with("_fwd_ret")) %>%
    na.omit()
  
  # 3. Combine signals and forward returns
  combined_data <- inner_join(momentum_signals, forward_returns, by = "date")
  
  # 4. Filter for rebalancing dates (every 'holding_months')
  rebal_dates <- combined_data$date[seq(1, nrow(combined_data), by = holding_months)]
  
  strategy_df <- combined_data %>%
    filter(date %in% rebal_dates)
  
  # 5. Form portfolios and calculate returns
  strategy_returns <- strategy_df %>%
    rowwise() %>%
    mutate(
      signals = list(c_across(ends_with("_signal"))),
      q_short = quantile(signals, short_quantile, na.rm = TRUE),
      q_long = quantile(signals, long_quantile, na.rm = TRUE),
      fwd_rets = list(c_across(ends_with("_fwd_ret")))
    ) %>%
    ungroup() %>%
    mutate(
      returns = pmap(list(signals, fwd_rets, q_short, q_long), function(s, f, qs, ql) {
        
        long_indices <- which(s >= ql)
        short_indices <- which(s <= qs)
        
        if (long_quantile == short_quantile) {
          long_indices <- which(s > ql) # > median
          short_indices <- which(s <= qs) # <= median
        }
        
        long_ret <- if(length(long_indices) > 0) mean(f[long_indices], na.rm = TRUE) else 0
        short_ret <- if(length(short_indices) > 0) mean(f[short_indices], na.rm = TRUE) else 0
        
        tibble(
          long_return = exp(long_ret) - 1,
          short_return = exp(short_ret) - 1,
          long_short_return = (exp(long_ret) - 1) - (exp(short_ret) - 1)
        )
      })
    ) %>%
    select(date, returns) %>%
    unnest(returns)
  
  return(strategy_returns)
}


# --- 7. Run Strategy & Plot Long/Short/Long-Short ---
print("--- Running 1-Month Cross-Sectional Factor Momentum (Arnott et al. 2023) ---")

# This definition matches the Arnott et al. paper and student slides
# lookback_months = 1: Signal is prior 1-month return
# holding_months = 1: Hold for 1 month
# signal_lag = 1: Use return from t-1 to decide portfolio at t
# long_quantile = 0.5 / short_quantile = 0.5: Long top half (above-median), Short bottom half (below-median)

base_factor_mom <- calculate_momentum_strategy(final_merged_renamed, 
                                               renamed_factor_cols,
                                               lookback_months = 1,
                                               holding_months = 1,
                                               signal_lag = 1,
                                               long_quantile = 0.5,
                                               short_quantile = 0.5)

# Prepare data for plotting
plot_data_ls <- base_factor_mom %>%
  select(date, long_return, short_return, long_short_return) %>%
  # Invert short return for plotting
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Return") %>%
  group_by(Strategy) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup() %>%
  # Filter for the 3 requested lines
  filter(Strategy %in% c("long_return", "short_return_inv", "long_short_return"))

# Plot Long vs. Short
ls_plot <- ggplot(plot_data_ls, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  # Add highlight for 2000s bubble
  annotate("rect", 
           xmin = ymd("2000-03-01"), xmax = ymd("2002-10-01"), 
           ymin = min(plot_data_ls$Cumulative_Return, na.rm=T), 
           ymax = max(plot_data_ls$Cumulative_Return, na.rm=T),
           alpha = 0.2, fill = "gray50") +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("long_return" = "darkgreen", 
                                "short_return_inv" = "darkred", 
                                "long_short_return" = "blue"),
                     labels = c("Long-Only Leg", "Long-Short Strategy", "Short-Only Leg (Inverted)")) +
  labs(title = "Factor Momentum: Long vs. Short Leg Performance (Arnott et al. 2023 Replication)",
       subtitle = "1M/1M Strategy, Median Split. Shaded area = 2000-2002 Dot-com bubble burst.",
       x = "Year", y = "Cumulative Return (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(ls_plot)
