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
industry_cols <- str_to_lower(industry_names)
renamed_factor_cols <- intersect(names(factor_rename_map), colnames(final_merged_renamed))

# --- 6. Save Data Environment ---
save.image(file = "First_round_data.RData")
print("Data environment saved to First_round_data.RData")


# --- 7. NEW: Generalized Momentum Strategy Function ---

#' Calculates a momentum strategy with flexible lookback, holding, lag, and quantiles.
#'
#' @param df Data frame containing 'date' and all 'target_cols'.
#' @param target_cols Vector of column names to trade (e.g., industry or factor names).
#' @param lookback_months Integer, number of months to calculate signal return.
#' @param holding_months Integer, number of months to hold the portfolio.
#' @param signal_lag Integer, number of months to lag the signal (1 = standard, 2 = robustness test).
#' @param long_quantile Numeric, the upper quantile to go long (e.g., 0.7 for top 30%).
#' @param short_quantile Numeric, the lower quantile to go short (e.g., 0.3 for bottom 30%).
#' @return A data frame with date, long_return, short_return, and long_short_return.
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
  
  # 1. Select data and calculate rolling lookback returns
  momentum_signals <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    # Calculate rolling lookback returns (sum of log returns)
    mutate(across(all_of(target_cols_exist), 
                  # Use anonymous function `\(x)` for modern dplyr syntax
                  \(x) rollapply(log(1 + x), 
                                 width = lookback_months, 
                                 FUN = sum, 
                                 fill = NA, 
                                 align = "right"),
                  .names = "{.col}_signal")) %>%
    # Lag the signal
    # Use anonymous function `\(x)` for modern dplyr syntax
    mutate(across(ends_with("_signal"), \(x) lag(x, n = signal_lag))) %>%
    # Keep only rows with valid, lagged signals
    na.omit() %>%
    select(date, ends_with("_signal"))
  
  # 2. Get forward returns for the holding period
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
    # Lag forward returns to match signal date
    mutate(across(ends_with("_fwd_ret"), \(x) lag(x, n = 1))) %>%
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
      # Get all signals for this row
      signals = list(c_across(ends_with("_signal"))),
      
      # Calculate quantile thresholds
      q_short = quantile(signals, short_quantile, na.rm = TRUE),
      q_long = quantile(signals, long_quantile, na.rm = TRUE),
      
      # Get all forward returns for this row
      fwd_rets = list(c_across(ends_with("_fwd_ret")))
    ) %>%
    ungroup() %>%
    # Use pmap (from purrr) to iterate row by row
    mutate(
      returns = pmap(list(signals, fwd_rets, q_short, q_long), function(s, f, qs, ql) {
        
        long_indices <- which(s >= ql)
        short_indices <- which(s <= qs)
        
        # Handle median split (long_quantile = 0.5, short_quantile = 0.5)
        if (long_quantile == short_quantile) {
          long_indices <- which(s > ql) # > median
          short_indices <- which(s <= qs) # <= median
        }
        
        long_ret <- if(length(long_indices) > 0) mean(f[long_indices], na.rm = TRUE) else 0
        short_ret <- if(length(short_indices) > 0) mean(f[short_indices], na.rm = TRUE) else 0
        
        # Return as a small data frame (tibble)
        tibble(
          long_return = exp(long_ret) - 1,     # Convert back from log returns
          short_return = exp(short_ret) - 1,    # Convert back from log returns
          long_short_return = (exp(long_ret) - 1) - (exp(short_ret) - 1)
        )
      })
    ) %>%
    select(date, returns) %>%
    unnest(returns)
  
  return(strategy_returns)
}


# --- 8. Analyze Long vs. Short Legs (and 2000s Bubble) ---
print("--- 8. Running Long/Short Decomposition (1M/1M, Median) ---")

# Base case: 1M lookback, 1M hold, 1M lag, median split
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
  filter(Strategy %in% c("long_return", "short_return_inv", "long_short_return"))

# Plot Long vs. Short
ls_plot <- ggplot(plot_data_ls, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  # --- ADDED: Highlight for 2000s bubble ---
  annotate("rect", 
           xmin = ymd("2000-03-01"), xmax = ymd("2002-10-01"), 
           ymin = min(plot_data_ls$Cumulative_Return), ymax = max(plot_data_ls$Cumulative_Return),
           alpha = 0.2, fill = "gray50") +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("long_return" = "darkgreen", 
                                "short_return_inv" = "darkred", 
                                "long_short_return" = "blue"),
                     labels = c("Long-Only Leg", "Long-Short Strategy", "Short-Only Leg (Inverted)")) +
  labs(title = "Factor Momentum: Long vs. Short Leg Performance",
       subtitle = "1M/1M Strategy, Median Split. Shaded area = 2000-2002 Dot-com bubble burst.",
       x = "Year", y = "Cumulative Return (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(ls_plot)


# --- 9. Sensitivity Analysis (Grid) ---
print("--- 9. Running Sensitivity Analysis Grid ---")

# 1. Define parameter grid
param_grid <- expand.grid(
  lookback = c(1, 3, 6),
  holding = c(1, 3),
  lag = c(1, 2),
  short_q = c(0.3, 0.5), # 0.3 = Bottom 30%, 0.5 = Bottom 50%
  stringsAsFactors = FALSE
) %>%
  mutate(long_q = 1 - short_q) %>%
  filter(short_q == long_q | short_q != 0.5) %>%
  mutate(strategy_name = paste0("L", lookback, "_H", holding, "_Lag", lag, "_Q", short_q*100))

# Helper function to run one row of the grid and calculate Sharpe
run_sensitivity <- function(params) {
  cat("Running:", params$strategy_name, "\n")
  
  returns <- calculate_momentum_strategy(
    df = final_merged_renamed,
    target_cols = renamed_factor_cols,
    lookback_months = params$lookback,
    holding_months = params$holding,
    signal_lag = params$lag,
    long_quantile = params$long_q,
    short_quantile = params$short_q
  )
  
  if (is.null(returns) || nrow(returns) < 12) {
    return(tibble(strategy_name = params$strategy_name, Sharpe_Ratio = NA))
  }
  
  # Calculate annualized Sharpe
  sharpe <- (mean(returns$long_short_return, na.rm = TRUE) * 12) / 
    (sd(returns$long_short_return, na.rm = TRUE) * sqrt(12))
  
  return(tibble(strategy_name = params$strategy_name, Sharpe_Ratio = sharpe))
}

# Run the grid
sensitivity_results <- map_dfr(split(param_grid, 1:nrow(param_grid)), run_sensitivity)

print("Sensitivity Results (Sharpe Ratios):")
print(sensitivity_results %>% arrange(-Sharpe_Ratio), n = 24)


# --- 10. Factor Cluster Analysis ---
print("--- 10. Running Factor Cluster Analysis ---")

# 1. Create correlation matrix
factor_data_for_corr <- final_merged_renamed %>%
  select(any_of(renamed_factor_cols)) %>%
  na.omit()
cor_matrix <- cor(factor_data_for_corr)

# 2. Perform clustering
dist_matrix <- as.dist(1 - abs(cor_matrix)) # Use absolute correlation for distance
hclust_results <- hclust(dist_matrix, method = "ward.D2")
plot(hclust_results, main = "Factor Cluster Dendrogram", xlab = "", sub = "")

# 3. Cut tree into 5 clusters
K_CLUSTERS <- 5
clusters <- cutree(hclust_results, k = K_CLUSTERS)
cluster_list <- map(1:K_CLUSTERS, ~names(clusters[clusters == .x]))
names(cluster_list) <- paste0("Cluster_", 1:K_CLUSTERS)

print("Factor Clusters:")
print(cluster_list)

# 4. Run momentum strategy on each cluster
cluster_momentum_performance <- map_dfr(cluster_list, ~{
  strategy_returns <- calculate_momentum_strategy(
    df = final_merged_renamed,
    target_cols = .x, # Pass the vector of factor names for this cluster
    lookback_months = 1,
    holding_months = 1,
    signal_lag = 1,
    long_quantile = 0.5,
    short_quantile = 0.5
  )
  if (is.null(strategy_returns)) return(NULL)
  strategy_returns %>% mutate(Return = long_short_return)
}, .id = "Cluster")

# 5. Plot cluster momentum performance
cluster_plot_data <- cluster_momentum_performance %>%
  group_by(Cluster) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

cluster_plot <- ggplot(cluster_plot_data, aes(x = date, y = Cumulative_Return, color = Cluster)) +
  geom_line() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Momentum Performance by Factor Cluster",
       subtitle = "1M/1M Strategy, Median Split",
       x = "Year", y = "Cumulative Return (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(cluster_plot)


# --- 11. Drawdown and Crisis Analysis ---
print("--- 11. Running Drawdown & Crisis Analysis ---")

# 1. Get Market Data (Mkt-RF)
options(timeout = 300)
ff_factors_raw <- download_french_data("Fama/French 3 Factors")
mkt_data <- ff_factors_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    # *** FIX ***: Correctly quote "RF"
    across(c("Mkt-RF", "RF"), ~ as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename(mkt_excess = `Mkt-RF`) |>
  filter(date >= start_date & date <= end_date) |>
  mutate(date = ceiling_date(date, "month") - days(1))

# 2. Define Crisis Periods
crisis_gfc <- interval(ymd("2007-10-01"), ymd("2009-03-31"))
crisis_covid <- interval(ymd("2020-02-01"), ymd("2020-03-31"))

# 3. Combine strategy and market data
analysis_data <- base_factor_mom %>%
  select(date, factor_mom = long_short_return) %>%
  inner_join(mkt_data, by = "date")

# 4. Calculate performance during crises
crisis_performance <- analysis_data %>%
  mutate(
    period = case_when(
      # *** FIX ***: Use %within% operator from lubridate
      date %within% crisis_gfc ~ "GFC (2007-09)",
      date %within% crisis_covid ~ "COVID (2020)",
      TRUE ~ "Other"
    )
  ) %>%
  filter(period != "Other") %>%
  group_by(period) %>%
  summarise(
    Factor_Mom_Return = prod(1 + factor_mom) - 1,
    Market_Return = prod(1 + mkt_excess) - 1
  )

print("Performance during Crisis Periods:")
print(crisis_performance)

# 5. Calculate and plot drawdowns
drawdown_data <- analysis_data %>%
  select(date, factor_mom, mkt_excess) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Return") %>%
  group_by(Strategy) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  mutate(Peak = cummax(Cumulative_Return)) %>%
  mutate(Drawdown = (Cumulative_Return / Peak) - 1) %>%
  ungroup()

drawdown_plot <- ggplot(drawdown_data, aes(x = date, y = Drawdown, color = Strategy)) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("factor_mom" = "blue", "mkt_excess" = "black")) +
  labs(title = "Drawdown: Factor Momentum vs. Market (Mkt-RF)",
       x = "Year", y = "Drawdown from Peak") +
  theme_minimal()

print(drawdown_plot)

# 6. Calculate Tail Correlation
bad_market_months <- analysis_data %>%
  filter(mkt_excess < -0.02) # Define "bad" as < -2%

tail_correlation <- cor(bad_market_months$factor_mom, bad_market_months$mkt_excess)

print(paste("Correlation in 'Bad' Market Months (Mkt-RF < -2%):", 
            round(tail_correlation, 3)))

# --- 12. Portfolio Allocation Context ---
print("--- 12. Running Portfolio Context Analysis ---")

# 1. Create 60/40 Portfolio
# Using Mkt = Mkt-RF + RF (total market return) and RF (as 'bond' proxy)
port_60_40 <- analysis_data %>%
  # *** FIX ***: Changed 'rf' to 'RF' to match the column name
  mutate(mkt_total = mkt_excess + RF) %>%
  mutate(port_60_40_ret = 0.6 * mkt_total + 0.4 * RF) %>%
  select(date, port_60_40_ret)

# 2. Combine with strategy
allocation_data <- base_factor_mom %>%
  select(date, factor_mom = long_short_return) %>%
  inner_join(port_60_40, by = "date")

# 3. Calculate correlation
full_correlation <- cor(allocation_data$factor_mom, allocation_data$port_60_40_ret)

print(paste("Full-Sample Correlation of Factor Momentum to 60/40 Portfolio:", 
            round(full_correlation, 3)))


# --- 13. NEW: Volatility Scaled Analysis ---
print("--- 13. Running Vol-Scaled Analysis ---")

# 1. Define target volatility
TARGET_VOL <- 0.10 # 10%
LOOKBACK_MONTHS <- 36

# 2. Define Vol Scaling Function
scale_volatility <- function(returns_df, return_col = "long_short_return", 
                             target_ann_vol = 0.10, lookback_months = 36) {
  
  returns_df <- returns_df %>%
    arrange(date)
  
  # Calculate rolling monthly standard deviation
  rolling_sd <- rollapply(returns_df[[return_col]],
                          width = lookback_months,
                          FUN = sd,
                          na.rm = TRUE,
                          fill = NA,
                          align = "right",
                          partial = 12) # Require at least 12 months
  
  # Annualize volatility
  annualized_rolling_vol <- rolling_sd * sqrt(12)
  
  # Calculate leverage factor for NEXT month (use lag)
  leverage_factor <- lag(pmin(5, target_ann_vol / annualized_rolling_vol, na.rm = TRUE), 1)
  leverage_factor[is.infinite(leverage_factor) | is.na(leverage_factor)] <- 1 # Default to 1x
  
  # Calculate scaled return
  returns_df %>%
    mutate(
      leverage = leverage_factor,
      scaled_return = .data[[return_col]] * leverage
    ) %>%
    filter(!is.na(scaled_return))
}

# 3. Scale the base factor momentum strategy
scaled_factor_mom <- scale_volatility(base_factor_mom, 
                                      return_col = "long_short_return",
                                      target_ann_vol = TARGET_VOL,
                                      lookback_months = LOOKBACK_MONTHS)

# 4. Plot scaled performance
scaled_plot_data <- scaled_factor_mom %>%
  mutate(Cumulative_Return = cumprod(1 + scaled_return))

scaled_plot <- ggplot(scaled_plot_data, aes(x = date, y = Cumulative_Return)) +
  geom_line(color = "blue", linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = paste("Factor Momentum Scaled to", scales::percent(TARGET_VOL, 1), "Annual Volatility"),
    subtitle = paste("Lookback:", LOOKBACK_MONTHS, "months. Max Leverage: 5x"),
    x = "Year", y = "Cumulative Return (Log Scale)"
  ) +
  theme_minimal()

print(scaled_plot)

# 5. Calculate stats for scaled strategy
scaled_sharpe <- (mean(scaled_factor_mom$scaled_return) * 12) / 
  (sd(scaled_factor_mom$scaled_return) * sqrt(12))

print(paste("Scaled Strategy (", TARGET_VOL*100, "% Vol) Sharpe Ratio:", round(scaled_sharpe, 3)))


