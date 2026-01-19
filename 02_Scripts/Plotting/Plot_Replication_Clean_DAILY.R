# --- 0. Load Required Libraries ---
# Data manipulation
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
# Fama-French data
library(frenchdata)
# Plotting
library(ggplot2)
library(scales)
library(corrplot)
library(RColorBrewer)
library(broom) # For tidying model output
library(zoo) # For rollapply
library(purrr)     # For loops/mapping

# --- 1. Load and Prepare Factor/Theme Data (DAILY) ---

# Define start and end dates for analysis
start_date <- ymd("1963-07-01") 
end_date <- ymd("2024-12-31")   

# Load Daily Data from RData file
load("Daily_Factor_and_Themes_Data.RData")

# Prepare Factors (Daily)
all_factors_wide <- all_factors_daily_vw_cap %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>% 
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

# Prepare Themes (Daily)
all_themes_wide <- all_themes_daily_vw_cap %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>% 
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

# Merge themes and factors
merged_factors <- inner_join(all_themes_wide, all_factors_wide, by = "date")

# Load industry names
ind_ports <- read.csv("17_Industry_Portfolios.csv") 
industry_names <- ind_ports[7:23, 1]

# --- 2. Load and Prepare Fama-French Industry Data (DAILY) ---

ff_17_industry_daily_raw <- download_french_data("17 Industry Portfolios [Daily]")

ff_17_industry_daily <- ff_17_industry_daily_raw$subsets$data[[1]] |>
  mutate(
    date = ymd(date),
    across(all_of(industry_names), ~ as.numeric(.) / 100),
    .keep = "none" 
  ) |>
  rename_with(str_to_lower) |> 
  filter(date >= start_date & date <= end_date)

# --- 3. Merge All Data ---
final_merged <- inner_join(ff_17_industry_daily, merged_factors, by = "date")

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
theme_cols <- intersect(colnames(all_themes_wide)[-1], colnames(final_merged_renamed))


# --- 6. Function to Calculate Momentum (STRICT MONTHLY REBALANCING) ---
# This function creates a portfolio only once every 21 days.
calculate_momentum <- function(df, target_cols, strategy_name, lookback_days = 21, holding_days = 21) {
  
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) return(NULL)
  
  momentum_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    # 1. Calculate Signal: Rolling 21-day return (Lookback)
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1+x), width = lookback_days, FUN = sum, fill = NA, align = "right"), 
                  .names = "{.col}_signal")) %>%
    # 2. Calculate Forward Return: Next 21-day return (Holding Period)
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1+x), width = holding_days, FUN = sum, fill = NA, align = "left"), 
                  .names = "{.col}_fwd")) %>%
    # 3. Lag the Signal by 1 day (Trade at Open/Close of T using info from T-1)
    mutate(across(ends_with("_signal"), \(x) lag(x, n = 1))) %>%
    # 4. Filter for Rebalancing Dates (Strictly every 21 days)
    filter(row_number() %% holding_days == 1) %>%
    na.omit() %>%
    rowwise() %>%
    mutate(median_signal = median(c_across(ends_with("_signal")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(ends_with("_signal"),
                  ~ case_when(
                    !is.na(.) & . > median_signal ~ 1,
                    !is.na(.) & . <= median_signal ~ -1,
                    TRUE ~ 0 
                  ),
                  .names = "{sub('_signal', '_pos', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      n_long = sum(c_across(ends_with("_pos")) == 1),
      n_short = sum(c_across(ends_with("_pos")) == -1)
    ) %>%
    ungroup() %>%
    mutate(across(ends_with("_pos"),
                  ~ case_when(
                    . == 1 & n_long > 0 ~ 1 / n_long,
                    . == -1 & n_short > 0 ~ 1 / n_short, 
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      # 5. Calculate Return over the NEXT 21 days
      # We convert the log forward return back to simple return for the portfolio calculation
      momentum_return = sum(
        c_across(ends_with("_wgt")) * (exp(c_across(ends_with("_fwd"))) - 1), 
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    mutate(strategy_type = strategy_name)
  
  return(momentum_df)
}

# --- 7. Calculate Industry and Factor Momentum ---
# STRICT 21-day Lookback, STRICT 21-day Holding (Monthly Rebalancing)
industry_momentum <- calculate_momentum(final_merged_renamed, industry_cols, "Industry Momentum", lookback_days = 21, holding_days = 21)
factor_momentum <- calculate_momentum(final_merged_renamed, renamed_factor_cols, "Factor Momentum", lookback_days = 21, holding_days = 21)

# --- 8. Combine Momentum Series and Plot ---
if (!is.null(industry_momentum) && !is.null(factor_momentum)) {
  common_start_date <- max(min(industry_momentum$date), min(factor_momentum$date))
  
  combined_momentum <- bind_rows(industry_momentum, factor_momentum) %>%
    filter(date >= common_start_date) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate Cumulative Return (Compounding every 21 days)
    mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  print( 
    ggplot(combined_momentum, aes(x = date, y = cumulative_return_log, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(breaks = scales::log_breaks(n = 10)) +
      scale_color_manual(values = c("Industry Momentum" = "black", "Factor Momentum" = "blue")) +
      labs(title = "Cumulative Performance (Strict Monthly Rebalancing)",
           subtitle = "21-Day Lookback, 21-Day Holding Period",
           y = "Cumulative Performance ($)", color = "Strategy") +
      theme_minimal() + theme(legend.position = "top")
  )
}

# --- 9. Correlation Heatmap (Same as before) ---
factor_data_for_corr <- final_merged_renamed %>%
  select(any_of(renamed_factor_cols)) %>% na.omit()

if (ncol(factor_data_for_corr) >= 2) {
  cor_matrix <- cor(factor_data_for_corr)
  col_palette <- colorRampPalette(brewer.pal(n = 11, name = "BrBG"))(200)
  print(corrplot(cor_matrix, method = "color", type = "upper", order = "original", 
                 tl.col = "black", tl.cex = 0.5, col = col_palette, diag = FALSE))
}

# --- Function for Volatility Scaling (Adapted for Monthly Frequency) ---
scale_volatility <- function(df, target_ann_vol = 0.10, lookback_periods = 36, min_obs = 12) {
  # Since data is now 1 obs every 21 days (Monthly), we use Monthly parameters
  # lookback_periods = 36 (36 months)
  
  df <- df %>% arrange(date)
  
  rolling_sd <- rollapply(df$momentum_return, width = lookback_periods, FUN = sd, 
                          na.rm = TRUE, fill = NA, align = "right", partial = min_obs) 
  
  # Annualize: 12 observations per year
  annualized_rolling_vol <- rolling_sd * sqrt(12)
  
  leverage_factor <- lag(pmin(5, target_ann_vol / annualized_rolling_vol, na.rm = TRUE), 1)
  leverage_factor[is.infinite(leverage_factor) | is.na(leverage_factor)] <- 1 
  
  df %>% mutate(scaled_momentum_return = momentum_return * leverage_factor) %>%
    filter(!is.na(scaled_momentum_return)) 
}

# --- 9. Apply Volatility Scaling ---
target_vol <- 0.10; 
lookback_obs <- 36 # 36 Months (since data is now monthly frequency)

if (!is.null(industry_momentum)) industry_momentum_scaled <- scale_volatility(industry_momentum, target_ann_vol = target_vol, lookback_periods = lookback_obs)
if (!is.null(factor_momentum)) factor_momentum_scaled <- scale_volatility(factor_momentum, target_ann_vol = target_vol, lookback_periods = lookback_obs)

if (!is.null(industry_momentum_scaled) && !is.null(factor_momentum_scaled)) {
  common_start_date_scaled <- max(min(industry_momentum_scaled$date), min(factor_momentum_scaled$date))
  combined_momentum_scaled <- bind_rows(industry_momentum_scaled, factor_momentum_scaled) %>%
    filter(date >= common_start_date_scaled) %>%
    group_by(strategy_type) %>%
    mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>% ungroup()
  
  print(ggplot(combined_momentum_scaled, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
          geom_line(linewidth = 1) + scale_y_log10() +
          labs(title = "Scaled Performance (Strict Monthly Rebalancing)", y = "Cumulative Performance ($)"))
}

# --- 1. Download Fama-French 3 Factors (DAILY) ---
ff3_factors_daily_raw <- download_french_data("Fama/French 3 Factors [Daily]")
ff3_factors_daily <- ff3_factors_daily_raw$subsets$data[[1]] |>
  mutate(date = ymd(date), across(c("Mkt-RF", SMB, HML, RF), ~ as.numeric(.) / 100), .keep = "none") |>
  filter(date >= start_date & date <= end_date) |> rename(Mkt_RF = "Mkt-RF") 

# --- 3. Run Regressions (Summary only) ---
jkp_factor_name <- "Book_to_Market_HML" 
reg_data <- final_merged_renamed %>% select(date, all_of(jkp_factor_name)) %>%
  inner_join(ff3_factors_daily, by = "date") %>% mutate(Factor_Excess = .data[[jkp_factor_name]] - RF) %>% na.omit() 
print(summary(lm(Factor_Excess ~ Mkt_RF + SMB + HML, data = reg_data)))

################ ROUND 2 ####################
# FF5 and Mom Downloads skipped for brevity (assuming same as before)
# ... (Downloads remain the same) ...

# --- 7. NEW: Generalized Momentum Strategy Function (FIXED FOR MONTHLY REBALANCING) ---
calculate_momentum_strategy <- function(df, target_cols, 
                                        lookback_periods = 21,  
                                        holding_periods = 21,  
                                        signal_lag = 1, 
                                        long_quantile = 0.5, 
                                        short_quantile = 0.5) {
  
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) return(NULL)
  
  # 1. Calculate Signals (Rolling 21-day returns)
  momentum_signals <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1 + x), width = lookback_periods, FUN = sum, fill = NA, align = "right"), 
                  .names = "{.col}_signal")) %>%
    # Lag the signal by signal_lag days (e.g., 1 day)
    mutate(across(ends_with("_signal"), \(x) lag(x, n = signal_lag))) %>%
    select(date, ends_with("_signal"))
  
  # 2. Calculate Forward Returns (Next 21-day returns)
  forward_returns <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1 + x), width = holding_periods, FUN = sum, fill = NA, align = "left"), 
                  .names = "{.col}_fwd_ret")) %>%
    select(date, ends_with("_fwd_ret"))
  
  combined_data <- inner_join(momentum_signals, forward_returns, by = "date") %>% na.omit()
  
  # 3. Force Monthly Rebalancing (Select every 21st row)
  rebal_dates <- combined_data$date[seq(1, nrow(combined_data), by = holding_periods)]
  
  strategy_df <- combined_data %>% filter(date %in% rebal_dates)
  
  strategy_returns <- strategy_df %>%
    rowwise() %>%
    mutate(
      signals = list(c_across(ends_with("_signal"))),
      fwd_rets = list(c_across(ends_with("_fwd_ret"))),
      q_short = quantile(signals, short_quantile, na.rm = TRUE),
      q_long = quantile(signals, long_quantile, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      returns = pmap(list(signals, fwd_rets, q_short, q_long), function(s, f, qs, ql) {
        long_indices <- if (long_quantile == short_quantile) which(s > ql) else which(s >= ql)
        short_indices <- which(s <= qs)
        
        long_ret <- if(length(long_indices) > 0) mean(f[long_indices], na.rm = TRUE) else 0
        short_ret <- if(length(short_indices) > 0) mean(f[short_indices], na.rm = TRUE) else 0
        
        tibble(
          long_return = exp(long_ret) - 1,     
          short_return = exp(short_ret) - 1,    
          long_short_return = (exp(long_ret) - 1) - (exp(short_ret) - 1)
        )
      })
    ) %>%
    select(date, returns) %>% unnest(returns)
  
  return(strategy_returns)
}

# --- 8. Analyze Long vs. Short Legs (Monthly Rebalancing on Daily Data) ---
print("--- 8. Running Long/Short Decomposition (21d/21d, Median) ---")
base_factor_mom <- calculate_momentum_strategy(
  final_merged_renamed, renamed_factor_cols, 
  lookback_periods = 21, holding_periods = 21, signal_lag = 1
)

# Prepare Data for Plotting
plot_data_ls <- base_factor_mom %>%
  select(date, long_return, short_return, long_short_return) %>%
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Return") %>%
  group_by(Strategy) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

# Add explicit Start Date 
start_date_plot <- min(plot_data_ls$date) - 1
start_rows <- tibble(
  date = start_date_plot,
  Strategy = unique(plot_data_ls$Strategy),
  Return = 0,
  Cumulative_Return = 1.0
)
plot_data_final <- bind_rows(start_rows, plot_data_ls) %>% arrange(Strategy, date)

ls_plot <- ggplot(plot_data_final, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  annotate("rect", xmin = ymd("2000-03-01"), xmax = ymd("2002-10-01"), 
           ymin = min(plot_data_final$Cumulative_Return), ymax = max(plot_data_final$Cumulative_Return),
           alpha = 0.2, fill = "gray50") +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(
    values = c("long_return" = "darkgreen", "short_return_inv" = "darkred", "long_short_return" = "blue"),
    labels = c("Long-Only Leg", "Long-Short Strategy", "Short-Only Leg (Inverted)")
  ) +
  labs(title = "Factor Momentum (21d Lookback, Monthly Rebal)",
       subtitle = "Mimicking Monthly Data using Daily inputs",
       y = "Cumulative Return (Log Scale, Start=1)") +
  theme_minimal() + theme(legend.position = "bottom")

print(ls_plot)

## --- 9. Sensitivity Analysis ---
#param_grid <- expand.grid(
#  lookback = c(21, 63, 126), holding = c(21, 63), lag = c(1, 2), 
#  short_q = c(0.3, 0.5), stringsAsFactors = FALSE
#) %>% mutate(long_q = 1 - short_q) %>%
#  filter(short_q == long_q | short_q != 0.5) %>%
#  mutate(strategy_name = paste0("L", lookback, "_H", holding, "_Lag", lag, "_Q", short_q*100))
#
#run_sensitivity <- function(params) {
#  cat("Running:", params$strategy_name, "\n")
#  returns <- calculate_momentum_strategy(final_merged_renamed, renamed_factor_cols, 
#                                         lookback_periods = params$lookback, holding_periods = params$holding, 
#                                         signal_lag = params$lag, long_quantile = params$long_q, short_quantile = params$short_q)
#  if (is.null(returns) || nrow(returns) < 12) return(tibble(strategy_name = params$strategy_name, Sharpe_Ratio = NA))
#  
#  obs_per_year <- 252 / params$holding
#  sharpe <- (mean(returns$long_short_return, na.rm = TRUE) * obs_per_year) / 
#    (sd(returns$long_short_return, na.rm = TRUE) * sqrt(obs_per_year))
#  return(tibble(strategy_name = params$strategy_name, Sharpe_Ratio = sharpe))
#}
#print(map_dfr(split(param_grid, 1:nrow(param_grid)), run_sensitivity) %>% arrange(-Sharpe_Ratio), n = 24)
#
## --- 10. Factor Cluster Analysis (Daily inputs, Monthly Rebal) ---
#factor_data_for_corr <- final_merged_renamed %>% select(any_of(renamed_factor_cols)) %>% na.omit()
#clusters <- cutree(hclust(as.dist(1 - abs(cor(factor_data_for_corr))), method = "ward.D2"), k = 5)
#cluster_list <- map(1:5, ~names(clusters[clusters == .x]))
#names(cluster_list) <- paste0("Cluster_", 1:5)
#
#cluster_momentum_performance <- map_dfr(cluster_list, function(cluster_factors) {
#  calculate_momentum_strategy(final_merged_renamed, cluster_factors, 
#                              lookback_periods = 21, holding_periods = 21, signal_lag = 1)
#}, .id = "Cluster")
#
#cluster_plot_data <- cluster_momentum_performance %>%
#  select(Cluster, date, Return = long_short_return) %>%
#  group_by(Cluster) %>% arrange(date) %>% mutate(Cumulative_Return = cumprod(1 + Return)) %>% ungroup()
#
#print(ggplot(cluster_plot_data, aes(x = date, y = Cumulative_Return, color = Cluster)) +
#        geom_line() + scale_y_log10() + labs(title = "Cluster Performance (21d/21d Strategy)"))
#
## --- 13. Volatility Scaled (Daily) ---
#TARGET_VOL <- 0.10 
## Lookback: 36 "Months" (where 1 Month = 21 day chunk)
#scale_volatility_strat <- function(returns_df, target_ann_vol = 0.10, lookback_obs = 36) {
#  returns_df <- returns_df %>% arrange(date)
#  rolling_sd <- rollapply(returns_df$long_short_return, width = lookback_obs, FUN = sd, na.rm = TRUE, fill = NA, align = "right")
#  
#  # Annualize: We have 1 observation every 21 days (approx 12 per year)
#  obs_per_year <- 12 
#  annualized_rolling_vol <- rolling_sd * sqrt(obs_per_year)
#  
#  leverage_factor <- lag(pmin(5, target_ann_vol / annualized_rolling_vol, na.rm = TRUE), 1)
#  leverage_factor[is.infinite(leverage_factor) | is.na(leverage_factor)] <- 1
#  
#  returns_df %>% mutate(scaled_return = long_short_return * leverage_factor) %>% filter(!is.na(scaled_return))
#}
#
#scaled_factor_mom <- scale_volatility_strat(base_factor_mom, target_ann_vol = TARGET_VOL, lookback_obs = 36)
#print(ggplot(scaled_factor_mom %>% mutate(Cum = cumprod(1+scaled_return)), aes(x=date, y=Cum)) + geom_line() + scale_y_log10() + labs(title="Vol Scaled (Monthly Rebal)"))