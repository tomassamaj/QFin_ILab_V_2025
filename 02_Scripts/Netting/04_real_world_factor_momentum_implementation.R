# ==============================================================================
# REAL-WORLD FACTOR MOMENTUM: FROM THEORY TO ACTUAL STOCK POSITIONS
# ==============================================================================
# Purpose: Build a tradeable factor momentum strategy with actual stock weights
# Flow: Daily Data → Monthly Factors → Strategy Selection → Stock Positions
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, arrow, data.table, lubridate, zoo, scales, ggplot2, gridExtra
)

# --- CONFIGURATION ---
BASE_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
DATA_DIR <- file.path(BASE_DIR, "01_Data/Clean_Daily_Inputs")

# Inputs
DAILY_PFS_FILE <- file.path(DATA_DIR, "pfs_daily.parquet")
STOCK_RETURNS_FILE <- file.path(DATA_DIR, "Global_Stock_Returns_Monthly.parquet")
WEIGHTS_FILE <- file.path(BASE_DIR, "01_Data/Processed/usa_factor_weights_corrected.parquet")

# Outputs
OUTPUT_DIR <- file.path(BASE_DIR, "03_Outputs/Strategy")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# STEP 1: CONVERT DAILY PORTFOLIO SORTS TO MONTHLY FACTOR RETURNS
# ==============================================================================
# Why: We need monthly factor returns to match the weight rebalancing frequency

cat("\n=== STEP 1: Building Monthly Factor Returns from Daily Data ===\n")

pfs_daily <- read_parquet(DAILY_PFS_FILE) %>%
  filter(excntry == "USA") %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  collect()
setDT(pfs_daily)

# Calculate daily long-short returns
daily_factors <- pfs_daily[, .(
  ret_long = ret[pf == max(pf)],
  ret_short = ret[pf == min(pf)]
), by = .(date, characteristic)]
daily_factors[, factor_ret := ret_long - ret_short]

# Add month identifier AFTER creating daily_factors
daily_factors[, month_date := floor_date(date, "month")]

# Apply sign corrections (from your audit)
# Load the list of factors to flip
factors_to_flip <- c(
  "betabab_1260d", "market_equity", "beta_60m", "ivol_ff3_21d",
  "age", "prc", "ret_1_0", "sale_gr1", "at_gr1", "at_be",
  "ret_60_12", "chcsho_12m", "netis_at", "o_score", "capx_gr1",
  "oaccruals_at", "dolvol_126d", "inv_gr1", "cowc_gr1a",
  "capex_abn", "dbnetis_at", "noa_at"
)

daily_factors[characteristic %in% factors_to_flip, factor_ret := factor_ret * -1]

# Aggregate to MONTHLY returns (compound daily returns within each month)
monthly_factors <- daily_factors[, .(
  monthly_ret = exp(sum(log(1 + factor_ret))) - 1,
  n_days = .N
), by = .(month_date, characteristic)]

# Rename for consistency
setnames(monthly_factors, "month_date", "month")

cat("Monthly factor returns calculated for", uniqueN(monthly_factors$characteristic), "factors\n")
cat("Date range:", as.character(min(monthly_factors$month)), "to", as.character(max(monthly_factors$month)), "\n")

# ==============================================================================
# STEP 2: TEST MULTIPLE STRATEGY VARIANTS
# ==============================================================================
# We'll test different long/short configurations to find the best one

cat("\n=== STEP 2: Testing Strategy Variants ===\n")

test_strategy <- function(monthly_rets, lookback = 1, strategy_name, 
                         long_pct = 0.5, short_pct = 0.5) {
  
  # Convert to wide format for cross-sectional ranking
  wide_rets <- dcast(monthly_rets, month ~ characteristic, value.var = "monthly_ret")
  setorder(wide_rets, month)
  
  # Calculate momentum signals (lookback-month return)
  signal_cols <- setdiff(names(wide_rets), "month")
  for (col in signal_cols) {
    set(wide_rets, j = paste0(col, "_signal"), 
        value = shift(wide_rets[[col]], n = lookback, type = "lag"))
  }
  
  # Remove first lookback rows (no signal yet)
  wide_rets <- wide_rets[(lookback + 1):.N]
  
  # Calculate strategy returns for each month
  strategy_rets <- wide_rets[, {
    # Extract signals and returns
    signals <- unlist(.SD[, paste0(signal_cols, "_signal"), with = FALSE])
    returns <- unlist(.SD[, signal_cols, with = FALSE])
    
    # Ensure we have valid data
    valid_idx <- !is.na(signals) & !is.na(returns)
    signals <- signals[valid_idx]
    returns <- returns[valid_idx]
    
    if (length(signals) < 2) {
      list(strategy_ret = NA_real_)
    } else {
      # Rank signals
      n_factors <- length(signals)
      long_cutoff <- quantile(signals, 1 - long_pct, na.rm = TRUE)
      short_cutoff <- quantile(signals, short_pct, na.rm = TRUE)
      
      # Create positions
      long_mask <- signals >= long_cutoff
      short_mask <- signals <= short_cutoff
      
      # Equal-weight within each leg
      n_long <- sum(long_mask)
      n_short <- sum(short_mask)
      
      long_ret <- if (n_long > 0) sum(returns[long_mask]) / n_long else 0
      short_ret <- if (n_short > 0) sum(returns[short_mask]) / n_short else 0
      
      list(
        strategy_ret = long_ret - short_ret,
        long_only_ret = long_ret,
        n_long = n_long,
        n_short = n_short
      )
    }
  }, by = .(month)]
  
  strategy_rets[, strategy := strategy_name]
  return(strategy_rets)
}

# Test configurations
strategies <- list(
  list(name = "Median_LS", long_pct = 0.5, short_pct = 0.5),
  list(name = "Top25_Bottom25", long_pct = 0.25, short_pct = 0.25),
  list(name = "Top50_LO", long_pct = 0.5, short_pct = 0),
  list(name = "Top25_LO", long_pct = 0.25, short_pct = 0),
  list(name = "Top33_Bottom33", long_pct = 0.33, short_pct = 0.33)
)

all_strategy_results <- rbindlist(lapply(strategies, function(s) {
  test_strategy(monthly_factors, lookback = 1, 
               strategy_name = s$name, 
               long_pct = s$long_pct, 
               short_pct = s$short_pct)
}))

# Calculate performance metrics
perf_summary <- all_strategy_results[!is.na(strategy_ret), .(
  Total_Return = prod(1 + strategy_ret) - 1,
  Annualized_Return = (prod(1 + strategy_ret)^(12/.N)) - 1,
  Volatility = sd(strategy_ret) * sqrt(12),
  Sharpe = (mean(strategy_ret) * 12) / (sd(strategy_ret) * sqrt(12)),
  Max_Drawdown = {
    cum_ret <- cumprod(1 + strategy_ret)
    max(cummax(cum_ret) / cum_ret - 1)
  },
  N_months = .N
), by = strategy]

perf_summary[, Calmar := Annualized_Return / Max_Drawdown]
setorder(perf_summary, -Sharpe)

cat("\n--- Strategy Performance Comparison ---\n")
print(perf_summary)

# Select best strategy
best_strategy <- perf_summary[1, strategy]
cat("\n✅ Best Strategy:", best_strategy, "\n")

# ==============================================================================
# STEP 3: IDENTIFY WHICH FACTORS TO HOLD (WITH IMPLEMENTATION LAG)
# ==============================================================================

cat("\n=== STEP 3: Generating Factor Positions (Monthly Rebalancing) ===\n")

# Get the configuration for the best strategy
best_config <- strategies[[which(sapply(strategies, function(x) x$name == best_strategy))]]

# Build factor positions with 1-day implementation lag
wide_rets <- dcast(monthly_factors, month ~ characteristic, value.var = "monthly_ret")
setorder(wide_rets, month)

signal_cols <- setdiff(names(wide_rets), "month")

# Calculate signals (1-month lookback)
for (col in signal_cols) {
  set(wide_rets, j = paste0(col, "_signal"), 
      value = shift(wide_rets[[col]], n = 1, type = "lag"))
}

# Determine positions for each month
factor_positions <- wide_rets[(2:.N), {
  signals <- unlist(.SD[, paste0(signal_cols, "_signal"), with = FALSE])
  names(signals) <- signal_cols
  
  valid_signals <- signals[!is.na(signals)]
  
  if (length(valid_signals) < 2) {
    data.table(characteristic = character(0), position = numeric(0))
  } else {
    long_cutoff <- quantile(valid_signals, 1 - best_config$long_pct, na.rm = TRUE)
    short_cutoff <- quantile(valid_signals, best_config$short_pct, na.rm = TRUE)
    
    positions <- case_when(
      valid_signals >= long_cutoff ~ 1,
      valid_signals <= short_cutoff ~ -1,
      TRUE ~ 0
    )
    
    data.table(
      characteristic = names(valid_signals),
      position = positions,
      signal = valid_signals
    )[position != 0]
  }
}, by = .(month)]

# Add implementation lag: positions determined at month-end T are traded on day 1 of month T+1
factor_positions[, trade_month := month %m+% months(1)]

cat("Factor positions generated for", uniqueN(factor_positions$month), "months\n")
cat("Average factors per month:", factor_positions[, .N, by = .(month)][, mean(N)], "\n")

# ==============================================================================
# STEP 4: CONVERT FACTOR POSITIONS TO ACTUAL STOCK WEIGHTS
# ==============================================================================

cat("\n=== STEP 4: Mapping Factors to Single Stock Positions ===\n")

# Load stock-level weights (from your corrected weights file)
stock_weights <- read_parquet(WEIGHTS_FILE) %>%
  select(id, eom, characteristic, weight, leg) %>%
  collect()
setDT(stock_weights)

# Map: Weights are for month T → Apply to returns of month T+1
stock_weights[, trade_month := floor_date(eom, "month") %m+% months(1)]

# Merge factor positions with stock weights
# Logic: 
# - If factor position = 1 (long), we want stocks in leg = 1 (high characteristic)
# - If factor position = -1 (short), we want stocks in leg = -1 (low characteristic)

stock_positions <- merge(
  factor_positions,
  stock_weights,
  by = c("trade_month", "characteristic"),
  allow.cartesian = TRUE
)

# Filter: Only keep stocks where position direction matches leg
# For long factor: keep leg = 1
# For short factor: keep leg = -1
stock_positions <- stock_positions[
  (position == 1 & leg == 1) | (position == -1 & leg == -1)
]

# Calculate stock-level weights
# Each factor gets equal weight, and within each factor, stocks are weighted per 'weight' column
stock_positions[, n_factors_held := uniqueN(characteristic), by = trade_month]
stock_positions[, factor_allocation := 1 / n_factors_held]

# Stock weight = (Factor allocation) × (Stock's weight within factor) × (Direction)
stock_positions[, stock_weight := factor_allocation * weight * sign(position)]

# Aggregate across factors (net out positions if a stock appears in multiple factors)
final_portfolio <- stock_positions[, .(
  net_weight = sum(stock_weight),
  n_factors = .N
), by = .(trade_month, id)]

cat("\n--- Portfolio Statistics ---\n")
cat("Average stocks per month:", final_portfolio[, .N, by = trade_month][, mean(N)], "\n")
cat("Max stocks in any month:", final_portfolio[, .N, by = trade_month][, max(N)], "\n")

# ==============================================================================
# STEP 5: CALCULATE ACTUAL STRATEGY RETURNS FROM STOCK POSITIONS
# ==============================================================================

cat("\n=== STEP 5: Backtesting with Actual Stock Returns ===\n")

# Load monthly stock returns
stock_returns <- read_parquet(STOCK_RETURNS_FILE) %>%
  filter(excntry == "USA") %>%
  select(id, eom, ret = ret_exc) %>%
  collect()
setDT(stock_returns)
stock_returns[, trade_month := floor_date(eom, "month")]

# Merge portfolio weights with actual returns
backtest <- merge(
  final_portfolio,
  stock_returns,
  by = c("id", "trade_month"),
  all.x = TRUE
)

# Calculate portfolio return each month
portfolio_returns <- backtest[, .(
  portfolio_ret = sum(net_weight * ret, na.rm = TRUE),
  n_stocks = .N,
  weight_coverage = sum(abs(net_weight), na.rm = TRUE)
), by = trade_month]

setorder(portfolio_returns, trade_month)

# Calculate cumulative performance
portfolio_returns[, cum_ret := cumprod(1 + portfolio_ret)]

# Performance metrics
actual_perf <- portfolio_returns[, .(
  Total_Return = prod(1 + portfolio_ret) - 1,
  Ann_Return = (prod(1 + portfolio_ret)^(12/.N)) - 1,
  Ann_Vol = sd(portfolio_ret) * sqrt(12),
  Sharpe = (mean(portfolio_ret) * 12) / (sd(portfolio_ret) * sqrt(12)),
  Max_DD = max(cummax(cum_ret) / cum_ret - 1)
)]

cat("\n--- Actual Stock Portfolio Performance ---\n")
print(actual_perf)

# ==============================================================================
# STEP 6: VOLATILITY SCALING (OPTIONAL)
# ==============================================================================

cat("\n=== STEP 6: Applying Volatility Targeting ===\n")

TARGET_VOL <- 0.10
LOOKBACK_MONTHS <- 36

portfolio_returns[, rolling_vol := {
  if (.N < 12) NA_real_
  else rollapply(portfolio_ret, width = min(LOOKBACK_MONTHS, .N), 
                FUN = sd, fill = NA, align = "right") * sqrt(12)
}]

portfolio_returns[, leverage := shift(pmin(2.0, TARGET_VOL / rolling_vol), 1)]
portfolio_returns[is.na(leverage) | is.infinite(leverage), leverage := 1.0]

portfolio_returns[, scaled_ret := portfolio_ret * leverage]
portfolio_returns[, cum_scaled := cumprod(1 + scaled_ret)]

# Scaled performance
scaled_perf <- portfolio_returns[!is.na(scaled_ret), .(
  Total_Return = prod(1 + scaled_ret) - 1,
  Ann_Return = (prod(1 + scaled_ret)^(12/.N)) - 1,
  Ann_Vol = sd(scaled_ret) * sqrt(12),
  Sharpe = (mean(scaled_ret) * 12) / (sd(scaled_ret) * sqrt(12)),
  Max_DD = max(cummax(cum_scaled) / cum_scaled - 1)
)]

cat("\n--- Volatility-Scaled Performance ---\n")
print(scaled_perf)

# ==============================================================================
# STEP 7: VISUALIZATION & REPORTING
# ==============================================================================

cat("\n=== STEP 7: Generating Reports ===\n")

# 1. Performance Chart
p1 <- ggplot(portfolio_returns, aes(x = trade_month)) +
  geom_line(aes(y = cum_ret, color = "Unscaled"), linewidth = 1) +
  geom_line(aes(y = cum_scaled, color = "Vol-Scaled (10%)"), linewidth = 1) +
  scale_y_log10(labels = comma) +
  scale_color_manual(values = c("Unscaled" = "gray50", "Vol-Scaled (10%)" = "#27AE60")) +
  labs(
    title = paste("Real-World Factor Momentum Strategy:", best_strategy),
    subtitle = "1-Month Lookback | Monthly Rebalancing | 1-Day Implementation Lag",
    y = "Cumulative Wealth (Log Scale)",
    x = "",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# 2. Portfolio Concentration Over Time
p2 <- ggplot(portfolio_returns, aes(x = trade_month, y = n_stocks)) +
  geom_line(color = "#2980B9", linewidth = 0.8) +
  geom_smooth(se = FALSE, color = "red", linetype = "dashed", linewidth = 0.6) +
  labs(
    title = "Portfolio Concentration",
    subtitle = "Number of stocks held each month",
    y = "Number of Stocks",
    x = ""
  ) +
  theme_minimal()

print(p2)

# 3. Rolling Sharpe Ratio
portfolio_returns[, rolling_sharpe := {
  rollapply(portfolio_ret, width = 36, 
           FUN = function(x) (mean(x) * 12) / (sd(x) * sqrt(12)),
           fill = NA, align = "right")
}]

p3 <- ggplot(portfolio_returns, aes(x = trade_month, y = rolling_sharpe)) +
  geom_line(color = "#8E44AD", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Rolling 3-Year Sharpe Ratio",
    y = "Sharpe Ratio",
    x = ""
  ) +
  theme_minimal()

print(p3)

# ==============================================================================
# STEP 8: EXPORT FINAL OUTPUTS
# ==============================================================================

cat("\n=== STEP 8: Saving Outputs ===\n")

# 1. Save factor positions (for each month, which factors to hold)
write_parquet(
  factor_positions,
  file.path(OUTPUT_DIR, "factor_positions_monthly.parquet")
)

# 2. Save final stock portfolio (for each month, which stocks and weights)
write_parquet(
  final_portfolio,
  file.path(OUTPUT_DIR, "stock_portfolio_monthly.parquet")
)

# 3. Save performance time series
write_parquet(
  portfolio_returns,
  file.path(OUTPUT_DIR, "strategy_returns_monthly.parquet")
)

# 4. Create a "trade file" for the most recent month (what to buy/sell today)
latest_month <- max(final_portfolio$trade_month)
latest_portfolio <- final_portfolio[trade_month == latest_month]

write_csv(
  latest_portfolio[, .(
    stock_id = id,
    target_weight = net_weight,
    weight_pct = scales::percent(net_weight, accuracy = 0.01),
    direction = ifelse(net_weight > 0, "LONG", "SHORT"),
    num_factors = n_factors
  )][order(-abs(target_weight))],
  file.path(OUTPUT_DIR, paste0("trade_list_", latest_month, ".csv"))
)

cat("\n✅ All outputs saved to:", OUTPUT_DIR, "\n")
cat("\n=== IMPLEMENTATION COMPLETE ===\n")
cat("You now have:\n")
cat("  1. Monthly factor signals\n")
cat("  2. Stock-level positions (actual holdings)\n")
cat("  3. Backtested performance\n")
cat("  4. Trade file for current month\n")
