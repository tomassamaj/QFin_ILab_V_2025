# ==============================================================================
# JKP REPLICATION: DAILY MOMENTUM (CORE FACTORS ONLY)
# Purpose: Apply 21-Day Momentum Logic to a Restricted "Core" Factor Universe
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, arrow, lubridate, zoo, ggplot2, scales)

# --- CONFIGURATION ---
#setwd(
#  "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
#)
setwd(
  "C:/WU/ILab ZZ/QFin_ILab_V_2025"
)

DAILY_PFS_FILE <- "01_Data/Raw/pfs_daily.parquet"
OUTPUT_PLOT <- "03_Outputs/Figures/JKP_Momentum_Replication_Core.pdf"

# --- 1. DEFINE THE "CORE" FACTOR MAP ---
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

# ------------------------------------------------------------------------------
# 2. LOAD & FILTER DAILY DATA
# ------------------------------------------------------------------------------
cat("--- 1. Processing Daily JKP Data (Filtered for Core Factors) ---\n")

# Load only necessary columns
jkp_daily <- read_parquet(DAILY_PFS_FILE) %>%
  filter(excntry == "USA") %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  # *** CRITICAL STEP: FILTER FOR YOUR SPECIFIC FACTORS ***
  filter(characteristic %in% factor_rename_map)

# Construct High-Minus-Low Factors
daily_factors <- jkp_daily %>%
  group_by(date, characteristic) %>%
  mutate(
    is_long = (pf == max(pf)),
    is_short = (pf == min(pf))
  ) %>%
  filter(is_long | is_short) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(date, characteristic),
    names_from = is_long,
    values_from = ret,
    values_fn = mean
  ) %>%
  rename(ret_long = `TRUE`, ret_short = `FALSE`) %>%
  mutate(
    # Basic High-Low construction
    factor_ret = ret_long - ret_short,
    date = as.Date(date)
  ) %>%
  select(date, characteristic, factor_ret) %>%
  na.omit()

# RENAME CHARACTERISTICS TO READABLE NAMES
# We swap the JKP codes (e.g., 'be_me') for your names (e.g., 'Book_to_Market_HML')
inverse_map <- setNames(names(factor_rename_map), factor_rename_map)
daily_factors$characteristic <- inverse_map[daily_factors$characteristic]

# Pivot for Strategy
daily_factors_wide <- daily_factors %>%
  pivot_wider(names_from = characteristic, values_from = factor_ret) %>%
  arrange(date)

cat(
  "   Dimensions:",
  nrow(daily_factors_wide),
  "days x",
  ncol(daily_factors_wide) - 1,
  "factors\n"
)
cat(
  "   Factors included:",
  paste(names(daily_factors_wide)[-1], collapse = ", "),
  "\n"
)

# ------------------------------------------------------------------------------
# 3. DEFINE STRATEGY FUNCTION
# ------------------------------------------------------------------------------
calculate_momentum_strategy <- function(
  df,
  target_cols,
  lookback_periods = 21,
  holding_periods = 21,
  signal_lag = 1,
  long_quantile = 0.5,
  short_quantile = 0.5
) {
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) {
    return(NULL)
  }

  cat("   Calculating Signals on", length(target_cols_exist), "factors...\n")

  # 1. Signals (Rolling 21-day return)
  momentum_signals <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    mutate(across(
      all_of(target_cols_exist),
      \(x) {
        rollapply(
          log(1 + x),
          width = lookback_periods,
          FUN = sum,
          fill = NA,
          align = "right"
        )
      },
      .names = "{.col}_signal"
    )) %>%
    mutate(across(ends_with("_signal"), \(x) lag(x, n = signal_lag))) %>%
    select(date, ends_with("_signal"))

  # 2. Forward Returns (Next 21-day return)
  forward_returns <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    mutate(across(
      all_of(target_cols_exist),
      \(x) {
        rollapply(
          log(1 + x),
          width = holding_periods,
          FUN = sum,
          fill = NA,
          align = "left"
        )
      },
      .names = "{.col}_fwd_ret"
    )) %>%
    select(date, ends_with("_fwd_ret"))

  combined_data <- inner_join(
    momentum_signals,
    forward_returns,
    by = "date"
  ) %>%
    na.omit()

  # 3. Monthly Rebalancing
  rebal_dates <- combined_data$date[seq(
    1,
    nrow(combined_data),
    by = holding_periods
  )]
  strategy_df <- combined_data %>% filter(date %in% rebal_dates)

  # 4. Returns Calculation
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
      returns = pmap(
        list(signals, fwd_rets, q_short, q_long),
        function(s, f, qs, ql) {
          long_indices <- if (long_quantile == short_quantile) {
            which(s > ql)
          } else {
            which(s >= ql)
          }
          short_indices <- which(s <= qs)

          long_ret <- if (length(long_indices) > 0) {
            mean(f[long_indices], na.rm = TRUE)
          } else {
            0
          }
          short_ret <- if (length(short_indices) > 0) {
            mean(f[short_indices], na.rm = TRUE)
          } else {
            0
          }

          tibble(
            long_return = exp(long_ret) - 1,
            short_return = exp(short_ret) - 1,
            long_short_return = (exp(long_ret) - 1) - (exp(short_ret) - 1)
          )
        }
      )
    ) %>%
    select(date, returns) %>%
    unnest(returns)

  return(strategy_returns)
}

# ------------------------------------------------------------------------------
# 4. RUN STRATEGY (Core Factors Only)
# ------------------------------------------------------------------------------
cat("--- 3. Running Strategy on Core Factors ---\n")

factor_cols <- colnames(daily_factors_wide)[-1]

# Apply strict Monthly Rebalancing (21d) logic
core_mom_results <- calculate_momentum_strategy(
  daily_factors_wide,
  factor_cols,
  lookback_periods = 21,
  holding_periods = 21,
  signal_lag = 1
)

# ------------------------------------------------------------------------------
# 5. PLOT RESULTS
# ------------------------------------------------------------------------------
cat("--- 4. Generating Plot ---\n")

plot_data_ls <- core_mom_results %>%
  select(date, long_return, short_return, long_short_return) %>%
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Return") %>%
  group_by(Strategy) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

start_rows <- tibble(
  date = min(plot_data_ls$date) - 1,
  Strategy = unique(plot_data_ls$Strategy),
  Return = 0,
  Cumulative_Return = 1.0
)
plot_data_final <- bind_rows(start_rows, plot_data_ls) %>%
  arrange(Strategy, date)

p <- ggplot(
  plot_data_final,
  aes(x = date, y = Cumulative_Return, color = Strategy)
) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(
    values = c(
      "long_return" = "darkgreen",
      "short_return_inv" = "darkred",
      "long_short_return" = "blue"
    ),
    labels = c("Long Winners", "Factor Mom (L/S)", "Short Losers (Inv)")
  ) +
  labs(
    title = "Factor Momentum (Core Factors Only)",
    subtitle = "Universe: 35 Selected Factors | Logic: 21d Lookback, 21d Holding",
    y = "Cumulative Return (Log Scale)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave(OUTPUT_PLOT, plot = p, width = 10, height = 6)
cat("\n✅ Core Factor Replication Complete! Plot saved to:", OUTPUT_PLOT, "\n")
