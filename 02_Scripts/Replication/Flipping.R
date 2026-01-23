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


################## ==============================================================================
# daily flipping

# ==============================================================================
# JKP DATA PIPELINE: CORE DAILY FACTORS & STRATEGY (WITH BENCHMARK VALIDATION)
# Purpose: Generate Daily L/S Returns, Validate against Benchmark, Run Momentum
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, arrow, lubridate, zoo, ggplot2, scales)

# --- CONFIGURATION ---
setwd(
  "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
)

# Inputs
DAILY_PFS_FILE <- "01_Data/Raw/pfs_daily.parquet"
# Update this filename if yours is different!
DAILY_BENCH_FILE <- "01_Data/Raw/[usa]_[all_factors]_[daily]_[vw_cap].csv"

# Outputs
OUTPUT_FILE <- "01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet"
OUTPUT_PLOT <- "03_Outputs/Figures/Core_Factor_Momentum_Daily.pdf"

# --- 0. DEFINE CORE FACTOR UNIVERSE (35 Factors) ---
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
# 1. LOAD & PROCESS DAILY DATA
# ==============================================================================
cat("--- 1. Loading and Filtering Daily Data ---\n")

# Load Daily Data (Filtered for USA and Core Factors)
jkp_daily <- read_parquet(DAILY_PFS_FILE) %>%
  filter(excntry == "USA") %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  filter(characteristic %in% factor_rename_map)

# Construct Daily High-Minus-Low
daily_factors_raw <- jkp_daily %>%
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
    raw_ret = ret_long - ret_short, # Always High - Low initially
    date = as.Date(date)
  ) %>%
  select(date, characteristic, raw_ret) %>%
  na.omit()

# ==============================================================================
# 2. VALIDATE WITH DAILY BENCHMARK (The "Truth Check")
# ==============================================================================
cat("--- 2. Validating Signs against Daily Benchmark ---\n")

if (file.exists(DAILY_BENCH_FILE)) {
  bench_daily <- read_csv(DAILY_BENCH_FILE, show_col_types = FALSE) %>%
    filter(freq == "daily", weighting == "vw_cap") %>%
    transmute(date = as.Date(date), characteristic = name, bench_ret = ret) %>%
    filter(characteristic %in% factor_rename_map)

  # Check Correlations
  diagnostic <- inner_join(
    daily_factors_raw,
    bench_daily,
    by = c("date", "characteristic")
  )

  diag_stats <- diagnostic %>%
    group_by(characteristic) %>%
    summarise(corr = cor(raw_ret, bench_ret)) %>%
    mutate(action = ifelse(corr < -0.5, "FLIP", "KEEP"))

  factors_to_flip <- diag_stats %>%
    filter(action == "FLIP") %>%
    pull(characteristic)
  cat(
    "   Found",
    length(factors_to_flip),
    "factors to flip based on daily benchmark.\n"
  )
} else {
  cat(
    "⚠️ Daily Benchmark file not found. Skipping validation (Assuming High-Low is correct).\n"
  )
  factors_to_flip <- c()
}

# ==============================================================================
# 3. FINALIZE & RENAME
# ==============================================================================
cat("--- 3. Finalizing Daily Factors ---\n")

final_daily_factors <- daily_factors_raw %>%
  mutate(
    final_ret = if_else(
      characteristic %in% factors_to_flip,
      -1 * raw_ret,
      raw_ret
    )
  ) %>%
  select(date, characteristic, factor_ret = final_ret)

# Rename to Readable Names
inverse_map <- setNames(names(factor_rename_map), factor_rename_map)
final_daily_factors$characteristic <- inverse_map[
  final_daily_factors$characteristic
]

# Save
write_parquet(final_daily_factors, OUTPUT_FILE)
cat("✅ Saved Validated Daily Factors to:", OUTPUT_FILE, "\n")

# ==============================================================================
# 4. RUN STRATEGY (21-Day Lookback / 21-Day Holding)
# ==============================================================================
cat("--- 4. Running Momentum Strategy ---\n")

# Prepare Wide Format
daily_wide <- final_daily_factors %>%
  pivot_wider(names_from = characteristic, values_from = factor_ret) %>%
  arrange(date)

# Strategy Function (Standard)
calculate_momentum_strategy <- function(
  df,
  target_cols,
  lookback_periods = 21,
  holding_periods = 21,
  signal_lag = 1,
  long_quantile = 0.5,
  short_quantile = 0.5
) {
  momentum_signals <- df %>%
    select(date, all_of(target_cols)) %>%
    mutate(across(
      all_of(target_cols),
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

  forward_returns <- df %>%
    select(date, all_of(target_cols)) %>%
    mutate(across(
      all_of(target_cols),
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

  # Monthly Rebalancing
  rebal_dates <- combined_data$date[seq(
    1,
    nrow(combined_data),
    by = holding_periods
  )]
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

# Run Strategy
core_factors <- names(daily_wide)[-1]
results <- calculate_momentum_strategy(
  daily_wide,
  core_factors,
  lookback_periods = 21,
  holding_periods = 21,
  signal_lag = 1
)

# ==============================================================================
# 5. PLOT
# ==============================================================================
plot_data <- results %>%
  select(date, long_return, short_return, long_short_return) %>%
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Return") %>%
  group_by(Strategy) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

# Add Start Point
plot_data <- bind_rows(
  tibble(
    date = min(plot_data$date) - 1,
    Strategy = unique(plot_data$Strategy),
    Return = 0,
    Cumulative_Return = 1.0
  ),
  plot_data
) %>%
  arrange(Strategy, date)

p <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(
    values = c(
      "long_return" = "darkgreen",
      "short_return_inv" = "darkred",
      "long_short_return" = "blue"
    ),
    labels = c("Long Winners", "Net Strategy (L/S)", "Short Losers (Inv)")
  ) +
  labs(
    title = "Daily Core Factor Momentum (Benchmarked)",
    subtitle = paste(
      "Universe: 35 Validated Factors | Lookback: 21 Days | Holding: 21 Days"
    ),
    y = "Cumulative Return (Log Scale)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
ggsave(OUTPUT_PLOT, plot = p, width = 10, height = 6)
cat("✅ Plot saved to:", OUTPUT_PLOT, "\n")
