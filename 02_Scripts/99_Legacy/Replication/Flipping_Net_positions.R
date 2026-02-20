# ==============================================================================
# JKP DATA PIPELINE: CORE FACTOR GENERATION & SINGLE STOCK POSITIONS
# Purpose: Exact replication of Code 1 + Single Stock Logic (Sec 6)
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, arrow, lubridate, gridExtra, grid, zoo, scales, data.table)

# --- CONFIGURATION ---
setwd("C:/WU/ILab ZZ/QFin_ILab_V_2025")

# Inputs
BENCH_FILE       <- "01_Data/Raw/[usa]_[all_factors]_[monthly]_[vw_cap].csv"
REPL_FILE        <- "01_Data/Raw/pfs.parquet"
DAILY_PFS_FILE   <- "01_Data/Raw/pfs_daily.parquet"
DAILY_BENCH_FILE <- "01_Data/Raw/[usa]_[all_factors]_[daily]_[vw_cap].csv"
UNIVERSE_FILE    <- "01_Data/Raw/USA.parquet"

# Outputs
OUTPUT_FILE      <- "01_Data/Processed/USA_Valid_Factor_Returns.parquet"
OUTPUT_DAILY     <- "01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet"
OUTPUT_PLOT      <- "03_Outputs/Figures/Core_Factor_Momentum_Daily.pdf"

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
# 1. LOAD DATA & CONSTRUCT INITIAL PORTFOLIOS (MONTHLY)
# ==============================================================================
cat("--- 1. Loading and Constructing Portfolios (Monthly) ---\n")

# Load Benchmark
bench <- read_csv(BENCH_FILE, show_col_types = FALSE) %>%
  filter(freq == "monthly", weighting == "vw_cap") %>%
  transmute(date = as.Date(date), characteristic = name, bench_ret = ret)

# Load Replication Data
repl_raw <- read_parquet(REPL_FILE)

# Construct Long-Short
repl_ls <- repl_raw %>%
  filter(excntry == "USA") %>%
  select(date = eom, characteristic, pf, ret = ret_vw_cap) %>%
  group_by(date, characteristic) %>%
  mutate(
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
    raw_ret = ret_long - ret_short, 
    date = as.Date(date)
  ) %>%
  na.omit()

# ==============================================================================
# 2. DIAGNOSTIC PHASE: IDENTIFY SIGN FLIPS
# ==============================================================================
cat("--- 2. Diagnosing Factor Directions (Correlation Check) ---\n")

diagnostic <- inner_join(repl_ls, bench, by = c("date", "characteristic"))

diag_stats <- diagnostic %>%
  group_by(characteristic) %>%
  summarise(corr_raw = cor(raw_ret, bench_ret, use = "complete.obs")) %>%
  mutate(action = case_when(corr_raw < -0.50 ~ "Flip", TRUE ~ "Keep"))

factors_to_flip_monthly <- diag_stats %>%
  filter(action == "Flip") %>%
  pull(characteristic)

cat("Found", length(factors_to_flip_monthly), "factors that need sign flipping (Monthly).\n")

# ==============================================================================
# 3. CORRECTION & FILTERING PHASE
# ==============================================================================
cat("--- 3. Applying Corrections and Filtering for CORE 35 ---\n")

core_map_df <- tibble(
  characteristic = unname(factor_rename_map),
  Readable_Name = names(factor_rename_map)
)

final_data <- repl_ls %>%
  mutate(
    final_ret = if_else(characteristic %in% factors_to_flip_monthly, -1 * raw_ret, raw_ret),
    direction = if_else(characteristic %in% factors_to_flip_monthly, "Low Minus High", "High Minus Low")
  ) %>%
  inner_join(core_map_df, by = "characteristic") %>%
  select(date, characteristic, Readable_Name, direction, final_ret) %>%
  arrange(characteristic, date)

write_parquet(final_data, OUTPUT_FILE)
cat("SUCCESS: Cleaned Core Factor Data saved to", OUTPUT_FILE, "\n")

# ==============================================================================
# 4. DAILY FACTOR PROCESSING & STRATEGY
# ==============================================================================
cat("\n--- 4. Processing Daily Data & Strategy ---\n")

# Load Daily Data
jkp_daily <- read_parquet(DAILY_PFS_FILE) %>%
  filter(excntry == "USA") %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  filter(characteristic %in% factor_rename_map)

# Construct Daily HML
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
    raw_ret = ret_long - ret_short, 
    date = as.Date(date)
  ) %>%
  select(date, characteristic, raw_ret) %>%
  na.omit()

# Validate with Daily Benchmark
if (file.exists(DAILY_BENCH_FILE)) {
  bench_daily <- read_csv(DAILY_BENCH_FILE, show_col_types = FALSE) %>%
    filter(freq == "daily", weighting == "vw_cap") %>%
    transmute(date = as.Date(date), characteristic = name, bench_ret = ret) %>%
    filter(characteristic %in% factor_rename_map)

  diagnostic_daily <- inner_join(daily_factors_raw, bench_daily, by = c("date", "characteristic"))
  
  factors_to_flip_daily <- diagnostic_daily %>%
    group_by(characteristic) %>%
    summarise(corr = cor(raw_ret, bench_ret)) %>%
    filter(corr < -0.5) %>%
    pull(characteristic)
    
  cat("   Found", length(factors_to_flip_daily), "daily factors to flip.\n")
} else {
  cat("Daily Benchmark file not found. Skipping validation.\n")
  factors_to_flip_daily <- c()
}

# Finalize Daily Factors
final_daily_factors <- daily_factors_raw %>%
  mutate(final_ret = if_else(characteristic %in% factors_to_flip_daily, -1 * raw_ret, raw_ret)) %>%
  select(date, characteristic, factor_ret = final_ret)

# Rename to Readable Names
inverse_map <- setNames(names(factor_rename_map), factor_rename_map)
final_daily_factors$characteristic <- inverse_map[final_daily_factors$characteristic]

write_parquet(final_daily_factors, OUTPUT_DAILY)

# ==============================================================================
# 5. RUN STRATEGY (EXACT CODE 1 LOGIC)
# ==============================================================================
cat("--- 5. Running Momentum Strategy ---\n")

daily_wide <- final_daily_factors %>%
  pivot_wider(names_from = characteristic, values_from = factor_ret) %>%
  arrange(date)

# ORIGINAL FUNCTION FROM CODE 1
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

# Plotting Logic (Matched EXACTLY to Code 1)
plot_data <- results %>%
  select(date, long_return, short_return, long_short_return) %>%
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Return") %>%
  group_by(Strategy) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

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
cat("Plot saved to:", OUTPUT_PLOT, "\n")


# ==============================================================================
# 6. DERIVE SINGLE STOCK NET POSITIONS (LATEST REBALANCE)
# ==============================================================================
cat("\n--- 6. Deriving Net Single Stock Positions ---\n")

# --- Configuration ---
uni_file     <- "01_Data/Raw/USA.parquet"
cutoffs_file <- "01_Data/Raw/nyse_cutoffs.parquet"

# A. DATE SNAPPING: Align Strategy Date with Available Universe Data
# 1. Get the latest date from your strategy results
strategy_latest <- max(results$date)

# 2. Get the available dates from the Universe file (USA.parquet)
#    We use distinct() on the dataset to avoid loading the whole file.
available_dates <- open_dataset(uni_file) %>% 
  select(date) %>% 
  distinct() %>% 
  collect() %>% 
  pull(date) %>% 
  as.Date()

# 3. Find the latest Universe date that is ON or BEFORE the strategy end date
target_date <- max(available_dates[available_dates <= strategy_latest])

if (is.na(target_date) || is.infinite(target_date)) {
  stop("Error: No overlapping dates found between Strategy Results and Universe File.")
}

cat("Requested Strategy Date: ", as.character(strategy_latest), "\n")
cat("Snapped to Universe Date:", as.character(target_date), "\n")

# B. RECOVER STRATEGY SIGNALS
# We need to know which factors the strategy is Long/Short on this specific date.
factor_signals <- daily_wide %>%
  filter(date <= target_date) %>%
  arrange(date) %>%
  tail(252) %>%
  select(-date) %>%
  # Re-calculate the momentum signal (sum of log returns)
  mutate(across(everything(), ~ rollapply(log(1 + .), width = 21, FUN = sum, fill = NA, align = "right"))) %>%
  slice_tail(n = 1) %>% 
  pivot_longer(cols = everything(), names_to = "characteristic", values_to = "signal")

# Determine Strategy Direction (Long Winners / Short Losers)
long_threshold  <- quantile(factor_signals$signal, 0.5, na.rm = TRUE)
short_threshold <- quantile(factor_signals$signal, 0.5, na.rm = TRUE)

strategy_weights <- factor_signals %>%
  mutate(
    strat_position = case_when(
      signal >= long_threshold ~ 1,   # Strategy Buys this Factor
      signal <= short_threshold ~ -1, # Strategy Shorts this Factor
      TRUE ~ 0
    )
  ) %>%
  filter(strat_position != 0)

cat("Strategy is active in", nrow(strategy_weights), "factors.\n")

# C. LOAD STOCK UNIVERSE & CUTOFFS
# Now we are guaranteed to find data because we snapped 'target_date' to 'available_dates'
usa_snapshot <- open_dataset(uni_file) %>%
  filter(date == target_date) %>%
  select(id, excntry, me, all_of(unname(factor_rename_map))) %>%
  collect()

# For NYSE Cutoffs, we match the Year-Month of the target_date
# (Cutoffs use 'eom' which might differ by 1-2 days from 'date' due to weekends)
target_month <- floor_date(target_date, "month")
nyse_breaks <- read_parquet(cutoffs_file) %>%
  mutate(month_date = floor_date(as.Date(eom), "month")) %>%
  filter(month_date == target_month) %>%
  select(-month_date)

# D. CONSTRUCT STOCK WEIGHTS
stock_weights_list <- list()

cat("Processing single stock weights...\n")

for (i in seq_len(nrow(strategy_weights))) {
  
  # 1. Identify Factor and Strategy Direction
  fact_name <- strategy_weights$characteristic[i]
  strat_dir <- strategy_weights$strat_position[i]
  raw_col   <- factor_rename_map[fact_name]
  
  # 2. Check for Sign Flip (from Section 2/3)
  is_flipped <- fact_name %in% names(factor_rename_map[factor_rename_map %in% factors_to_flip_monthly])
  
  # 3. Get Data
  factor_data <- usa_snapshot %>%
    select(id, me, signal_val = all_of(raw_col)) %>%
    filter(!is.na(signal_val), !is.na(me))
  
  # 4. Handle Breakpoints
  # If NYSE breaks exist for this month, use them. Otherwise default to 20/80 quantile.
  if (nrow(nyse_breaks) > 0) {
    # We generally use the p20 and p80 columns from the cutoffs file
    # Note: Ensure the file actually has p20/p80 columns. If not, fallback to quantile.
    if ("nyse_p20" %in% names(nyse_breaks) && "nyse_p80" %in% names(nyse_breaks)) {
      # Use the MEAN of the breakpoints if there are multiple rows (rare, but safe)
      b_low  <- mean(nyse_breaks$nyse_p20, na.rm = TRUE)
      b_high <- mean(nyse_breaks$nyse_p80, na.rm = TRUE)
    } else {
      # Fallback if specific columns missing
      b <- quantile(factor_data$signal_val, probs = c(0.2, 0.8), na.rm = TRUE)
      b_low <- b[1]; b_high <- b[2]
    }
  } else {
    b <- quantile(factor_data$signal_val, probs = c(0.2, 0.8), na.rm = TRUE)
    b_low <- b[1]; b_high <- b[2]
  }

  # 5. Assign Legs & Weights
  factor_data <- factor_data %>%
    mutate(
      leg = case_when(
        signal_val >= b_high ~ 1,  # High Characteristic
        signal_val <= b_low ~ -1,  # Low Characteristic
        TRUE ~ 0
      )
    ) %>%
    filter(leg != 0)
  
  # Apply Flip Logic
  if (is_flipped) factor_data$leg <- -1 * factor_data$leg
  
  # Calculate Weight
  factor_weights <- factor_data %>%
    group_by(leg) %>%
    mutate(weight_in_leg = me / sum(me)) %>%
    ungroup() %>%
    mutate(
      net_factor_weight = strat_dir * leg * weight_in_leg
    ) %>%
    select(id, net_factor_weight)
  
  stock_weights_list[[i]] <- factor_weights
}

# E. AGGREGATE FINAL POSITIONS
final_positions <- bind_rows(stock_weights_list) %>%
  group_by(id) %>%
  summarise(
    total_weight = sum(net_factor_weight),
    n_factors_active = n_distinct(net_factor_weight)
  ) %>%
  arrange(desc(total_weight)) %>%
  mutate(normalized_weight = total_weight / sum(abs(total_weight)))

cat("Calculated positions for", nrow(final_positions), "unique stocks.\n")
print(head(final_positions, 10))

# Save
write_parquet(final_positions, "01_Data/Processed/Final_Stock_Positions.parquet")
cat("SUCCESS: Stock positions saved to 01_Data/Processed/Final_Stock_Positions.parquet\n")