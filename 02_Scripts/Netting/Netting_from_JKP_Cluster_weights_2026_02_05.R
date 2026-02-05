# -------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# -------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(lubridate)

# Input/Output Paths (Adjust if necessary)
input_path <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Processed/usa_factor_weights.parquet"
output_path <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Processed/usa_factor_weights_netted.parquet"

# -------------------------------------------------------------------------
# 2. DEFINE FACTOR LISTS
# -------------------------------------------------------------------------

# Factors where High Value = Short (or "Low minus High")
# e.g., Small Minus Big (Size), Low Inv Growth minus High Inv Growth
factors_to_flip <- c(
  "age",
  "aliq_at",
  "aliq_mat",
  "at_be",
  "at_gr1",
  "be_gr1a",
  "beta_60m",
  "beta_dimson_21d",
  "betabab_1260d",
  "betadown_252d",
  "capex_abn",
  "capx_gr1",
  "capx_gr2",
  "capx_gr3",
  "chcsho_12m",
  "coa_gr1a",
  "col_gr1a",
  "corr_1260d",
  "coskew_21d",
  "cowc_gr1a",
  "dbnetis_at",
  "debt_gr3",
  "dolvol_126d",
  "dolvol_var_126d",
  "dsale_drec",
  "earnings_variability",
  "emp_gr1",
  "eq_dur",
  "eqnetis_at",
  "fnl_gr1a",
  "inv_gr1",
  "inv_gr1a",
  "iskew_capm_21d",
  "iskew_ff3_21d",
  "iskew_hxz4_21d",
  "ivol_capm_21d",
  "ivol_capm_252d",
  "ivol_ff3_21d",
  "ivol_hxz4_21d",
  "lnoa_gr1a",
  "lti_gr1a",
  "market_equity",
  "ncoa_gr1a",
  "ncol_gr1a",
  "netdebt_me",
  "netis_at",
  "nncoa_gr1a",
  "noa_at",
  "noa_gr1a",
  "o_score",
  "oaccruals_at",
  "oaccruals_ni",
  "ocfq_saleq_std",
  "ppeinv_gr1a",
  "prc",
  "ret_1_0",
  "ret_60_12",
  "rmax1_21d",
  "rmax5_21d",
  "rmax5_rvol_21d",
  "rskew_21d",
  "rvol_21d",
  "sale_gr1",
  "sale_gr3",
  "saleq_gr1",
  "seas_11_15na",
  "seas_16_20na",
  "seas_2_5na",
  "seas_6_10na",
  "taccruals_at",
  "taccruals_ni",
  "turnover_126d",
  "turnover_var_126d"
)

# The specific subset for QFin Lab
target_factors <- c(
  "age",
  "ami_126d",
  "at_be",
  "at_gr1",
  "at_turnover",
  "be_me",
  "beta_60m",
  "betabab_1260d",
  "capex_abn",
  "capx_gr1",
  "chcsho_12m",
  "cowc_gr1a",
  "dbnetis_at",
  "dolvol_126d",
  "dsale_dinv",
  "ebit_sale",
  "ebitda_mev",
  "f_score",
  "gp_at",
  "inv_gr1",
  "ivol_ff3_21d",
  "market_equity",
  "netis_at",
  "ni_be",
  "ni_me",
  "niq_at",
  "noa_at",
  "o_score",
  "oaccruals_at",
  "ocf_me",
  "ope_be",
  "prc",
  "qmj",
  "ret_1_0",
  "ret_12_1",
  "ret_60_12",
  "sale_gr1",
  "sale_me",
  "z_score"
)

# -------------------------------------------------------------------------
# 3. MAIN PROCESSING PIPELINE
# -------------------------------------------------------------------------
print("Loading and Processing Data...")

processed_data <- read_parquet(input_path) %>%
  # Filter for only the factors we care about
  filter(characteristic %in% target_factors) %>%
  mutate(
    # 1. Determine Direction: -1 if in 'factors_to_flip', else 1
    direction = if_else(characteristic %in% factors_to_flip, -1, 1),

    # 2. Calculate Final Net Leg
    # Logic:
    #   If leg is +1 (Top Bin) and direction is 1 (Positive factor) -> Long (+1)
    #   If leg is +1 (Top Bin/Big Cap) and direction is -1 (SMB) -> Short (-1)
    net_leg = leg * direction,

    # 3. Calculate Actual Trading Weight (Signed)
    # This is the value you sum up to get your portfolio position
    trading_weight = weight * net_leg
  ) %>%
  # Clean up columns (Keep only what is needed for analysis)
  select(id, eom, characteristic, weight, leg, net_leg, trading_weight)

# -------------------------------------------------------------------------
# 4. ROBUST SANITY CHECKS
# -------------------------------------------------------------------------
print("--- START SANITY CHECKS ---")

# CHECK 1: Market Equity (Size) -> Should be FLIPPED
# Top Bin (Leg 1) = Largest Companies -> Should be Short (net_leg -1)
print("1. Checking Size (SMB) - Top Bin (Big Caps):")
check_size <- processed_data %>%
  filter(characteristic == "market_equity", leg == 1) %>%
  head(5)
print(check_size)
# EXPECTED OUTPUT: leg = 1, net_leg = -1, trading_weight = negative

# CHECK 2: Gross Profitability -> Should NOT be flipped
# Top Bin (Leg 1) = High Profit -> Should be Long (net_leg 1)
print("2. Checking Gross Profitability - Top Bin (High Profit):")
check_gp <- processed_data %>%
  filter(characteristic == "gp_at", leg == 1) %>%
  head(5)
print(check_gp)
# EXPECTED OUTPUT: leg = 1, net_leg = 1, trading_weight = positive

# CHECK 3: Dollar Neutrality
# The sum of trading weights for any factor/date should be approx 0 (Long +100%, Short -100%)
print("3. Checking Dollar Neutrality (Sums should be near 0):")
latest_date <- max(processed_data$eom)
neutrality_check <- processed_data %>%
  filter(eom == latest_date) %>%
  group_by(characteristic) %>%
  summarise(
    net_exposure = sum(trading_weight),
    long_leg_sum = sum(trading_weight[trading_weight > 0]),
    short_leg_sum = sum(trading_weight[trading_weight < 0])
  ) %>%
  head(10)
print(neutrality_check)

# -------------------------------------------------------------------------
# 5. SAVE OUTPUT
# -------------------------------------------------------------------------
print(paste("Saving processed file to:", output_path))
write_parquet(processed_data, output_path)
print("Done!")

# -------------------------------------------------------------------------
#########################################################################
# -------------------------------------------------------------------------

# ==============================================================================
# JKP PHASE 2: FACTOR MOMENTUM -> SINGLE STOCK NETTING
# ==============================================================================
# Purpose:
# 1. Calculate Daily Factor Momentum Signals (21-day lookback)
# 2. Determine Strategy Allocations (Median L/S, Quartile L/S, etc.)
# 3. Merge with Single Stock Constituent Weights
# 4. Net out positions to create final Stock-Level Portfolios
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, arrow, lubridate, zoo, data.table)

# --- 1. SETUP & PATHS (Adjust relative to project root) ---
# Assuming running on cluster or local with same folder structure
DATA_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Processed/"
PFS_DAILY_FILE <- file.path(DATA_DIR, "USA_factors.parquet")
WEIGHTS_FILE <- file.path(DATA_DIR, "usa_factor_weights.parquet")
OUTPUT_DIR <- file.path(DATA_DIR, "intermediate") # Saving to intermediate first
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# --- 2. FACTOR DEFINITIONS ---

# Factors to FLIP (High = Short/Bad)
factors_to_flip <- c(
  "age",
  "aliq_at",
  "aliq_mat",
  "at_be",
  "at_gr1",
  "be_gr1a",
  "beta_60m",
  "beta_dimson_21d",
  "betabab_1260d",
  "betadown_252d",
  "capex_abn",
  "capx_gr1",
  "capx_gr2",
  "capx_gr3",
  "chcsho_12m",
  "coa_gr1a",
  "col_gr1a",
  "corr_1260d",
  "coskew_21d",
  "cowc_gr1a",
  "dbnetis_at",
  "debt_gr3",
  "dolvol_126d",
  "dolvol_var_126d",
  "dsale_drec",
  "earnings_variability",
  "emp_gr1",
  "eq_dur",
  "eqnetis_at",
  "fnl_gr1a",
  "inv_gr1",
  "inv_gr1a",
  "iskew_capm_21d",
  "iskew_ff3_21d",
  "iskew_hxz4_21d",
  "ivol_capm_21d",
  "ivol_capm_252d",
  "ivol_ff3_21d",
  "ivol_hxz4_21d",
  "lnoa_gr1a",
  "lti_gr1a",
  "market_equity",
  "ncoa_gr1a",
  "ncol_gr1a",
  "netdebt_me",
  "netis_at",
  "nncoa_gr1a",
  "noa_at",
  "noa_gr1a",
  "o_score",
  "oaccruals_at",
  "oaccruals_ni",
  "ocfq_saleq_std",
  "ppeinv_gr1a",
  "prc",
  "ret_1_0",
  "ret_60_12",
  "rmax1_21d",
  "rmax5_21d",
  "rmax5_rvol_21d",
  "rskew_21d",
  "rvol_21d",
  "sale_gr1",
  "sale_gr3",
  "saleq_gr1",
  "seas_11_15na",
  "seas_16_20na",
  "seas_2_5na",
  "seas_6_10na",
  "taccruals_at",
  "taccruals_ni",
  "turnover_126d",
  "turnover_var_126d"
)

# Target Core Factors
target_factors <- c(
  "age",
  "ami_126d",
  "at_be",
  "at_gr1",
  "at_turnover",
  "be_me",
  "beta_60m",
  "betabab_1260d",
  "capex_abn",
  "capx_gr1",
  "chcsho_12m",
  "cowc_gr1a",
  "dbnetis_at",
  "dolvol_126d",
  "dsale_dinv",
  "ebit_sale",
  "ebitda_mev",
  "f_score",
  "gp_at",
  "inv_gr1",
  "ivol_ff3_21d",
  "market_equity",
  "netis_at",
  "ni_be",
  "ni_me",
  "niq_at",
  "noa_at",
  "o_score",
  "oaccruals_at",
  "ocf_me",
  "ope_be",
  "prc",
  "qmj",
  "ret_1_0",
  "ret_12_1",
  "ret_60_12",
  "sale_gr1",
  "sale_me",
  "z_score"
)

# ------------------------------------------------------------------------------
# 3. CALCULATE STRATEGY WEIGHTS (The "Brain")
# ------------------------------------------------------------------------------
cat("--- 1. Computing Factor Momentum Signals ---\n")

# Load Daily Factor Returns
pfs <- read_parquet(PFS_DAILY_FILE) %>%
  filter(excntry == "USA", characteristic %in% target_factors) %>%
  select(date, characteristic, direction, ret = ret_vw_cap) %>%
  collect()

# Create Long-Short Factor Returns (High Bin - Low Bin)
factor_rets <- pfs %>%
  group_by(date, characteristic) %>%
  summarise(
    ret_long = ret[direction == max(direction)],
    ret_short = ret[direction == min(direction)],
    factor_ret = ret_long - ret_short,
    .groups = "drop"
  ) %>%
  arrange(characteristic, date)

# Calculate 21-Day Momentum Signal (Lagged by 1 day)
signals <- factor_rets %>%
  group_by(characteristic) %>%
  mutate(
    log_ret = log(1 + factor_ret),
    # 21-day cumulative return
    raw_signal = rollsum(log_ret, k = 21, fill = NA, align = "right"),
    # Lag 1 day to avoid look-ahead bias
    mom_signal = lag(raw_signal, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(mom_signal))

# --- Determine Strategy Weights at Month-End ---
# We align with the constituent weights which are monthly (EOM)
monthly_strategy <- signals %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(characteristic, month) %>%
  filter(date == max(date)) %>% # Take the signal at the last trading day
  ungroup() %>%
  # IMPORTANT: The Constituent File is dated EOM.
  # If we have weights for 2024-01-31, we trade them in Feb.
  # Our signal calculated on 2024-01-31 determines the Feb portfolio.
  # So we match on EOM dates.
  mutate(rebal_date = ceiling_date(date, "month") - days(1)) %>%
  select(rebal_date, characteristic, mom_signal)

# Assign Weights for the 4 Strategies
strategy_weights <- monthly_strategy %>%
  group_by(rebal_date) %>%
  mutate(
    rank = rank(mom_signal),
    n = n(),

    # 1. Median L/S (Top 50% Long, Bottom 50% Short)
    w_median_ls = case_when(
      rank > n / 2 ~ 1 / (n / 2),
      rank <= n / 2 ~ -1 / (n / 2),
      TRUE ~ 0
    ),

    # 2. Quartile L/S (Top 25% Long, Bottom 25% Short)
    n_quartile = floor(n * 0.25),
    w_quartile_ls = case_when(
      rank > (n - n_quartile) ~ 1 / n_quartile,
      rank <= n_quartile ~ -1 / n_quartile,
      TRUE ~ 0
    ),

    # 3. Median Long-Only (Top 50% Long)
    w_median_lo = case_when(
      rank > n / 2 ~ 1 / (n / 2),
      TRUE ~ 0
    ),

    # 4. Quartile Long-Only (Top 25% Long)
    w_quartile_lo = case_when(
      rank > (n - n_quartile) ~ 1 / n_quartile,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  select(rebal_date, characteristic, starts_with("w_"))

cat(
  "   Signals generated for",
  n_distinct(strategy_weights$rebal_date),
  "months.\n"
)

# ------------------------------------------------------------------------------
# 4. PROCESS CONSTITUENT WEIGHTS (The "Body")
# ------------------------------------------------------------------------------
cat("--- 2. Processing Single Stock Weights ---\n")

# We process this in chunks or using data.table if memory is tight,
# but for 40 factors/USA only, standard RAM usually suffices.

raw_weights <- read_parquet(WEIGHTS_FILE) %>%
  filter(characteristic %in% target_factors) %>%
  select(id, eom, characteristic, weight, leg) %>%
  collect()

# Convert to data.table for speed
setDT(raw_weights)

# 1. Flip Signs (Construct the Factor itself)
# Factor_Weight = (Weight * Leg * Direction)
# e.g., Size Factor: Small Caps (Leg -1) * Flip (-1) = +1 (Long)
raw_weights[, direction := ifelse(characteristic %in% factors_to_flip, -1L, 1L)]
raw_weights[, stock_factor_weight := weight * leg * direction]

# ------------------------------------------------------------------------------
# 5. MERGE AND NET (The "Merger")
# ------------------------------------------------------------------------------
cat("--- 3. Merging and Netting Positions ---\n")

# Convert strategy weights to DT
setDT(strategy_weights)

# Merge
# inner_join on Date + Characteristic
combined <- merge(
  raw_weights,
  strategy_weights,
  by.x = c("eom", "characteristic"),
  by.y = c("rebal_date", "characteristic"),
  all = FALSE
) # Only keep factors we have signals for

# Netting Logic:
# Net Weight Stock i = Sum_factors ( Stock_Weight_in_Factor * Strategy_Weight_in_Factor )
final_portfolio <- combined[,
  .(
    net_w_median_ls = sum(stock_factor_weight * w_median_ls),
    net_w_quartile_ls = sum(stock_factor_weight * w_quartile_ls),
    net_w_median_lo = sum(stock_factor_weight * w_median_lo),
    net_w_quartile_lo = sum(stock_factor_weight * w_quartile_lo),
    n_factors_active = uniqueN(characteristic)
  ),
  by = .(eom, id)
]

# ------------------------------------------------------------------------------
# 6. SAVE OUTPUT
# ------------------------------------------------------------------------------
cat("--- 4. Saving Final Portfolios ---\n")

output_file <- file.path(OUTPUT_DIR, "final_strategy_stock_weights.parquet")
write_parquet(final_portfolio, output_file)

cat("Success! File saved to:", output_file, "\n")
cat("Rows:", nrow(final_portfolio), "\n")
cat("Columns:", colnames(final_portfolio), "\n")

# ------------------------------------------------------------------------------
# 7. QUICK INSPECTION
# ------------------------------------------------------------------------------
print("Top Holdings (Median L/S) - Latest Date")
final_portfolio[eom == max(eom)][order(-abs(net_w_median_ls))][1:10] %>% print()

print("Dollar Neutrality Check (Sums):")
final_portfolio[
  eom == max(eom),
  .(
    Sum_Median_LS = sum(net_w_median_ls),
    Sum_Quartile_LS = sum(net_w_quartile_ls),
    Sum_Quartile_LO = sum(net_w_quartile_lo)
  )
] %>%
  print()


# ==============================================================================
# JKP PHASE 3: SINGLE STOCK BACKTEST & PERFORMANCE
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, arrow, lubridate, ggplot2, scales, data.table)

# --- 1. SETUP & PATHS ---
# Adjust these to your local paths
BASE_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
WEIGHTS_FILE <- file.path(
  BASE_DIR,
  "01_Data/Processed/intermediate/final_strategy_stock_weights.parquet"
)

# *** UPDATE THIS PATH TO YOUR STOCK RETURNS FILE ***
# This file must have columns: id, eom, ret (or ret_exc)
RETURNS_FILE <- file.path(BASE_DIR, "01_Data/Processed/USA_factors.parquet")

# --- 2. LOAD DATA ---
cat("--- 1. Loading Weights and Returns ---\n")

# Load Strategy Weights (Positions determined at t-1)
weights <- read_parquet(WEIGHTS_FILE) %>%
  mutate(date = as.Date(eom)) %>%
  select(-eom)

# Load Stock Returns (Realized at t)
# Filter for USA and the stocks we actually hold to save RAM
stock_ids <- unique(weights$id)

rets <- read_parquet(RETURNS_FILE)
head(rets)


rets <- read_parquet(RETURNS_FILE) %>%
  filter(excntry == "USA") %>% # Ensure USA only
  filter(id %in% stock_ids) %>%
  mutate(date = as.Date(eom)) %>%
  select(id, date, ret = ret_exc) # Using Excess Returns

# --- 3. ALIGNMENT (THE "LAG") ---
# Strategy Logic: Weights calculated at end of Jan (t-1) generate returns in Feb (t)
cat("--- 2. Aligning Weights (t-1) with Returns (t) ---\n")

# Create 'match_date' in weights to align with returns
# If weight date is Jan 31, match_date becomes Feb (aligned to next month end)
weights_aligned <- weights %>%
  mutate(
    # Move weight date forward to the trading month
    # e.g., 1990-01-31 -> 1990-02-01 -> 1990-02-28 (roughly)
    # We essentially join on: Return Date = Weight Date + 1 Month
    match_month = floor_date(date, "month") + months(1)
  )

rets_aligned <- rets %>%
  mutate(
    match_month = floor_date(date, "month")
  )

# Merge
# We use inner_join because we can only calculate returns if we have both weight and return
portfolio_data <- weights_aligned %>%
  inner_join(rets_aligned, by = c("id", "match_month"))

# --- 4. CALCULATE PORTFOLIO RETURNS ---
cat("--- 3. Calculating Monthly Strategy Returns ---\n")

setDT(portfolio_data)

# Calculate weighted return for each strategy per month
monthly_perf <- portfolio_data[,
  .(
    # Gross Returns (before t-costs)
    ret_median_ls = sum(net_w_median_ls * ret, na.rm = TRUE),
    ret_quartile_ls = sum(net_w_quartile_ls * ret, na.rm = TRUE),
    ret_quartile_lo = sum(net_w_quartile_lo * ret, na.rm = TRUE),

    # Count stocks to ensure we aren't trading on empty data
    n_stocks = .N
  ),
  by = .(date = date.y)
] # Use return date

# --- 5. VISUALIZATION ---
cat("--- 4. Plotting Performance ---\n")

# Transform for ggplot
plot_data <- monthly_perf %>%
  arrange(date) %>%
  mutate(
    Cum_Median_LS = cumprod(1 + ret_median_ls),
    Cum_Quartile_LS = cumprod(1 + ret_quartile_ls),
    Cum_Quartile_LO = cumprod(1 + ret_quartile_lo)
  ) %>%
  select(date, starts_with("Cum_")) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Wealth") %>%
  mutate(Strategy = str_replace(Strategy, "Cum_", ""))

# Plot
p <- ggplot(plot_data, aes(x = date, y = Wealth, color = Strategy)) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(
    values = c(
      "Median_LS" = "#2C3E50", # Dark Blue
      "Quartile_LS" = "#E74C3C", # Red
      "Quartile_LO" = "#27AE60" # Green
    )
  ) +
  labs(
    title = "Single-Stock Factor Momentum (USA)",
    subtitle = "Gross Performance (No T-Costs) | Replicated from Constituents",
    y = "Cumulative Wealth (Log Scale)",
    x = "",
    caption = "Strategies: Median (Top 50-Bottom 50), Quartile (Top 25-Bottom 25), Quartile LO (Top 25 Only)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

# --- 6. STATISTICS TABLE ---
# Quick stats for the console
stats <- monthly_perf %>%
  pivot_longer(
    cols = starts_with("ret_"),
    names_to = "Strategy",
    values_to = "Ret"
  ) %>%
  group_by(Strategy) %>%
  summarise(
    Ann_Return = mean(Ret, na.rm = T) * 12,
    Ann_Vol = sd(Ret, na.rm = T) * sqrt(12),
    Sharpe = (mean(Ret, na.rm = T) * 12) / (sd(Ret, na.rm = T) * sqrt(12)),
    Max_DD = max(1 - cumprod(1 + Ret) / cummax(cumprod(1 + Ret)))
  ) %>%
  mutate(across(where(is.numeric), round, 4))

print(as.data.frame(stats))
