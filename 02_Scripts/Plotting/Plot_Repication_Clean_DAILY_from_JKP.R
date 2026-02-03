# ==============================================================================
# JKP REPLICATION: REAL-WORLD FACTOR MOMENTUM (1-DAY DELAY)
# Purpose: Replicate Arnott et al. / Ehsani & Linnainmaa using JKP Core Factors
#          with realistic trading constraints (1-day skip).
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, lubridate, zoo, ggplot2, scales, ggthemes)

# --- CONFIGURATION ---
setwd("/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code")
DAILY_PFS_FILE <- "01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet"
OUTPUT_PLOT    <- "03_Outputs/Figures/JKP_Momentum_RealWorld_Replication.pdf"


# ------------------------------------------------------------------------------
# 2. LOAD & PREPARE JKP DATA
# ------------------------------------------------------------------------------
cat("--- 1. Processing Daily JKP Data (Filtered for Core Factors) ---\n")

daily_factors <- read_parquet(DAILY_PFS_FILE)



# Pivot to Wide (Format required for Strategy Function)
daily_factors_wide <- daily_factors %>%
  pivot_wider(names_from = characteristic, values_from = factor_ret) %>%
  arrange(date)
head(daily_factors_wide)
summary(daily_factors_wide)
tail(daily_factors_wide)
cat("   Data Ready: ", nrow(daily_factors_wide), "days x", ncol(daily_factors_wide)-1, "factors\n")

# ------------------------------------------------------------------------------
# 3. DEFINE STRATEGY FUNCTION (With 1-Day Delay Logic)
# ------------------------------------------------------------------------------
calculate_momentum_real_world <- function(df, target_cols, 
                                          lookback_days = 21, 
                                          holding_days = 21,
                                          implementation_lag = 1) { # 1 Day Skip
  
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) return(NULL)
  
  # Note: 
  # lag = 1 implies trading at Close of Day T to capture Return of Day T+1.
  # lag = 2 implies trading at Close of Day T+1 to capture Return of Day T+2 (1 Day Skip).
  # We use (1 + implementation_lag) to represent the total lag relative to the signal date.
  total_lag <- 1 + implementation_lag
  
  cat("   Calculating Strategy: Lookback=", lookback_days, 
      "| Holding=", holding_days, 
      "| Skip Days=", implementation_lag, "\n")
  
  momentum_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    
    # 1. Calculate Signal: Rolling Sum Log Return
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1 + x), width = lookback_days, FUN = sum, fill = NA, align = "right"), 
                  .names = "{.col}_signal")) %>%
    
    # 2. Lag Signal (Account for Implementation Delay)
    mutate(across(ends_with("_signal"), \(x) lag(x, n = total_lag))) %>%
    
    # 3. Forward Returns (Next 21 days)
    mutate(across(all_of(target_cols_exist), 
                  \(x) rollapply(log(1 + x), width = holding_days, FUN = sum, fill = NA, align = "left"), 
                  .names = "{.col}_fwd")) %>%
    
    # 4. Strict Monthly Rebalancing
    # We find the first valid date and then jump by 'holding_days'
    na.omit() 
  
  # Extract rebalancing dates from the valid dataset
  # Note: We take every K-th row to simulate holding the portfolio for K days
  rebal_indices <- seq(1, nrow(momentum_df), by = holding_days)
  momentum_df <- momentum_df[rebal_indices, ]
  
  # 5. Portfolio Construction (Cross-Sectional Median Split)
  strategy_returns <- momentum_df %>%
    rowwise() %>%
    mutate(
      median_signal = median(c_across(ends_with("_signal")), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Assign Positions: Long (> Median), Short (<= Median)
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
    # Calculate Weights (Equal Weight 1/N)
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
      # 6. Calculate Net Portfolio Return
      # Sum(Weight * Simple Return)
      momentum_return = sum(
        c_across(ends_with("_wgt")) * (exp(c_across(ends_with("_fwd"))) - 1), 
        na.rm = TRUE
      ),
      # Calculate Long Leg Return
      long_leg_return = sum(
        (c_across(ends_with("_pos")) == 1) * (1/n_long) * (exp(c_across(ends_with("_fwd"))) - 1),
        na.rm = TRUE
      ),
      # Calculate Short Leg Return
      short_leg_return = sum(
        (c_across(ends_with("_pos")) == -1) * (1/n_short) * (exp(c_across(ends_with("_fwd"))) - 1),
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return, long_leg_return, short_leg_return)
  
  return(strategy_returns)
}

# ------------------------------------------------------------------------------
# 4. RUN STRATEGY
# ------------------------------------------------------------------------------
factor_cols <- colnames(daily_factors_wide)[-1]

# Apply Real-World Logic: 21d Lookback, 1 Day Delay (Skip), 21d Holding
results_df <- calculate_momentum_real_world(
  daily_factors_wide, factor_cols, 
  lookback_days = 21, 
  holding_days = 21, 
  implementation_lag = 1 # SKIP 1 DAY (Real World)
)

# ------------------------------------------------------------------------------
# 5. PLOTTING (Long/Short Decomposition)
# ------------------------------------------------------------------------------
cat("--- 4. Generating Comparison Plot ---\n")

# Prepare Data for Plotting
plot_data_ls <- results_df %>%
  select(date, long_return = long_leg_return, short_return = short_leg_return, long_short_return = momentum_return) %>%
  mutate(short_return_inv = -short_return) %>% # Invert Short leg for visual comparison
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

# Generate Plot
p <- ggplot(plot_data_final, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  # Add Shaded Region for DotCom Bubble (as seen in original examples)
  annotate("rect", xmin = ymd("2000-03-01"), xmax = ymd("2002-10-01"), 
           ymin = 0.5, ymax = max(plot_data_final$Cumulative_Return),
           alpha = 0.1, fill = "gray50") +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma, breaks = c(1, 2, 5, 10, 20, 50, 100)) +
  scale_color_manual(
    values = c("long_return" = "darkgreen", "short_return_inv" = "darkred", "long_short_return" = "blue"),
    labels = c("Long-Only Leg", "Long-Short Strategy", "Short-Only Leg (Inverted)")
  ) +
  labs(title = "Factor Momentum: Real-World Replication (JKP Data)",
       subtitle = "Logic: 21d Lookback, 1-Day Wait, 21d Holding (Monthly Rebal)",
       y = "Cumulative Return (Log Scale, Start=1)",
       caption = "Data Source: JKP Daily Core Factors") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p)

# Save
ggsave(OUTPUT_PLOT, plot = p, width = 10, height = 6)
cat("\n✅ Real-World Replication Complete! Plot saved to:", OUTPUT_PLOT, "\n")

# Print Summary Stats
ann_factor <- 252 / 21 # Approx 12 periods per year
sharpe <- (mean(results_df$momentum_return) * ann_factor) / 
  (sd(results_df$momentum_return) * sqrt(ann_factor))
cat("Total Return:", format(tail(plot_data_final$Cumulative_Return, 1), digits=4), "x\n")
cat("Sharpe Ratio:", round(sharpe, 2), "\n")













# ==============================================================================
# JKP REPLICATION: REAL-WORLD FACTOR MOMENTUM (1-DAY DELAY)
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, lubridate, zoo, ggplot2, scales, ggthemes)

# --- 1. CONFIGURATION ---
setwd("/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code")
DAILY_PFS_FILE  <- "01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet"
UNIVERSE_FILE   <- "01_Data/Raw/USA.parquet"
METADATA_FILE   <- "01_Data/Processed/USA_Valid_Factor_Returns.parquet"

# Mapping Strategy names to USA.parquet signal columns
factor_rename_map <- c(
  "Size_SMB" = "market_equity", "Book_to_Market_HML" = "be_me",
  "Operating_Profitability_RMW" = "ope_be", "Asset_Growth_CMA" = "at_gr1",
  "Long_Term_Reversals_LTREV" = "ret_60_12", "Residual_Variance_RVAR" = "ivol_ff3_21d",
  "Quality_Minus_Junk_QMJ" = "qmj", "Low_Beta_BAB" = "betabab_1260d",
  "Amihud_Illiquidity" = "ami_126d", "Firm_Age" = "age",
  "Nominal_Price" = "prc", "High_Volume_Premium" = "dolvol_126d",
  "Gross_Profitability" = "gp_at", "Return_on_Equity" = "ni_be",
  "Return_on_Assets" = "niq_at", "Profit_Margin" = "ebit_sale",
  "Change_in_Asset_Turnover" = "at_turnover", "Accruals_Factor" = "oaccruals_at",
  "Net_Operating_Assets" = "noa_at", "Net_Working_Capital_Changes" = "cowc_gr1a",
  "Cash_Flow_to_Price" = "ocf_me", "Earnings_to_Price" = "ni_me",
  "Enterprise_Multiple" = "ebitda_mev", "Sales_to_Price" = "sale_me",
  "Growth_in_Inventory" = "inv_gr1", "Sales_Growth" = "sale_gr1",
  "Growth_in_Sales_Inventory" = "dsale_dinv", "Abnormal_Investment" = "capex_abn",
  "CAPX_Growth_Rate" = "capx_gr1", "Debt_Issuance_Factor" = "dbnetis_at",
  "Leverage_Factor" = "at_be", "One_Year_Share_Issuance" = "chcsho_12m",
  "Total_External_Financing" = "netis_at", "Ohlson_O_Score" = "o_score",
  "Altman_Z_Score" = "z_score", "Piotroski_F_Score" = "f_score"
)

# --- 2. DEFINE STRATEGY FUNCTION ---
calculate_momentum_final <- function(df, target_cols, lookback = 21, holding = 21, lag = 1) {
  total_lag <- 1 + lag
  res <- df %>%
    select(date, all_of(target_cols)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols), \(x) rollapply(log(1 + x), width = lookback, FUN = sum, fill = NA, align = "right"), .names = "{.col}_sig")) %>%
    mutate(across(ends_with("_sig"), \(x) lag(x, n = total_lag))) %>%
    mutate(across(all_of(target_cols), \(x) rollapply(log(1 + x), width = holding, FUN = sum, fill = NA, align = "left"), .names = "{.col}_fwd")) %>%
    na.omit()
  
  rebal_idx <- seq(1, nrow(res), by = holding)
  res <- res[rebal_idx, ]
  
  res <- res %>%
    rowwise() %>%
    mutate(median_sig = median(c_across(ends_with("_sig")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(all_of(target_cols), ~ case_when(
      get(paste0(cur_column(), "_sig")) >  median_sig ~  1,
      get(paste0(cur_column(), "_sig")) <= median_sig ~ -1,
      TRUE ~ 0
    ), .names = "{.col}_pos")) %>%
    rowwise() %>%
    mutate(n_long = sum(c_across(ends_with("_pos")) == 1),
           n_short = sum(c_across(ends_with("_pos")) == -1)) %>%
    ungroup() %>%
    mutate(across(all_of(target_cols), ~ case_when(
      get(paste0(cur_column(), "_pos")) ==  1 ~  1 / max(1, n_long),
      get(paste0(cur_column(), "_pos")) == -1 ~ -1 / max(1, n_short),
      TRUE ~ 0
    ), .names = "{.col}_wgt"))
  
  wgts <- as.matrix(res %>% select(ends_with("_wgt")))
  fwds <- as.matrix(exp(res %>% select(ends_with("_fwd"))) - 1)
  res$momentum_return <- rowSums(wgts * fwds, na.rm = TRUE)
  return(res)
}

# --- 3. RUN STRATEGY & LOOK-THROUGH ---
cat("--- Running Strategy and Stock Look-Through ---\n")
daily_factors_wide <- read_parquet(DAILY_PFS_FILE) %>% pivot_wider(names_from = characteristic, values_from = factor_ret) %>% arrange(date)
results_full <- calculate_momentum_final(daily_factors_wide, colnames(daily_factors_wide)[-1])

strat_weights <- results_full %>%
  select(date, ends_with("_wgt")) %>%
  pivot_longer(-date, names_to = "characteristic", values_to = "strat_wgt") %>%
  mutate(characteristic = sub("_wgt", "", characteristic),
         signal_col = factor_rename_map[characteristic],
         join_month = floor_date(date, "month")) %>%
  filter(strat_wgt != 0)

factor_meta <- read_parquet(METADATA_FILE) %>% select(characteristic, direction) %>% distinct()
stock_universe <- read_parquet(UNIVERSE_FILE) %>% filter(excntry == "USA")

# --- 1. PREPARE THE LOOP ---
cat("--- Starting Memory-Efficient Monthly Look-Through ---\n")

all_rebal_dates <- unique(strat_weights$date)
net_stock_positions_list <- list()

# Process each rebalancing date one by one to save RAM
for (i in seq_along(all_rebal_dates)) {
  
  target_date <- all_rebal_dates[i]
  rebal_month <- floor_date(target_date, "month")
  
  cat(sprintf("\rProcessing Month %d/%d: %s", i, length(all_rebal_dates), as.character(target_date)))
  
  # A. Filter strategy weights for this specific date
  current_weights <- strat_weights %>% filter(date == target_date)
  
  # B. Filter universe for this specific month-end
  current_universe <- stock_universe %>% 
    filter(floor_date(eom, "month") == rebal_month)
  
  if(nrow(current_universe) == 0) next
  
  # C. Process month-level data
  month_positions <- current_universe %>%
    pivot_longer(cols = any_of(na.omit(unique(factor_rename_map))), 
                 names_to = "signal_col", values_to = "signal_val") %>%
    inner_join(current_weights, by = "signal_col") %>%
    inner_join(factor_meta, by = "characteristic") %>%
    group_by(characteristic) %>%
    mutate(rank = percent_rank(signal_val),
           stock_pos = case_when(
             direction ==  1 & rank > 0.7 ~  1, direction ==  1 & rank < 0.3 ~ -1,
             direction == -1 & rank < 0.3 ~  1, direction == -1 & rank > 0.7 ~ -1,
             TRUE ~ 0
           )) %>%
    filter(stock_pos != 0) %>%
    group_by(characteristic, stock_pos) %>%
    mutate(wgt_in_leg = me_lag1 / sum(me_lag1, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(net_stock_wgt = strat_wgt * stock_pos * wgt_in_leg) %>%
    group_by(id) %>%
    summarize(final_net_weight = sum(net_stock_wgt), .groups = "drop") %>%
    mutate(date = target_date) # Tag with the actual strategy date
  
  net_stock_positions_list[[i]] <- month_positions
}

# --- 2. COMBINE RESULTS ---
cat("\n--- Finalizing Data ---\n")
net_stock_positions <- bind_rows(net_stock_positions_list)

# --- 3. VIEW TOP POSITIONS ---
latest_date <- max(net_stock_positions$date)
cat("\nSuccess! Top 10 Net Positions as of:", as.character(latest_date), "\n")

# Attach Company Metadata (GICS)
names_map <- read_parquet(UNIVERSE_FILE) %>% select(id, gics) %>% distinct()

print(net_stock_positions %>% 
        filter(date == latest_date) %>% 
        left_join(names_map, by = "id") %>%
        arrange(desc(abs(final_net_weight))) %>% 
        head(10))

# --- 4. VIEW TOP POSITIONS ---
latest_date <- max(net_stock_positions$date)
cat("\nTop 10 Net Positions as of:", as.character(latest_date), "\n")
print(head(net_stock_positions %>% filter(date == latest_date) %>% arrange(desc(abs(final_net_weight))), 10))