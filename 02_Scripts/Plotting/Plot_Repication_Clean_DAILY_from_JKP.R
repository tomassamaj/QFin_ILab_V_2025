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
