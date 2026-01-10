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
# --- Load Required Libraries ---
# Make sure these are loaded from your previous script
library(zoo) # For rollapply
library(purrr)     # For loops/mapping
library(quantmod)  # For financial data handling
library(gt) # For tables


# --- 1. Load and Prepare Factor/Theme Data ---

# Define start and end dates for analysis
start_date <- ymd("1963-07-01") # Match factor data availability 
end_date <- ymd("2024-12-31")   # Match factor data availability 

# Load factor returns from CSV
all_factors_monthly_vw_cap <- read.csv("[usa]_[all_factors]_[monthly]_[vw_cap].csv")
all_factors_wide <- all_factors_monthly_vw_cap %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>% # Convert date early
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

# Load theme returns from CSV
all_themes_monthly_vw_cap <- read.csv("[usa]_[all_themes]_[monthly]_[vw_cap].csv")
all_themes_wide <- all_themes_monthly_vw_cap %>%
  select(date, name, ret) %>%
  mutate(date = ymd(date)) %>% # Convert date early
  filter(date >= start_date & date <= end_date) %>%
  pivot_wider(names_from = name, values_from = ret)

# Merge themes and factors
# Ensure dates match exactly for merging
merged_factors <- inner_join(all_themes_wide, all_factors_wide, by = "date")

ind_ports <- read.csv("17_Industry_Portfolios.csv") # Load industry names

# --- 2. Load and Prepare Fama-French Industry Data ---

# Download 17 Industry Portfolios data
# Need a list of industry names first (adjust path if needed, or define manually)
# Assuming 'ind_ports' dataframe exists from previous context
# ind_ports <- read.csv("17_Industry_Portfolios.csv") # Example if needed
industry_names <- ind_ports[7:23, 1]

ff_17_industry_monthly_raw <- download_french_data("17 Industry Portfolios")

ff_17_industry_monthly <- ff_17_industry_monthly_raw$subsets$data[[1]] |>
  mutate(
    # Convert YYYYMM to Date (start of month)
    date = floor_date(ymd(str_c(date, "01")), "month"),
    # Convert returns to numeric proportions
    across(all_of(industry_names), ~ as.numeric(.) / 100),
    .keep = "none" # Keep only specified columns
  ) |>
  rename_with(str_to_lower) |> # Standardize column names
  # Filter for the desired date range
  filter(date >= start_date & date <= end_date) |>
  # Convert date to end-of-month to match factor data
  mutate(date = ceiling_date(date, "month") - days(1))


# --- 3. Merge All Data ---

# Merge industry data with the combined factor/theme data
final_merged <- inner_join(ff_17_industry_monthly, merged_factors, by = "date")

# --- 4. Rename Factors to Readable Names ---

# Map of readable names to original factor names
factor_rename_map <- c(
  # Common factors
  "Size_SMB" = "market_equity", "Book_to_Market_HML" = "be_me",
  "Operating_Profitability_RMW" = "ope_be", "Asset_Growth_CMA" = "at_gr1",
  "Long_Term_Reversals_LTREV" = "ret_60_12", "Residual_Variance_RVAR" = "ivol_ff3_21d", # Assuming this col exists, adjust if needed
  "Quality_Minus_Junk_QMJ" = "qmj", "Low_Beta_BAB" = "betabab_1260d",
  # Non-fundamental
  "Amihud_Illiquidity" = "ami_126d", "Firm_Age" = "age",
  "Nominal_Price" = "prc", "High_Volume_Premium" = "dolvol_126d",
  # Profitability
  "Gross_Profitability" = "gp_at", "Return_on_Equity" = "ni_be",
  "Return_on_Assets" = "niq_at", "Profit_Margin" = "ebit_sale",
  "Change_in_Asset_Turnover" = "at_turnover",
  # Earnings quality
  "Accruals_Factor" = "oaccruals_at", "Net_Operating_Assets" = "noa_at",
  "Net_Working_Capital_Changes" = "cowc_gr1a", "Cash_Flow_to_Price" = "ocf_me",
  "Earnings_to_Price" = "ni_me", "Enterprise_Multiple" = "ebitda_mev",
  "Sales_to_Price" = "sale_me",
  # Investment and growth
  "Growth_in_Inventory" = "inv_gr1", "Sales_Growth" = "sale_gr1",
  "Growth_in_Sales_Inventory" = "dsale_dinv", "Abnormal_Investment" = "capex_abn",
  "CAPX_Growth_Rate" = "capx_gr1",
  # Financing
  "Debt_Issuance_Factor" = "dbnetis_at", "Leverage_Factor" = "at_be",
  "One_Year_Share_Issuance" = "chcsho_12m", "Total_External_Financing" = "netis_at",
  # Distress
  "Ohlson_O_Score" = "o_score", "Altman_Z_Score" = "z_score",
  # Composite
  "Piotroski_F_Score" = "f_score"
)

# Apply renaming to the merged dataset
# Use any_of() to avoid errors if a factor in the map isn't in the data
final_merged_renamed <- final_merged %>%
  rename(any_of(factor_rename_map))

# --- 5. Define Columns for Analysis ---

# Get industry column names (already lowercase)
industry_cols <- str_to_lower(industry_names)

# Get the successfully renamed factor names (present in the final df)
renamed_factor_cols <- intersect(names(factor_rename_map), colnames(final_merged_renamed))

# Get theme column names (present in the final df)
theme_cols <- intersect(colnames(all_themes_wide)[-1], colnames(final_merged_renamed))

# --- 6. Function to Calculate 1-Month Momentum Returns ---
calculate_momentum <- function(df, target_cols, strategy_name) {
  if (length(target_cols) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 columns provided."))
    return(NULL) # Return NULL if not enough columns
  }
  # Check if target columns exist
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 valid columns found in dataframe."))
    return(NULL) # Return NULL if not enough valid columns
  }
  
  momentum_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), lag, .names = "{.col}_lag1")) %>%
    filter(row_number() > 1) %>% # Remove first row with NAs
    rowwise() %>%
    mutate(median_lag1_ret = median(c_across(ends_with("_lag1")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(ends_with("_lag1"),
                  ~ case_when(
                    !is.na(.) & . > median_lag1_ret ~ 1,
                    !is.na(.) & . <= median_lag1_ret ~ -1,
                    TRUE ~ 0 # Handle NAs in lagged returns
                  ),
                  .names = "{sub('_lag1', '_pos', .col)}"
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
                    . == -1 & n_short > 0 ~ 1 / n_short, # Weight magnitude
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      momentum_return = sum(
        c_across(ends_with("_pos")) * # Position (+1 or -1)
          c_across(ends_with("_wgt")) * # Weight magnitude (1/N)
          c_across(all_of(target_cols_exist)), # Current month returns
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    mutate(strategy_type = strategy_name)
  
  return(momentum_df)
}

# --- 7. Calculate Industry and Factor Momentum ---
industry_momentum <- calculate_momentum(final_merged_renamed, industry_cols, "Industry Momentum")

# Calculate momentum using ONLY the renamed factors from the map
factor_momentum <- calculate_momentum(final_merged_renamed, renamed_factor_cols, "Factor Momentum")

# --- 8. Combine Momentum Series and Plot ---
if (!is.null(industry_momentum) && !is.null(factor_momentum)) {
  common_start_date <- max(min(industry_momentum$date), min(factor_momentum$date))
  
  combined_momentum <- bind_rows(industry_momentum, factor_momentum) %>%
    filter(date >= common_start_date) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  # Plotting
  print( # Explicitly print ggplot object
    ggplot(combined_momentum, aes(x = date, y = cumulative_return_log, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(
        breaks = scales::log_breaks(n = 10),
        labels = scales::label_number(accuracy = 0.1)
      ) +
      scale_color_manual(values = c("Industry Momentum" = "black", "Factor Momentum" = "blue")) +
      labs(
        title = "Cumulative Performance of Factor vs. Industry Momentum",
        subtitle = "Value of $1 invested (Log Scale)",
        x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  )
} else {
  print("Could not calculate both momentum series; skipping combined plot.")
}

# --- 9. Correlation Heatmap of Renamed Factors ---

# Select only the renamed factor columns for correlation
factor_data_for_corr <- final_merged_renamed %>%
  select(any_of(renamed_factor_cols)) %>%
  na.omit()

if (ncol(factor_data_for_corr) >= 2) {
  # Calculate correlation matrix
  cor_matrix <- cor(factor_data_for_corr)
  
  # Define color palette (e.g., Brown-BlueGreen)
  col_palette <- colorRampPalette(brewer.pal(n = 11, name = "BrBG"))(200)
  
  # Generate the upper triangular corrplot with coefficients
  print( # Explicitly print corrplot object if needed in some environments
    corrplot(cor_matrix,
             method = "color",
             type = "upper",
             order = "original", # Use "hclust" for clustered order
             tl.col = "black",
             tl.srt = 45,
             tl.cex = 0.5,     # Adjust text label size
             number.cex = 0.5, # Adjust coefficient number size
             col = col_palette,
             diag = FALSE,
             cl.cex = 0.7,
             mar = c(0,0,1,0) # Adjust margins if needed
    )
  )
  
  # Generate the upper triangular corrplot with coefficients
  print( # Explicitly print corrplot object if needed in some environments
    corrplot(cor_matrix,
             method = "color",
             type = "upper",
             order = "original", # Use "hclust" for clustered order
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             tl.cex = 0.5,     # Adjust text label size
             number.cex = 0.5, # Adjust coefficient number size
             col = col_palette,
             diag = FALSE,
             cl.cex = 0.7,
             mar = c(0,0,1,0) # Adjust margins if needed
    )
  )
  # Add title separately for corrplot
  title("Upper Triangular Correlation Heatmap of Selected Factors", line = 3)
  
} else {
  print("Not enough factor columns with complete data to generate correlation plot.")
}

# --- 6. Function to Calculate 1-Month Momentum Returns ---
calculate_momentum <- function(df, target_cols, strategy_name) {
  if (length(target_cols) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 columns provided."))
    return(NULL) # Return NULL if not enough columns
  }
  # Check if target columns exist
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 valid columns found in dataframe."))
    return(NULL) # Return NULL if not enough valid columns
  }
  
  momentum_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), lag, .names = "{.col}_lag1")) %>%
    filter(row_number() > 1) %>% # Remove first row with NAs
    rowwise() %>%
    mutate(median_lag1_ret = median(c_across(ends_with("_lag1")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(ends_with("_lag1"),
                  ~ case_when(
                    !is.na(.) & . > median_lag1_ret ~ 1,
                    !is.na(.) & . <= median_lag1_ret ~ -1,
                    TRUE ~ 0 # Handle NAs in lagged returns
                  ),
                  .names = "{sub('_lag1', '_pos', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      n_long = sum(c_across(ends_with("_pos")) == 1),
      n_short = sum(c_across(ends_with("_pos")) == -1)
    ) %>%
    ungroup() %>%
    # Calculate weights based on positions (use sign() for simplicity here matching long +1/N, short -1/N)
    mutate(across(ends_with("_pos"),
                  ~ case_when(
                    . == 1 & n_long > 0 ~ 1 / n_long,
                    . == -1 & n_short > 0 ~ -1 / n_short, # Weight magnitude AND sign
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      # Calculate return: Sum of (Weight * Current Return)
      momentum_return = sum(
        c_across(ends_with("_wgt")) * # Weight includes sign here
          c_across(all_of(target_cols_exist)), # Current month returns
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    mutate(strategy_type = strategy_name)
  
  return(momentum_df)
}

# --- 7. Calculate Raw Industry and Factor Momentum ---
industry_momentum_raw <- calculate_momentum(final_merged_renamed, industry_cols, "Industry Momentum")
factor_momentum_raw <- calculate_momentum(final_merged_renamed, renamed_factor_cols, "Factor Momentum")

# --- Function for Volatility Scaling ---
scale_volatility <- function(df, target_ann_vol = 0.10, lookback_months = 36, min_obs = 12) {
  if (!"momentum_return" %in% colnames(df)) {
    stop("Input dataframe must contain 'momentum_return' column.")
  }
  if (!"date" %in% colnames(df)) {
    stop("Input dataframe must contain 'date' column.")
  }
  # Ensure data is sorted by date
  df <- df %>% arrange(date)
  
  # Calculate rolling standard deviation
  rolling_sd <- rollapply(df$momentum_return,
                          width = lookback_months,
                          FUN = sd,
                          na.rm = TRUE,
                          fill = NA,
                          align = "right",
                          partial = min_obs) # Allow calculation with fewer initial obs
  
  # Annualize volatility (multiply monthly SD by sqrt(12))
  annualized_rolling_vol <- rolling_sd * sqrt(12)
  
  # Calculate leverage factor for NEXT month (use lag)
  # Cap leverage to avoid extreme values (e.g., max 5x)
  leverage_factor <- lag(pmin(5, target_ann_vol / annualized_rolling_vol, na.rm = TRUE), 1)
  
  # Handle potential division by zero or NA early on
  leverage_factor[is.infinite(leverage_factor) | is.na(leverage_factor)] <- 1 # Default to 1x leverage if vol is 0 or NA
  
  # Calculate scaled return using lagged leverage
  df <- df %>%
    mutate(
      rolling_ann_vol = annualized_rolling_vol,
      leverage = leverage_factor,
      scaled_momentum_return = momentum_return * leverage
    ) %>%
    filter(!is.na(scaled_momentum_return)) # Remove initial rows where scaling isn't possible
  
  return(df)
}

# --- 9. Apply Volatility Scaling ---
target_vol <- 0.10 # 10% annualized target volatility
lookback <- 36    # 36-month lookback

if (!is.null(industry_momentum_raw)) {
  industry_momentum_scaled <- scale_volatility(industry_momentum_raw,
                                               target_ann_vol = target_vol,
                                               lookback_months = lookback)
} else {
  industry_momentum_scaled <- NULL
}

if (!is.null(factor_momentum_raw)) {
  factor_momentum_scaled <- scale_volatility(factor_momentum_raw,
                                             target_ann_vol = target_vol,
                                             lookback_months = lookback)
} else {
  factor_momentum_scaled <- NULL
}


# --- 10. Combine Scaled Momentum Series and Plot ---
if (!is.null(industry_momentum_scaled) && !is.null(factor_momentum_scaled)) {
  # Find the common start date AFTER scaling (due to lookback)
  common_start_date_scaled <- max(min(industry_momentum_scaled$date), min(factor_momentum_scaled$date))
  
  combined_momentum_scaled <- bind_rows(industry_momentum_scaled, factor_momentum_scaled) %>%
    filter(date >= common_start_date_scaled) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate cumulative return based on SCALED returns
    mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
    ungroup()
  
  # Plotting Scaled Returns
  plot_scaled <- ggplot(combined_momentum_scaled, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(values = c("Industry Momentum" = "black", "Factor Momentum" = "blue")) +
    labs(
      title = paste0("Cumulative Performance (Scaled to ", scales::percent(target_vol, accuracy = 1), " Ann. Volatility)"),
      subtitle = "Value of $1 invested (Log Scale)",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_scaled)
  
} else {
  print("Could not calculate both scaled momentum series; skipping scaled plot.")
}

# --- 1. Download Fama-French 3 Factors ---
# Define dates matching your main data
start_date_ff <- floor_date(min(final_merged_renamed$date), "month")
end_date_ff <- ceiling_date(max(final_merged_renamed$date), "month") - days(1)

ff3_factors_monthly_raw <- download_french_data("Fama/French 3 Factors")
ff3_factors_monthly <- ff3_factors_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c("Mkt-RF", SMB, HML, RF), ~ as.numeric(.) / 100),
    .keep = "none"
  ) |>
  filter(date >= start_date_ff & date <= end_date_ff) |>
  # Ensure date format matches final_merged_renamed (end of month)
  mutate(date = ceiling_date(date, "month") - days(1)) |>
  rename(Mkt_RF = "Mkt-RF") # Rename for easier formula use

# --- 11. Compare Factor Momentum vs. Benchmarks (Equity & Bonds) ---

# --- 1. Download Benchmark Data (Quantmod) ---

# Define start date (slightly earlier to ensure monthly calculation works for start of analysis)
q_start <- "1960-01-01" 

# A. Download S&P 500 (^GSPC)
getSymbols("^GSPC", src = "yahoo", from = q_start, to = Sys.Date(), auto.assign = TRUE)
sp500_monthly <- monthlyReturn(Ad(GSPC), type = "log") # Log returns for consistency
# Convert index to Date and align to End-of-Month
sp500_df <- data.frame(date = index(sp500_monthly), sp500_ret = as.numeric(sp500_monthly)) %>%
  mutate(date = ceiling_date(date, "month") - days(1))

# B. Download US Treasuries (IEF)
# Note: IEF inception is around 2002. Data before that will be NA.
getSymbols("IEF", src = "yahoo", from = "2000-01-01", to = Sys.Date(), auto.assign = TRUE)
ief_monthly <- monthlyReturn(Ad(IEF), type = "log")
ief_df <- data.frame(date = index(ief_monthly), bond_ret = as.numeric(ief_monthly)) %>%
  mutate(date = ceiling_date(date, "month") - days(1))

# --- 2. Merge and Align Data ---

# Prepare Strategy Data (Rename for plot labels)
strat_df <- factor_momentum_raw %>%
  select(date, `Factor Momentum` = momentum_return) %>%
  left_join(industry_momentum_raw %>% select(date, `Industry Momentum` = momentum_return), by = "date")

# Join with Quantmod Data
combined_data <- strat_df %>%
  left_join(sp500_df, by = "date") %>%
  left_join(ief_df, by = "date") %>%
  rename(`S&P 500` = sp500_ret, `US Treasuries (IEF)` = bond_ret) %>%
  pivot_longer(cols = -date, names_to = "Strategy", values_to = "Return") %>%
  # Remove rows where return is NA (Crucial for IEF to start at 1.0 in 2002)
  filter(!is.na(Return))

# --- 3. Calculate Cumulative Wealth ---

plot_data <- combined_data %>%
  arrange(Strategy, date) %>%
  group_by(Strategy) %>%
  mutate(Cumulative_Wealth = cumprod(1 + Return)) %>%
  ungroup()

# --- 4. Generate Plot ---

# Define colors matching the screenshot
custom_colors <- c(
  "Factor Momentum" = "#003f5c",     # Dark Blue
  "Industry Momentum" = "#4481c2",   # Lighter Blue
  "S&P 500" = "#d62728",             # Red
  "US Treasuries (IEF)" = "#ff7f0e"  # Orange
)

p <- ggplot(plot_data, aes(x = date, y = Cumulative_Wealth, color = Strategy)) +
  geom_line(linewidth = 1) +
  scale_y_log10(
    breaks = c(1, 3, 10, 30, 100),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Momentum (J=1, K=1) vs Benchmarks",
    subtitle = "Cumulative Wealth (Log Scale) | Rebased to 1.0",
    y = "Cumulative Wealth ($)",
    x = NULL,
    color = NULL,
    caption = "Benchmarks: S&P 500 (^GSPC) and US Treasuries (IEF) fetched via Quantmod"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_line(color = "gray90")
  )

print(p)

# --- 5. Generate Performance Table (gt) ---

# Get Risk Free rate from FF data for Sharpe Ratio calculation
# If ff3_factors_monthly is not in env, assume 0 for safety, but try to use it.
if(exists("ff3_factors_monthly")) {
  rf_data <- ff3_factors_monthly %>% select(date, RF)
} else {
  rf_data <- data.frame(date = unique(combined_data$date), RF = 0)
}

perf_stats <- combined_data %>%
  left_join(rf_data, by = "date") %>%
  group_by(Strategy) %>%
  summarise(
    Total_Return = prod(1 + Return) - 1,
    N_Months = n(),
    # Geometric Annualized Return
    Ann_Return = (1 + Total_Return)^(12 / N_Months) - 1,
    # Annualized Volatility
    Ann_Vol = sd(Return) * sqrt(12),
    # Sharpe Ratio (Mean Excess Return / Std Dev)
    Sharpe = (mean(Return - coalesce(RF, 0)) * 12) / Ann_Vol
  ) %>%
  select(Strategy, Total_Return, Ann_Return, Ann_Vol, Sharpe) %>%
  arrange(desc(Total_Return))

# Create Table
gt_table <- perf_stats %>%
  gt() %>%
  tab_header(
    title = md("**Performance Summary**")
  ) %>%
  fmt_percent(
    columns = c(Total_Return, Ann_Return, Ann_Vol),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(Sharpe),
    decimals = 2
  ) %>%
  cols_label(
    Total_Return = "Total Return",
    Ann_Return = "Ann. Return",
    Ann_Vol = "Ann. Vol",
    Sharpe = "Sharpe"
  ) %>%
  tab_options(
    table.border.top.color = "white",
    heading.align = "left",
    table.font.size = 14,
    data_row.padding = px(6)
  )

print(gt_table)
# --- 2. Select One JKP Factor to Regress ---
jkp_factor_name <- "Book_to_Market_HML" # Make sure this matches a column name

reg_data <- final_merged_renamed %>%
  select(date, all_of(jkp_factor_name)) %>%
  # Merge with FF3 factors
  inner_join(ff3_factors_monthly, by = "date") %>%
  # Calculate excess return for the JKP factor
  mutate(Factor_Excess = .data[[jkp_factor_name]] - RF) %>%
  na.omit() # Remove rows with NAs that might interfere

# --- 3. Run the Regression ---
# Regress the JKP Factor's Excess Return on FF3 Factors
ff3_model <- lm(Factor_Excess ~ Mkt_RF + SMB + HML, data = reg_data)

# --- 4. Display Summary ---
print(paste("Regression Summary for JKP Factor:", jkp_factor_name, "on FF3"))
summary(ff3_model)

# Optional: Tidy output using broom
tidy_summary <- tidy(ff3_model)
print("Tidied Regression Output:")
print(tidy_summary)

glance_summary <- glance(ff3_model)
print("Model Fit Statistics:")
print(glance_summary)

################ ROUND 2 ####################

factors_ff5_monthly_raw <- download_french_data("Fama/French 5 Factors (2x3)")

factors_ff5_monthly <- factors_ff5_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(date >= start_date & date <= end_date)

factors_mom_monthly_raw <- download_french_data("Momentum Factor (Mom)")

factors_mom_monthly <- factors_mom_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(Mom), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  filter(date >= start_date & date <= end_date)


# Merge FF5 and Momentum factors
factors_ff5_mom_monthly <- inner_join(factors_ff5_monthly, factors_mom_monthly, by = "date")

# --- 3. Run Regressions for All Factors ---

all_reg_data <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  inner_join(ff3_factors_monthly, by = "date")

cat("\n\n--- Running Full-Period FF3 Regressions ---\n")

# --- 3. Loop, Run Regressions, and Print Results ---

# Loop through each factor name in the list
for (factor_name in renamed_factor_cols) {
  
  # --- A. Prepare Data for this Factor ---
  regression_df <- all_reg_data %>%
    # Calculate this factor's excess return (Factor - RF)
    mutate(Factor_Excess = .data[[factor_name]] - RF) %>%
    # Select only relevant columns and drop NAs
    select(Factor_Excess, Mkt_RF, SMB, HML) %>%
    na.omit()
  
  # --- B. Run Regression (if data exists) ---
  if (nrow(regression_df) > 0) {
    
    # Run the linear model (FF3 Regression)
    ff3_model <- lm(Factor_Excess ~ Mkt_RF + SMB + HML, data = regression_df)
    
    # Tidy the model output into a clean data frame
    # and select/rename columns at the same time
    factor_results <- tidy(ff3_model) %>%
      select(term, estimate, p.value) %>%
      mutate(term = recode(term, `(Intercept)` = "Alpha"))
    
    # --- C. Print Formatted Output ---
    cat("\n")
    cat("----------------------------------------\n")
    cat(" Factor:", factor_name, "\n")
    cat("----------------------------------------\n")
    print(factor_results)
    
  } else {
    # Skip if no data
    cat("\n")
    cat("----------------------------------------\n")
    cat(" Factor:", factor_name, "\n")
    cat("----------------------------------------\n")
    cat("  Skipped - No complete data for regression.\n")
  }
}

# Regression on ff5 -------------------------------------------------------

# adjust so it is end of month
factors_ff5_mom_eom <- factors_ff5_mom_monthly %>%
  mutate(date = ceiling_date(date, "month") - days(1))

# --- 3. Prepare Full Regression Data ---

# Join factors with the 6-factor model data
all_reg_data_ff5_mom <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  # Use the date-corrected FF5+Mom data
  inner_join(factors_ff5_mom_eom, by = "date")

cat("\n\n--- Running Full-Period FF5 + Momentum Regressions ---\n")

# --- 4. Loop, Run Regressions, and Print Results ---

# Loop through each factor name in the list
for (factor_name in renamed_factor_cols) {
  
  # --- A. Prepare Data for this Factor ---
  regression_df <- all_reg_data_ff5_mom %>%
    # Calculate this factor's excess return (Factor - RF)
    # Note: 'rf' column comes from the 'factors_ff5_mom_eom' data
    mutate(Factor_Excess = .data[[factor_name]] - rf) %>%
    # Select only relevant columns and drop NAs
    select(Factor_Excess, mkt_excess, smb, hml, rmw, cma, mom) %>%
    na.omit()
  
  # --- B. Run Regression (if data exists) ---
  if (nrow(regression_df) > 0) {
    
    # Run the 6-factor linear model
    ff6_model <- lm(Factor_Excess ~ mkt_excess + smb + hml + rmw + cma + mom, 
                    data = regression_df)
    
    # Tidy the model output into a clean data frame
    factor_results <- tidy(ff6_model) %>%
      select(term, estimate, p.value) %>%
      # Rename intercept to 'Alpha' for clarity
      mutate(term = recode(term, `(Intercept)` = "Alpha"))
    
    # --- C. Print Formatted Output ---
    cat("\n")
    cat("----------------------------------------\n")
    cat(" Factor:", factor_name, "\n")
    cat("----------------------------------------\n")
    print(factor_results)
    
  } else {
    # Skip if no data
    cat("\n")
    cat("----------------------------------------\n")
    cat(" Factor:", factor_name, "\n")
    cat("----------------------------------------\n")
    cat("  Skipped - No complete data for regression.\n")
  }
}


# New plot just using selected factors ------------------------------------

selected_factors <- c(
  # --- Classic Factors ---
  "Size_SMB",                      # Size
  "Book_to_Market_HML",            # Value
  "Operating_Profitability_RMW",   # Profitability
  "Asset_Growth_CMA",              # Investment
  
  # --- Other Factors ---
  "Long_Term_Reversals_LTREV",     # Reversal
  "Low_Beta_BAB",                  # Low Volatility / Risk
  "Firm_Age",                      # Quality / Lifecycle
  "Enterprise_Multiple",           # Value (Alternative)
  "Accruals_Factor",               # Earnings Quality
  "Total_External_Financing",      # Financing / Issuance
  "Piotroski_F_Score"              # Quality / Financial Strength
)

cat("--- Running 'Expanded Team' Strategy with", length(selected_factors), "factors ---\n")
print(selected_factors)

# --- 1. Compute Selected Factor Momentum (UNSCALED) ---
selected_factor_momentum <- calculate_momentum(final_merged_renamed, 
                                               selected_factors, 
                                               "Selected Factor Momentum")

# --- 2. Combine with Industry Momentum and Plot (UNSCALED) ---
if (!is.null(industry_momentum) && !is.null(selected_factor_momentum)) {
  # Find the common start date
  common_start_date_selected_unscaled <- max(min(industry_momentum$date), min(selected_factor_momentum$date))
  
  combined_momentum_selected_unscaled <- bind_rows(industry_momentum, selected_factor_momentum) %>%
    filter(date >= common_start_date_selected_unscaled) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate cumulative return based on UNSCALED returns
    mutate(cumulative_return_unscaled_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  # Plotting Unscaled Returns
  plot_selected_unscaled <- ggplot(combined_momentum_selected_unscaled, aes(x = date, y = cumulative_return_unscaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(values = c("Industry Momentum" = "black", "Selected Factor Momentum" = "red")) +
    labs(
      title = "Cumulative Performance of Selected Factor Momentum vs. Industry Momentum (Unscaled)",
      subtitle = "Value of $1 invested (Log Scale)",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_selected_unscaled)
  
} else {
  print("Could not find both unscaled selected factor and industry momentum series; skipping selected factor plot.")
}

# ---
  
  # --- 3. Plot Selected Factor Momentum vs. All Factor Momentum (UNSCALED) ---
if (!is.null(factor_momentum) && !is.null(selected_factor_momentum)) {
    # Find the common start date
    common_start_date_comparison_unscaled <- max(min(factor_momentum$date), min(selected_factor_momentum$date))
    
    combined_momentum_comparison_unscaled <- bind_rows(
      factor_momentum %>% mutate(strategy_type = "All Factor Momentum"), # 'factor_momentum' is assumed to be the "All Factors" one
      selected_factor_momentum # This already has strategy_type = "Selected Factor Momentum" from its creation
    ) %>%
      filter(date >= common_start_date_comparison_unscaled) %>%
      arrange(strategy_type, date) %>%
      group_by(strategy_type) %>%
      # Calculate cumulative return based on UNSCALED returns
      mutate(cumulative_return_unscaled_log = cumprod(1 + momentum_return)) %>%
      ungroup()
    
    # Plotting Unscaled Returns
    plot_comparison_unscaled <- ggplot(combined_momentum_comparison_unscaled, aes(x = date, y = cumulative_return_unscaled_log, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(
        breaks = scales::log_breaks(n = 10),
        labels = scales::label_number(accuracy = 0.1)
      ) +
      scale_color_manual(values = c("All Factor Momentum" = "blue", "Selected Factor Momentum" = "red")) +
      labs(
        title = "Cumulative Performance: All Factor vs. Selected Factor Momentum (Unscaled)",
        subtitle = "Value of $1 invested (Log Scale)",
        x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    print(plot_comparison_unscaled)
    
  } else {
    print("Could not calculate both unscaled all factor and selected factor momentum series; skipping comparison plot.")
  }

# ---
  
  # --- 4. Calculate "Excluded" Factor Momentum (UNSCALED) ---
excluded_factors <- setdiff(renamed_factor_cols, selected_factors)
print("Excluded factors:")
print(excluded_factors)

if (length(excluded_factors) >= 2) {
  excluded_factor_momentum <- calculate_momentum(final_merged_renamed, 
                                                 excluded_factors, 
                                                 "Excluded Factor Momentum") 
} else {
  cat("\nWarning: Not enough 'excluded' factors to calculate momentum strategy.\n")
  excluded_factor_momentum <- NULL
}
# --- 5. Combine All Three Unscaled Strategies ---
if (!is.null(factor_momentum) && 
    !is.null(selected_factor_momentum) && 
    !is.null(excluded_factor_momentum)) {
  
  # Find the common start date
  common_start_date_all_three_unscaled <- max(
    min(factor_momentum$date), 
    min(selected_factor_momentum$date),
    min(excluded_factor_momentum$date)
  )
  
  # Combine, relabeling strategy_type for clarity
  combined_momentum_all_three_unscaled <- bind_rows(
    factor_momentum %>% mutate(strategy_type = "All Factors"), 
    selected_factor_momentum %>% mutate(strategy_type = "Selected Factors"), 
    excluded_factor_momentum %>% mutate(strategy_type = "Excluded Factors")
  ) %>%
    filter(date >= common_start_date_all_three_unscaled) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate cumulative return based on UNSCALED returns
    mutate(cumulative_return_unscaled_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  # --- 6. Plot All Three (UNSCALED) ---
  
  # Define colors for the three strategies
  # (This part was already correct)
  strategy_colors <- c(
    "All Factors"      = "blue", 
    "Selected Factors" = "red",
    "Excluded Factors" = "darkgreen"
  )
  
  plot_all_three_unscaled <- ggplot(combined_momentum_all_three_unscaled, 
                                    aes(x = date, y = cumulative_return_unscaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    # Now the 'values' (colors) will correctly match the 'strategy_type' column
    scale_color_manual(values = strategy_colors) + 
    labs(
      title = "Cumulative Performance: All vs. Selected vs. Excluded Factors (Unscaled)",
      subtitle = "Value of $1 invested (Log Scale)",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_all_three_unscaled)
  
} else {
  print("Could not calculate all three unscaled momentum series; skipping combined plot.")
}

# -------------------------00--

# Show performance of every factor 

# Pivot all factor columns to a long format
all_factors_long <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  pivot_longer(
    cols = -date,
    names_to = "factor_name",
    values_to = "return"
  ) %>%
  # Remove any NAs in returns that would break cumprod
  filter(!is.na(return))

# --- 2. Calculate Cumulative Returns ---
all_factors_cumulative <- all_factors_long %>%
  arrange(factor_name, date) %>%
  group_by(factor_name) %>%
  # Calculate cumulative return for each factor independently
  mutate(cumulative_return = cumprod(1 + return)) %>%
  ungroup()

# --- 3. Plot the "Spaghetti Graph" ---
spaghetti_plot <- ggplot(all_factors_cumulative, aes(x = date, y = cumulative_return, group = factor_name)) +
  geom_line(alpha = 0.6) + # Use alpha for some transparency
  scale_y_log10(
    breaks = scales::log_breaks(n = 10),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    title = "Cumulative Performance of All Individual Factors",
    subtitle = "Value of $1 invested (Log Scale). Each line is one factor.",
    x = "Year", y = "Cumulative Performance ($)"
  ) +
  theme_minimal(base_size = 12) +
  # Remove the legend as it would be unreadably large
  theme(legend.position = "none") 

print(spaghetti_plot)
# put names of each factr in the plot
# Add factor names at the end of each line
spaghetti_plot_labeled <- spaghetti_plot +
  geom_text(
    data = all_factors_cumulative %>%
      group_by(factor_name) %>%
      filter(date == max(date)), # Get last date for each factor
    aes(label = factor_name),
    hjust = 0, # Align text to the left of the point
    nudge_x = 10, # Nudge text slightly to the right
    size = 3, # Adjust text size as needed
    check_overlap = TRUE # Avoid overlapping labels
  ) +
  xlim(min(all_factors_cumulative$date), max(all_factors_cumulative$date) + months(6)) # Extend x-axis for labels
print(spaghetti_plot_labeled)


# --- Top/Bottom 25% (Quartile) Momentum Strategy ---

# --- 1. NEW Function: 1-Month Momentum (Top/Bottom 25%) ---
# This function longs the top 25% and shorts the bottom 25%, ignoring the middle 50%.
calculate_momentum_quartile <- function(df, target_cols, strategy_name) {
  
  # Need at least 4 assets to form quartiles
  if (length(target_cols) < 4) { 
    warning(paste("Skipping momentum for", strategy_name, "- less than 4 columns provided."))
    return(NULL)
  }
  # Check if target columns exist
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 4) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 4 valid columns found in dataframe."))
    return(NULL)
  }
  
  momentum_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), lag, .names = "{.col}_lag1")) %>%
    filter(row_number() > 1) %>% # Remove first row with NAs
    rowwise() %>%
    mutate(
      # Calculate 25th (q1) and 75th (q3) percentiles for this row
      q1_lag1_ret = quantile(c_across(ends_with("_lag1")), probs = 0.25, na.rm = TRUE),
      q3_lag1_ret = quantile(c_across(ends_with("_lag1")), probs = 0.75, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(across(ends_with("_lag1"),
                  ~ case_when(
                    !is.na(.) & . > q3_lag1_ret ~ 1,  # Long (Top 25%)
                    !is.na(.) & . < q1_lag1_ret ~ -1, # Short (Bottom 25%)
                    TRUE ~ 0                         # Middle 50%
                  ),
                  .names = "{sub('_lag1', '_pos', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      n_long = sum(c_across(ends_with("_pos")) == 1),
      n_short = sum(c_across(ends_with("_pos")) == -1)
    ) %>%
    ungroup() %>%
    # Calculate weights based on positions
    mutate(across(ends_with("_pos"),
                  ~ case_when(
                    . == 1 & n_long > 0 ~ 1 / n_long,
                    . == -1 & n_short > 0 ~ -1 / n_short, # Weight magnitude AND sign
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      # Calculate return: Sum of (Weight * Current Return)
      momentum_return = sum(
        c_across(ends_with("_wgt")) * # Weight includes sign here
          c_across(all_of(target_cols_exist)), # Current month returns
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    mutate(strategy_type = strategy_name)
  
  return(momentum_df)
}


# --- 2. Calculate Quartile Momentum Series ---
industry_momentum_q <- calculate_momentum_quartile(final_merged_renamed, industry_cols, "Industry Momentum (Quartile)")
factor_momentum_q <- calculate_momentum_quartile(final_merged_renamed, renamed_factor_cols, "Factor Momentum (Quartile)")


# --- 3. Combine and Plot RAW Quartile Momentum ---
if (!is.null(industry_momentum_q) && !is.null(factor_momentum_q)) {
  common_start_date_q <- max(min(industry_momentum_q$date), min(factor_momentum_q$date))
  
  combined_momentum_q <- bind_rows(industry_momentum_q, factor_momentum_q) %>%
    filter(date >= common_start_date_q) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  # Plotting
  print(
    ggplot(combined_momentum_q, aes(x = date, y = cumulative_return_log, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(
        breaks = scales::log_breaks(n = 10),
        labels = scales::label_number(accuracy = 0.1)
      ) +
      scale_color_manual(values = c("Industry Momentum (Quartile)" = "black", "Factor Momentum (Quartile)" = "orange")) +
      labs(
        title = "Cumulative Performance (Quartile Strategy: Top 25% vs Bottom 25%)",
        subtitle = "Value of $1 invested (Log Scale)",
        x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  )
} else {
  print("Could not calculate both quartile momentum series; skipping combined plot.")
}


# --- 4. Apply Volatility Scaling to Quartile Strategies ---

if (!is.null(industry_momentum_q)) {
  industry_momentum_q_scaled <- scale_volatility(industry_momentum_q,
                                               target_ann_vol = target_vol,
                                               lookback_months = lookback)
} else {
  industry_momentum_q_scaled <- NULL
}

if (!is.null(factor_momentum_q)) {
  factor_momentum_q_scaled <- scale_volatility(factor_momentum_q,
                                             target_ann_vol = target_vol,
                                             lookback_months = lookback)
} else {
  factor_momentum_q_scaled <- NULL
}

# --- 5. Combine and Plot SCALED Quartile Momentum ---
if (!is.null(industry_momentum_q_scaled) && !is.null(factor_momentum_q_scaled)) {
  common_start_date_q_scaled <- max(min(industry_momentum_q_scaled$date), min(factor_momentum_q_scaled$date))
  
  combined_momentum_q_scaled <- bind_rows(industry_momentum_q_scaled, factor_momentum_q_scaled) %>%
    filter(date >= common_start_date_q_scaled) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
    ungroup()
  
  # Plotting Scaled Quartile Returns
  plot_q_scaled <- ggplot(combined_momentum_q_scaled, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(values = c("Industry Momentum (Quartile)" = "black", "Factor Momentum (Quartile)" = "orange")) +
    labs(
      title = paste0("Cumulative Performance (Quartile Strategy, Scaled to ", scales::percent(target_vol, accuracy = 1), " Ann. Volatility)"),
      subtitle = "Value of $1 invested (Log Scale)",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_q_scaled)
  
} else {
  print("Could not calculate both scaled quartile momentum series; skipping scaled plot.")
}


# --- 50/50 vs 25/25 Factor Momentum Comparison Plots ---

# --- 1. Combine RAW Factor Momentum Strategies ---
if (!is.null(factor_momentum_raw) && !is.null(factor_momentum_q)) {
  
  # Combine the two factor momentum strategies (50/50 and 25/25)
  combined_factor_mom <- bind_rows(factor_momentum_raw, factor_momentum_q)
  
  # Find common start date
  common_start_date_factor_comp <- max(min(factor_momentum_raw$date), min(factor_momentum_q$date))
  
  # Calculate cumulative returns
  factor_mom_comp_plot_data <- combined_factor_mom %>%
    filter(date >= common_start_date_factor_comp) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  # Plotting Raw Comparison
  plot_factor_comp_raw <- ggplot(factor_mom_comp_plot_data, aes(x = date, y = cumulative_return_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    # Use distinct colors for comparison
    scale_color_manual(values = c("Factor Momentum" = "blue", "Factor Momentum (Quartile)" = "orange")) +
    labs(
      title = "Factor Momentum Comparison (Raw Returns)",
      subtitle = "50/50 (Median) vs. 25/25 (Quartile) Strategy | Log Scale",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_factor_comp_raw)
  
} else {
  print("Could not find raw factor momentum data for 50/50 vs 25/25 comparison plot.")
}


# --- 2. Combine SCALED Factor Momentum Strategies ---
if (!is.null(factor_momentum_scaled) && !is.null(factor_momentum_q_scaled)) {
  
  # Combine the two SCALED factor momentum strategies
  combined_factor_mom_scaled <- bind_rows(factor_momentum_scaled, factor_momentum_q_scaled)
  
  # Find common start date
  common_start_date_factor_comp_scaled <- max(min(factor_momentum_scaled$date), min(factor_momentum_q_scaled$date))
  
  # Calculate cumulative returns
  factor_mom_comp_scaled_plot_data <- combined_factor_mom_scaled %>%
    filter(date >= common_start_date_factor_comp_scaled) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Use the scaled return column
    mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
    ungroup()
  
  # Plotting Scaled Comparison
  plot_factor_comp_scaled <- ggplot(factor_mom_comp_scaled_plot_data, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    # Use distinct colors for comparison
    scale_color_manual(values = c("Factor Momentum" = "blue", "Factor Momentum (Quartile)" = "orange")) +
    labs(
      title = paste0("Factor Momentum Comparison (Scaled to ", scales::percent(target_vol, accuracy = 1), " Ann. Volatility)"),
      subtitle = "50/50 (Median) vs. 25/25 (Quartile) Strategy | Log Scale",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_factor_comp_scaled)
  
} else {
  print("Could not find scaled factor momentum data for 50/50 vs 25/25 comparison plot.")
}


# --- Factor Momentum with 1-Month Execution Lag (signal at t, return at t+1) ---

# Helper: momentum with execution lag
calculate_momentum_lagged <- function(df, target_cols, strategy_name, execution_lag = 1) {
  if (length(target_cols) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 columns provided."))
    return(NULL)
  }
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 valid columns found in dataframe."))
    return(NULL)
  }
  
  out <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    # build ranks on 1M-lagged returns (information at t)
    mutate(across(all_of(target_cols_exist), lag, .names = "{.col}_lag1")) %>%
    filter(row_number() > 1) %>%
    rowwise() %>%
    mutate(median_lag1_ret = median(c_across(ends_with("_lag1")), na.rm = TRUE)) %>%
    ungroup() %>%
    # positions: above-median long (+1), below/equal short (-1)
    mutate(across(ends_with("_lag1"),
                  ~ case_when(
                    !is.na(.) & . >  median_lag1_ret ~  1,
                    !is.na(.) & . <= median_lag1_ret ~ -1,
                    TRUE ~ 0
                  ),
                  .names = "{sub('_lag1', '_pos', .col)}")) %>%
    rowwise() %>%
    mutate(
      n_long  = sum(c_across(ends_with("_pos")) == 1),
      n_short = sum(c_across(ends_with("_pos")) == -1)
    ) %>%
    ungroup() %>%
    # convert positions to signed 1/N weights
    mutate(across(ends_with("_pos"),
                  ~ case_when(
                    . == 1  & n_long  > 0 ~  1 / n_long,
                    . == -1 & n_short > 0 ~ -1 / n_short,
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"))
  
  if (execution_lag > 0) {
    # apply weights to forward returns at t+lag
    lead_names <- paste0(target_cols_exist, "_lead")
    out <- out %>%
      mutate(across(all_of(target_cols_exist), dplyr::lead, n = execution_lag, .names = "{.col}_lead")) %>%
      mutate(valid_row = rowSums(across(all_of(lead_names), ~ !is.na(.))) > 0) %>%
      filter(valid_row) %>%
      select(-valid_row) %>%
      rowwise() %>%
      mutate(momentum_return = sum(c_across(ends_with("_wgt")) * c_across(all_of(lead_names)), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        signal_date         = date,
        implementation_date = date %m+% months(execution_lag)
      ) %>%
      transmute(date = implementation_date,
                momentum_return,
                strategy_type = strategy_name) %>%
      arrange(date)
  } else {
    out <- out %>%
      rowwise() %>%
      mutate(momentum_return = sum(c_across(ends_with("_wgt")) * c_across(all_of(target_cols_exist)), na.rm = TRUE)) %>%
      ungroup() %>%
      transmute(date, momentum_return, strategy_type = strategy_name) %>%
      arrange(date)
  }
  
  out
}

# Build (or reuse) industry momentum same-month
if (!exists("industry_momentum") || is.null(industry_momentum)) {
  industry_momentum <- calculate_momentum(final_merged_renamed, industry_cols, "Industry Momentum")
}

# Factor momentum with 1M delay (e.g., November signal -> December return)
factor_momentum_lag1 <- calculate_momentum_lagged(
  final_merged_renamed, renamed_factor_cols, "Factor Momentum (1M Lag)", execution_lag = 1
)

# Combine & plot
if (!is.null(industry_momentum) && !is.null(factor_momentum_lag1)) {
  common_start_date <- max(min(industry_momentum$date), min(factor_momentum_lag1$date))
  
  combined_momentum_lag <- bind_rows(industry_momentum, factor_momentum_lag1) %>%
    filter(date >= common_start_date) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  print(
    ggplot(combined_momentum_lag, aes(x = date, y = cumulative_return_log, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(breaks = scales::log_breaks(n = 10),
                    labels = scales::label_number(accuracy = 0.1)) +
      scale_color_manual(values = c("Industry Momentum" = "black",
                                    "Factor Momentum (1M Lag)" = "blue")) +
      labs(
        title = "Cumulative Performance: Industry (T) vs Factor (T+1)",
        subtitle = "Factor allocation delayed by one month (Nov signal → Dec return)",
        x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  )
} else {
  print("Could not calculate both momentum series; skipping delayed plot.")
}

# (Optional) 
if (exists("scale_volatility")) {
  target_vol <- if (exists("target_vol")) target_vol else 0.10
  lookback   <- if (exists("lookback"))   lookback   else 36
  
  ind_scaled <- tryCatch(scale_volatility(industry_momentum, target_ann_vol = target_vol, lookback_months = lookback), error = function(e) NULL)
  fac_scaled <- tryCatch(scale_volatility(factor_momentum_lag1, target_ann_vol = target_vol, lookback_months = lookback), error = function(e) NULL)
  
  if (!is.null(ind_scaled) && !is.null(fac_scaled)) {
    common_start_date_scaled <- max(min(ind_scaled$date), min(fac_scaled$date))
    combined_scaled <- bind_rows(ind_scaled, fac_scaled) %>%
      filter(date >= common_start_date_scaled) %>%
      arrange(strategy_type, date) %>%
      group_by(strategy_type) %>%
      mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
      ungroup()
    
    print(
      ggplot(combined_scaled, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
        geom_line(linewidth = 1) +
        scale_y_log10(breaks = scales::log_breaks(n = 10),
                      labels = scales::label_number(accuracy = 0.1)) +
        scale_color_manual(values = c("Industry Momentum" = "black",
                                      "Factor Momentum (1M Lag)" = "blue")) +
        labs(
          title = paste0("Cumulative Performance (Scaled to ", scales::percent(target_vol, accuracy = 1), " Ann. Vol): Industry (T) vs Factor (T+1)"),
          subtitle = "Value of $1 invested (Log Scale)",
          x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top",
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    )
  }
}


############## ROUND 2 CODE ####################

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





# --- 8. Analyze Long vs. Short Legs (Corrected Calculation) ---
print("--- 8. Running Long/Short Decomposition (1M/1M, Median) - Inline Calculation ---")

# Define Strategy Parameters
lookback_months <- 1
holding_months  <- 1
signal_lag      <- 1
long_quantile   <- 0.5
short_quantile  <- 0.5

# 1. Calculate Signals (Past 1-month cumulative log returns)
signals <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  arrange(date) %>%
  mutate(across(all_of(renamed_factor_cols), 
                ~ rollapply(log(1 + .), width = lookback_months, FUN = sum, fill = NA, align = "right"),
                .names = "{.col}_sig")) %>%
  mutate(across(ends_with("_sig"), ~ lag(., n = signal_lag))) # Lag signal by 1 month

# 2. Calculate Forward Returns (Next 1-month cumulative log returns)
returns <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  arrange(date) %>%
  mutate(across(all_of(renamed_factor_cols), 
                ~ rollapply(log(1 + .), width = holding_months, FUN = sum, fill = NA, align = "left"),
                .names = "{.col}_ret")) %>%
  select(date, ends_with("_ret"))

# 3. Merge and Calculate Portfolio Returns (Arithmetic Mean Fix)
# We calculate explicitly here to ensure Arithmetic Mean is used (fixing "too smooth" issue)
base_factor_mom <- inner_join(signals, returns, by = "date") %>%
  na.omit() %>%
  rowwise() %>%
  mutate(
    # Extract signal and return vectors for the current row (date)
    sigs = list(c_across(ends_with("_sig"))),
    rets = list(c_across(ends_with("_ret"))),
    
    # Calculate Portfolio Returns
    port_res = list({
      # Thresholds for median split
      q_l = quantile(sigs, long_quantile, na.rm=TRUE)
      q_s = quantile(sigs, short_quantile, na.rm=TRUE)
      
      # Identify assets for Long and Short baskets
      if (long_quantile == 0.5) {
        idx_l <- which(sigs > q_l)  # Top 50%
        idx_s <- which(sigs <= q_s) # Bottom 50%
      } else {
        idx_l <- which(sigs >= q_l)
        idx_s <- which(sigs <= q_s)
      }
      
      # *** CRITICAL FIX: Convert Log Returns to Simple Returns BEFORE Averaging ***
      # Averaging log returns = Geometric Mean (Variance Drag). 
      # Averaging simple returns = Arithmetic Mean (Correct Portfolio Return).
      simple_rets <- exp(rets) - 1
      
      # Calculate Arithmetic Mean (Equal Weight)
      r_long  <- if(length(idx_l)>0) mean(simple_rets[idx_l]) else 0
      r_short <- if(length(idx_s)>0) mean(simple_rets[idx_s]) else 0
      
      tibble(
        long_return = r_long,
        short_return = r_short,
        long_short_return = r_long - r_short
      )
    })
  ) %>%
  ungroup() %>%
  select(date, port_res) %>%
  unnest(port_res)

# 4. Prepare Data for Plotting
plot_data_ls <- base_factor_mom %>%
  select(date, long_return, short_return, long_short_return) %>%
  # Invert short return: We want to show the PnL of the short leg.
  # (e.g., if Short basket returns -5%, Strategy makes +5%)
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(-date, names_to = "Strategy", values_to = "Return") %>%
  filter(Strategy %in% c("long_return", "short_return_inv", "long_short_return")) %>%
  group_by(Strategy) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

# 5. Add explicit Start Date (t=0) at 1.0 for clean plotting
start_date_plot <- min(plot_data_ls$date) - months(1)
start_rows <- tibble(
  date = start_date_plot,
  Strategy = unique(plot_data_ls$Strategy),
  Return = 0,
  Cumulative_Return = 1.0
)
plot_data_final <- bind_rows(start_rows, plot_data_ls) %>% arrange(Strategy, date)

# 6. Plot
ls_plot <- ggplot(plot_data_final, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  # Highlight: Dot-com bubble (approx Mar 2000 - Oct 2002)
  annotate("rect", 
           xmin = ymd("2000-03-01"), xmax = ymd("2002-10-01"), 
           ymin = min(plot_data_final$Cumulative_Return, na.rm=T), 
           ymax = max(plot_data_final$Cumulative_Return, na.rm=T),
           alpha = 0.2, fill = "gray50") +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::comma, breaks = scales::log_breaks(n = 10)) +
  scale_color_manual(
    values = c("long_return" = "darkgreen", 
               "short_return_inv" = "darkred", 
               "long_short_return" = "blue"),
    labels = c("Long-Only Leg", "Long-Short Strategy", "Short-Only Leg (Inverted)")
  ) +
  labs(
    title = "Factor Momentum: Long vs. Short Leg Performance (Arnott et al. 2023 Replication)",
    subtitle = "1M/1M Strategy, Median Split. Short Leg inverted to show PnL contribution.",
    x = "Year", 
    y = "Cumulative Return (Log Scale, Start=1)"
  ) +
  theme_minimal(base_size = 12) +
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









# -------------------------00--
# 10. FACTOR CLUSTER ANALYSIS (CORRECTED)
# -------------------------00--
print("--- 10. Running Factor Cluster Analysis ---")

# --- 10.1. Correlation & Clustering ---

# Create correlation matrix
factor_data_for_corr <- final_merged_renamed %>%
  select(any_of(renamed_factor_cols)) %>%
  na.omit()

cor_matrix <- cor(factor_data_for_corr)

# Perform clustering
# Using 1 - abs(correlation) as distance so highly negative correlated factors cluster together
dist_matrix <- as.dist(1 - abs(cor_matrix)) 
hclust_results <- hclust(dist_matrix, method = "ward.D2")

# Cut tree into 5 clusters
K_CLUSTERS <- 5
clusters <- cutree(hclust_results, k = K_CLUSTERS)
cluster_list <- map(1:K_CLUSTERS, ~names(clusters[clusters == .x]))
names(cluster_list) <- paste0("Cluster_", 1:K_CLUSTERS)

print("Factor Clusters:")
print(cluster_list)

# --- 10.2. Calculate Momentum for Each Cluster (Using Inline Logic) ---

# Define fixed parameters for cluster comparison
lookback_months <- 1
holding_months  <- 1
signal_lag      <- 1
long_quantile   <- 0.5
short_quantile  <- 0.5

cluster_momentum_performance <- map_dfr(cluster_list, function(cluster_factors) {
  
  target_cols_exist <- intersect(cluster_factors, colnames(final_merged_renamed))
  if (length(target_cols_exist) < 2) return(NULL)
  
  # 1. Signal Generation
  signals <- final_merged_renamed %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), 
                  ~ rollapply(log(1 + .), width = lookback_months, FUN = sum, fill = NA, align = "right"),
                  .names = "{.col}_sig")) %>%
    mutate(across(ends_with("_sig"), ~ lag(., n = signal_lag)))
  
  # 2. Forward Returns
  returns <- final_merged_renamed %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), 
                  ~ rollapply(log(1 + .), width = holding_months, FUN = sum, fill = NA, align = "left"),
                  .names = "{.col}_ret")) %>%
    select(date, ends_with("_ret"))
  
  # 3. Merge & Filter
  full_dat <- inner_join(signals, returns, by = "date") %>% na.omit()
  
  # 4. Portfolio Calculation (Arithmetic Mean Fix)
  strat_ret <- full_dat %>%
    rowwise() %>%
    mutate(
      res = list({
        sigs <- c_across(ends_with("_sig"))
        rets <- c_across(ends_with("_ret"))
        
        q_l = quantile(sigs, long_quantile, na.rm=TRUE)
        q_s = quantile(sigs, short_quantile, na.rm=TRUE)
        
        if (long_quantile == 0.5) {
          idx_l <- which(sigs > q_l); idx_s <- which(sigs <= q_s)
        } else {
          idx_l <- which(sigs >= q_l); idx_s <- which(sigs <= q_s)
        }
        
        # Convert to simple returns
        simple_rets <- exp(rets) - 1
        
        ret_l <- if(length(idx_l)>0) mean(simple_rets[idx_l]) else 0
        ret_s <- if(length(idx_s)>0) mean(simple_rets[idx_s]) else 0
        
        tibble(long_return = ret_l, 
               short_return = ret_s, 
               long_short_return = ret_l - ret_s)
      })
    ) %>%
    ungroup() %>%
    select(date, res) %>%
    unnest(res)
  
  return(strat_ret)
}, .id = "Cluster")

# --- 10.3. Plot A: Momentum Performance by Factor Cluster ---
print("--- Plotting 10a: Momentum Performance by Cluster ---")

# Prepare Data
cluster_plot_data <- cluster_momentum_performance %>%
  select(Cluster, date, Return = long_short_return) %>%
  na.omit()

# Find Common Start Date (Preserving Date Class)
start_dates_a <- cluster_plot_data %>%
  group_by(Cluster) %>%
  summarise(first_date = min(date))

common_start_date_a <- max(start_dates_a$first_date)

# Filter & Cumulate
cluster_plot_data <- cluster_plot_data %>%
  filter(date >= common_start_date_a) %>%
  group_by(Cluster) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

# Add Start Date (t=0) at 1.0
start_rows_a <- tibble(
  date = common_start_date_a %m-% months(1),
  Cluster = unique(cluster_plot_data$Cluster),
  Return = 0,
  Cumulative_Return = 1.0
)

cluster_plot_final_a <- bind_rows(start_rows_a, cluster_plot_data) %>%
  arrange(Cluster, date)

# Plot
cluster_plot <- ggplot(cluster_plot_final_a, aes(x = date, y = Cumulative_Return, color = Cluster)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(labels = scales::comma, breaks = scales::log_breaks(n = 10)) +
  labs(title = "Momentum Performance by Factor Cluster",
       subtitle = "1M/1M Strategy, Median Split. Normalized start at 1.0.",
       x = "Year", y = "Cumulative Return (Log Scale, Start=1)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(cluster_plot)


# --- 10.4. Plot B: Long vs. Short vs. Net Performance (Faceted) ---
print("--- Plotting 10b: Long/Short Legs by Cluster ---")

# Prepare Data for Faceted Plot
cluster_ls_data <- cluster_momentum_performance %>%
  select(Cluster, date, long_return, short_return, long_short_return) %>%
  # Invert short return to show PnL contribution
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(
    cols = c("long_return", "short_return_inv", "long_short_return"), 
    names_to = "Strategy", 
    values_to = "Return"
  ) %>%
  na.omit() 

# Find Common Start Date
start_dates_b <- cluster_ls_data %>%
  group_by(Cluster) %>%
  summarise(first_date = min(date))

common_start_date_b <- max(start_dates_b$first_date)

# Filter & Cumulate
cluster_ls_data <- cluster_ls_data %>%
  filter(date >= common_start_date_b) %>%
  group_by(Cluster, Strategy) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

# Add Start Date (t=0) at 1.0
start_rows_b <- tibble(
  date = common_start_date_b %m-% months(1),
  # Create all combinations of Cluster and Strategy for the start row
  expand.grid(Cluster = unique(cluster_ls_data$Cluster), 
              Strategy = unique(cluster_ls_data$Strategy)),
  Return = 0,
  Cumulative_Return = 1.0
)

cluster_ls_final_b <- bind_rows(start_rows_b, cluster_ls_data) %>%
  arrange(Cluster, Strategy, date)

# Plot Faceted
cluster_ls_plot <- ggplot(cluster_ls_final_b, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  # Facet by Cluster
  facet_wrap(~ Cluster, scales = "free_y", ncol = 2) + 
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("long_return" = "darkgreen", 
                                "short_return_inv" = "darkred", 
                                "long_short_return" = "blue"),
                     labels = c("Long-Only", "Long-Short", "Short-Only (Inv)")) +
  labs(title = "Factor Momentum: Long vs. Short Leg Performance by Cluster",
       subtitle = "1M/1M Strategy, Median Split. Normalized start at 1.0.",
       x = "Year", y = "Cumulative Return (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(cluster_ls_plot)






# Even Split clusters -----------------------------------------------------

# -------------------------00--
# 10. FACTOR CLUSTER ANALYSIS (EVEN CLUSTERS + 1/3 LONG, 1/3 SHORT)
# -------------------------00--
print("--- 10. Running Factor Cluster Analysis (Even Clusters + 1/3 Long, 1/3 Short) ---")

# --- 10.1. Correlation & Clustering into 6 Even Clusters ---

# Create correlation matrix
factor_data_for_corr <- final_merged_renamed %>%
  select(any_of(renamed_factor_cols)) %>%
  na.omit()

cor_matrix <- cor(factor_data_for_corr)

# Perform clustering
# Using 1 - abs(correlation) as distance so highly negatively correlated factors cluster together
dist_matrix <- as.dist(1 - abs(cor_matrix)) 
hclust_results <- hclust(dist_matrix, method = "ward.D2")

# We want 6 clusters with as equal sizes as possible
K_CLUSTERS <- 6

# 1. Get factor names in dendrogram leaf order
factor_names_ordered <- colnames(cor_matrix)[hclust_results$order]

# 2. Work out equal-ish chunk sizes
n_factors <- length(factor_names_ordered)
base_size <- floor(n_factors / K_CLUSTERS)          # minimum size per cluster
remainder <- n_factors - base_size * K_CLUSTERS     # first 'remainder' clusters get +1

cluster_sizes <- rep(base_size, K_CLUSTERS)
if (remainder > 0) {
  cluster_sizes[1:remainder] <- cluster_sizes[1:remainder] + 1
}

# 3. Split the ordered factor list into consecutive chunks of those sizes
start_idx <- cumsum(c(1, head(cluster_sizes, -1)))
end_idx   <- cumsum(cluster_sizes)

cluster_list <- map2(start_idx, end_idx, ~ factor_names_ordered[.x:.y])
names(cluster_list) <- paste0("Cluster_", seq_len(K_CLUSTERS))

print("Even-sized Factor Clusters (based on dendrogram order):")
print(map_int(cluster_list, length))
print(cluster_list)


# --- 10.2. Calculate Momentum for Each Cluster (1/3 Long, 1/3 Short) ---

# Fixed parameters for signal/holding
lookback_months <- 1
holding_months  <- 1
signal_lag      <- 1

cluster_momentum_performance <- map_dfr(cluster_list, function(cluster_factors) {
  
  # Keep only factors that exist in the merged data
  target_cols_exist <- intersect(cluster_factors, colnames(final_merged_renamed))
  if (length(target_cols_exist) < 2) return(NULL)  # need at least 2 to form long & short
  
  # 1. Signal Generation (log returns over lookback, then lag)
  signals <- final_merged_renamed %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(
      all_of(target_cols_exist),
      ~ rollapply(log(1 + .), width = lookback_months, FUN = sum,
                  fill = NA, align = "right"),
      .names = "{.col}_sig"
    )) %>%
    mutate(across(ends_with("_sig"), ~ lag(., n = signal_lag)))
  
  # 2. Forward Returns (log returns over holding window)
  returns <- final_merged_renamed %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(
      all_of(target_cols_exist),
      ~ rollapply(log(1 + .), width = holding_months, FUN = sum,
                  fill = NA, align = "left"),
      .names = "{.col}_ret"
    )) %>%
    select(date, ends_with("_ret"))
  
  # 3. Merge & drop rows with any NA
  full_dat <- inner_join(signals, returns, by = "date") %>% 
    na.omit()
  
  # 4. Rowwise portfolio: 1/3 long, 1/3 short, middle neutral
  strat_ret <- full_dat %>%
    rowwise() %>%
    mutate(
      res = list({
        sigs <- c_across(ends_with("_sig"))
        rets <- c_across(ends_with("_ret"))
        
        # Drop positions with NA, just in case
        non_na <- which(!is.na(sigs) & !is.na(rets))
        if (length(non_na) < 2) {
          tibble(
            long_return       = 0,
            short_return      = 0,
            long_short_return = 0
          )
        } else {
          sigs <- sigs[non_na]
          rets <- rets[non_na]
          
          # Order signals ascending; bottom = short, top = long
          ord <- order(sigs)    # 1 = lowest signal
          n   <- length(ord)
          
          if (n < 3) {
            # Fallback: with <3 names, just use one long / one short
            idx_s <- ord[1]
            idx_l <- ord[n]
          } else {
            # 1/3 long, 1/3 short, middle 1/3 neutral
            k <- floor(n / 3)   # number in each tail
            idx_s <- ord[1:k]               # bottom k = short
            idx_l <- ord[(n - k + 1):n]     # top k = long
            # middle ord[(k+1):(n-k)] is unused (neutral)
          }
          
          # Convert log returns to simple returns
          simple_rets <- exp(rets) - 1
          
          ret_l <- mean(simple_rets[idx_l])
          ret_s <- mean(simple_rets[idx_s])
          
          tibble(
            long_return       = ret_l,
            short_return      = ret_s,
            long_short_return = ret_l - ret_s
          )
        }
      })
    ) %>%
    ungroup() %>%
    select(date, res) %>%
    unnest(res)
  
  return(strat_ret)
}, .id = "Cluster")


# --- 10.3. Plot A: Momentum Performance by Factor Cluster ---
print("--- Plotting 10a: Momentum Performance by Cluster ---")

# Prepare Data
cluster_plot_data <- cluster_momentum_performance %>%
  select(Cluster, date, Return = long_short_return) %>%
  na.omit()

# Find Common Start Date (Preserving Date Class)
start_dates_a <- cluster_plot_data %>%
  group_by(Cluster) %>%
  summarise(first_date = min(date), .groups = "drop")

common_start_date_a <- max(start_dates_a$first_date)

# Filter & Cumulate
cluster_plot_data <- cluster_plot_data %>%
  filter(date >= common_start_date_a) %>%
  group_by(Cluster) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

# Add Start Date (t=0) at 1.0
start_rows_a <- tibble(
  Cluster = unique(cluster_plot_data$Cluster)
) %>%
  mutate(
    date = common_start_date_a %m-% months(1),
    Return = 0,
    Cumulative_Return = 1.0
  )

cluster_plot_final_a <- bind_rows(start_rows_a, cluster_plot_data) %>%
  arrange(Cluster, date)

# Plot
cluster_plot <- ggplot(cluster_plot_final_a, aes(x = date, y = Cumulative_Return, color = Cluster)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(labels = scales::comma, breaks = scales::log_breaks(n = 10)) +
  labs(title = "Momentum Performance by Factor Cluster",
       subtitle = "1M/1M Strategy, 1/3 Long–1/3 Short per Cluster. Normalized start at 1.0.",
       x = "Year", y = "Cumulative Return (Log Scale, Start=1)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(cluster_plot)


# --- 10.4. Plot B: Long vs. Short vs. Net Performance (Faceted) ---
print("--- Plotting 10b: Long/Short Legs by Cluster ---")

# Prepare Data for Faceted Plot
cluster_ls_data <- cluster_momentum_performance %>%
  select(Cluster, date, long_return, short_return, long_short_return) %>%
  # Invert short return to show PnL contribution
  mutate(short_return_inv = -short_return) %>%
  pivot_longer(
    cols = c("long_return", "short_return_inv", "long_short_return"), 
    names_to = "Strategy", 
    values_to = "Return"
  ) %>%
  na.omit() 

# Find Common Start Date
start_dates_b <- cluster_ls_data %>%
  group_by(Cluster) %>%
  summarise(first_date = min(date), .groups = "drop")

common_start_date_b <- max(start_dates_b$first_date)

# Filter & Cumulate
cluster_ls_data <- cluster_ls_data %>%
  filter(date >= common_start_date_b) %>%
  group_by(Cluster, Strategy) %>%
  arrange(date) %>%
  mutate(Cumulative_Return = cumprod(1 + Return)) %>%
  ungroup()

# Add Start Date (t=0) at 1.0 for all Cluster × Strategy combos
start_rows_b <- expand_grid(
  Cluster  = unique(cluster_ls_data$Cluster),
  Strategy = unique(cluster_ls_data$Strategy)
) %>%
  mutate(
    date = common_start_date_b %m-% months(1),
    Return = 0,
    Cumulative_Return = 1.0
  )

cluster_ls_final_b <- bind_rows(start_rows_b, cluster_ls_data) %>%
  arrange(Cluster, Strategy, date)

# Plot Faceted
cluster_ls_plot <- ggplot(cluster_ls_final_b, aes(x = date, y = Cumulative_Return, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Cluster, scales = "free_y", ncol = 2) + 
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(
    values = c(
      "long_return"       = "darkgreen", 
      "short_return_inv"  = "darkred", 
      "long_short_return" = "blue"
    ),
    breaks = c("long_return", "short_return_inv", "long_short_return"),
    labels = c("Long-Only", "Short-Only (Inv)", "Long-Short")
  ) +
  labs(title = "Factor Momentum: Long vs. Short Leg Performance by Cluster",
       subtitle = "1M/1M Strategy, 1/3 Long 1/3 Short Split. Normalized start at 1",
       x = "Year", y = "Cumulative Return (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(cluster_ls_plot)




cluster_stats <- cluster_momentum_performance %>%
  group_by(Cluster) %>%
  summarise(
    n_obs = n(),
    
    # monthly means
    mean_ls  = mean(long_short_return, na.rm = TRUE),
    sd_ls    = sd(long_short_return, na.rm = TRUE),
    
    mean_long = mean(long_return, na.rm = TRUE),
    sd_long   = sd(long_return, na.rm = TRUE),
    
    mean_short = mean(short_return, na.rm = TRUE),
    sd_short   = sd(short_return, na.rm = TRUE),
    
    # annualised (assuming monthly data → *12)
    ann_ret_ls   = (1 + mean_ls) ^ 12 - 1,
    ann_vol_ls   = sd_ls * sqrt(12),
    ann_sharpe_ls = ann_ret_ls / ann_vol_ls,
    
    ann_ret_long   = (1 + mean_long) ^ 12 - 1,
    ann_vol_long   = sd_long * sqrt(12),
    ann_sharpe_long = ann_ret_long / ann_vol_long,
    
    ann_ret_short   = (1 + mean_short) ^ 12 - 1,
    ann_vol_short   = sd_short * sqrt(12),
    ann_sharpe_short = ann_ret_short / ann_vol_short,
    
    .groups = "drop"
  ) %>%
  arrange(desc(ann_sharpe_ls))  # sort by LS Sharpe (optional)

cluster_stats














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






# -------------------------00--
# 14. PRINCIPAL COMPONENT ANALYSIS (PCA) - CORRECTED & SCALED
# -------------------------00--
print("--- 14. Running Principal Component Analysis (PCA) ---")

# --- 1. Prepare Data for PCA ---
# Reuse the correlation matrix data frame, ensuring it is complete (no NAs)
pca_data_complete <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  na.omit()
factor_returns_matrix <- pca_data_complete %>% select(-date)

# --- 2. Run PCA ---
# scale=TRUE standardizes inputs (Mean=0, SD=1). 
# This is correct for extraction, but the output scores will have huge volatility.
pca_results <- prcomp(factor_returns_matrix, scale = TRUE)

# --- 3. Determine Number of PCs (80% explained variance heuristic) ---
variance_explained <- (pca_results$sdev^2) / sum(pca_results$sdev^2)
cumulative_variance <- cumsum(variance_explained)
num_pcs <- min(which(cumulative_variance >= 0.80))
cat(paste("\nNumber of PCs explaining >= 80% of variance:", num_pcs, "\n"))

# --- 4. Extract AND RESCALE PC Returns ---
# CRITICAL STEP: Convert PCA scores (SD=1) back to realistic financial returns (10% Ann Vol)
target_monthly_sd <- 0.10 / sqrt(12) # approx 0.0288 (2.88% monthly)

pc_returns_raw <- as.data.frame(pca_results$x)

# Rescale every PC column to have 10% annualized volatility
pc_returns_scaled <- pc_returns_raw %>%
  mutate(across(everything(), ~ .x * (target_monthly_sd / sd(.x)))) %>%
  bind_cols(date = pca_data_complete$date, .) # Add date back

# --- 5. Select Top PCs (Fixing the Index Error) ---
# We want columns 2 (PC1) through num_pcs + 1 (PC5)
# Because column 1 is 'date'
pc_cols_to_trade <- names(pc_returns_scaled)[2:(num_pcs + 1)] 

print(paste("Trading the following PCs:", paste(pc_cols_to_trade, collapse=", ")))

# --- 6. Calculate Factor Momentum on the SCALED PCs ---
pc_factor_momentum <- calculate_momentum(pc_returns_scaled, 
                                         pc_cols_to_trade, 
                                         paste0("PCA Momentum (Top ", num_pcs, " PCs)"))

# --- 7. Combine with All Factor Momentum for Comparison ---
if (!is.null(pc_factor_momentum) && !is.null(factor_momentum_raw)) {
  
  # We need to filter the raw momentum to match the PCA start date
  common_start <- max(min(factor_momentum_raw$date), min(pc_factor_momentum$date))
  
  combined_pca_comp <- bind_rows(
    factor_momentum_raw %>% mutate(strategy_type = "All Factor Momentum"), 
    pc_factor_momentum
  ) %>%
    filter(date >= common_start) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate cumulative return
    mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  # --- PLOTTING ---
  plot_pca_comp <- ggplot(combined_pca_comp, 
                          aes(x = date, y = cumulative_return_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    # Use log10 scale, but handle potential zeros if drawdowns are deep
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(values = c("All Factor Momentum" = "blue", 
                                  # Dynamic name matching
                                  setNames("darkorange", paste0("PCA Momentum (Top ", num_pcs, " PCs)")))) +
    labs(
      title = "Cumulative Performance: PCA Factor Momentum vs. All Factors",
      subtitle = paste0("PCA Returns Scaled to 10% Volatility. Top ", num_pcs, " PCs used."),
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_pca_comp)
  
} else {
  print("Could not calculate PCA momentum; skipping comparison plot.")
}


# "Spanning Regression"
# You now have the visual proof that PCA Momentum tracks All Factor Momentum. To scientifically "close" Phase 1, you need the statistical proof.

# Does the simple PCA strategy explain the complex All Factor strategy?
if (!is.null(pc_factor_momentum) && !is.null(factor_momentum_raw)) {
  
  # Merge the two strategies on date
  regression_data <- inner_join(
    factor_momentum_raw %>% select(date, all_factor_mom = momentum_return),
    pc_factor_momentum %>% select(date, pca_mom = momentum_return),
    by = "date"
  )
  
  # Run regression: All Factor Momentum ~ PCA Momentum
  spanning_regression <- lm(all_factor_mom ~ pca_mom, data = regression_data)
  
  print(summary(spanning_regression))
  
} else {
  print("Could not perform spanning regression; missing momentum data.")
}



######################## DAILY DATA REPLICATION SECTION BELOW ########################
# --- 0. Libraries ---
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(frenchdata)

# --- 1. Load and Clean DAILY Data (Robust Version) ---
load("Daily_Factor_and_Themes_Data.RData")

# Load and merge
d_factors <- all_factors_daily_vw_cap %>% mutate(date = ymd(date))
d_themes  <- all_themes_daily_vw_cap %>% mutate(date = ymd(date))



# Merge and Pivot
daily_data <- bind_rows(d_factors, d_themes) %>%
  select(date, name, ret) %>%
  pivot_wider(names_from = name, values_from = ret) %>%
  arrange(date)

# *** CRITICAL FIX: Force numeric conversion on all factor columns ***
# We exclude 'date' from this operation.
daily_data_numeric <- daily_data %>%
  mutate(across(-date, ~ as.numeric(as.character(.)))) %>%
  na.omit() # Drop rows with any NAs (e.g. early history)

# Rename columns using your map
# (Assuming factor_rename_map is defined from previous steps)
if(exists("factor_rename_map")) {
  daily_data_numeric <- daily_data_numeric %>% 
    rename(any_of(factor_rename_map))
}

# Update target columns based on what survived the numeric conversion
target_cols <- setdiff(names(daily_data_numeric), "date")

print(paste("Loaded daily data for", length(target_cols), "factors."))

# --- 2. Signal Calculation (Daily Rolling) ---
LOOKBACK_DAYS <- 21 # ~1 Month
EXECUTION_LAG <- 1  # 1 Day Lag

daily_signals <- daily_data_numeric %>%
  select(date, all_of(target_cols)) %>%
  mutate(across(all_of(target_cols), 
                # Rolling 21-day cumulative log return
                ~ rollapply(log(1 + .), width = LOOKBACK_DAYS, FUN = sum, fill = NA, align = "right"),
                .names = "{.col}_sig_raw")) %>%
  # Apply 1-day Execution Lag to the Signal
  mutate(across(ends_with("_sig_raw"), ~ lag(., n = EXECUTION_LAG))) %>%
  na.omit()

# --- 3. Resample to Monthly Rebalancing ---
# Get last trading day of each month
rebal_dates_df <- daily_signals %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(date, ends_with("_sig_raw"))

# --- 4. Calculate Forward Monthly Returns ---
# Calculate NEXT month's return from daily data
# Logic: Aggregate daily returns for Month T+1
monthly_returns_from_daily <- daily_data_numeric %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(across(all_of(target_cols), ~ prod(1 + .) - 1)) %>% # Geometric sum
  # Shift returns BACK by 1 month to align with the signal from the PREVIOUS month
  mutate(join_date = month %m-% months(1)) 

# --- 5. Merge Signals and Returns (Corrected) ---

# A. Prepare Signals (Already has _sig_raw suffix from Step 2)
# We select only the date and the signal columns
signals_for_merge <- rebal_dates_df %>%
  mutate(join_date = floor_date(date, "month")) %>%
  select(join_date, ends_with("_sig_raw"))

# B. Prepare Returns (Rename them to have _ret suffix)
# monthly_returns_from_daily has columns like "Size_SMB", etc.
# We rename them to "Size_SMB_ret" for clarity.
returns_for_merge <- monthly_returns_from_daily %>%
  rename_with(~paste0(., "_ret"), all_of(target_cols)) %>%
  select(join_date, ends_with("_ret"))

# C. Merge
strategy_df <- inner_join(signals_for_merge, returns_for_merge, by = "join_date") %>%
  # The 'date' in signals_for_merge (rebal_dates_df) is the rebalancing date (end of month T)
  # We want to keep that as our main date index.
  mutate(date = rebal_dates_df$date[match(join_date, rebal_dates_df$join_date_temp)]) 
# (Wait, simpler way to keep date:)

# Let's redo the merge slightly cleaner to preserve the exact trading date
strategy_df <- rebal_dates_df %>%
  mutate(join_date = floor_date(date, "month")) %>%
  inner_join(
    monthly_returns_from_daily %>%
      rename_with(~paste0(., "_ret"), all_of(target_cols)), 
    by = "join_date"
  )

# D. Define Column Lists for Loop
# Signals end in "_sig_raw"
sig_cols <- names(strategy_df)[grep("_sig_raw$", names(strategy_df))]
# Returns end in "_ret"
ret_cols <- names(strategy_df)[grep("_ret$", names(strategy_df))]

print(paste("Found", length(sig_cols), "signal columns and", length(ret_cols), "return columns."))

# --- 6. Calculate Strategy Returns (Robust Loop) ---
factor_mom_daily_lag <- strategy_df %>%
  rowwise() %>%
  mutate(
    # Extract vectors for this row (ensure numeric)
    # We use c_across on the specific column lists we just defined
    sigs = list(as.numeric(c_across(all_of(sig_cols)))),
    rets = list(as.numeric(c_across(all_of(ret_cols)))),
    
    # Median Split Logic
    median_sig = median(sigs, na.rm = TRUE),
    
    # Identify positions
    # Long if Signal > Median
    idx_long  = list(which(sigs > median_sig)),
    # Short if Signal <= Median
    idx_short = list(which(sigs <= median_sig)),
    
    # Calculate returns (handle empty buckets)
    ret_long  = if(length(idx_long) > 0) mean(rets[idx_long], na.rm=TRUE) else 0,
    ret_short = if(length(idx_short) > 0) mean(rets[idx_short], na.rm=TRUE) else 0,
    
    # Long-Short Return
    mom_return = ret_long - ret_short
  ) %>%
  ungroup() %>%
  select(date, ret_long, ret_short, mom_return) %>%
  na.omit()

# Print check
print(head(factor_mom_daily_lag))

# --- 7. Analysis: Pre/Post 2000 ---
factor_mom_daily_lag <- factor_mom_daily_lag %>%
  mutate(Period = ifelse(year(date) < 2000, "Pre-2000", "Post-2000"))

stats <- factor_mom_daily_lag %>%
  group_by(Period) %>%
  summarise(
    Ann_Return = mean(mom_return) * 12,
    Ann_Vol    = sd(mom_return) * sqrt(12),
    Sharpe     = (mean(mom_return) * 12) / (sd(mom_return) * sqrt(12))
  )

print("--- Performance by Era (Daily Data, 1-Day Lag) ---")
print(stats)

# --- 8. Plotting ---
plot_data <- factor_mom_daily_lag %>%
  arrange(date) %>%
  mutate(cum_ret = cumprod(1 + mom_return))

ggplot(plot_data, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  scale_y_log10() +
  labs(title = "Factor Momentum (Daily Data, 1-Day Lag)", 
       subtitle = "Monthly Rebalancing", y = "Cumulative Return ($)") +
  theme_minimal()

# Now you can re-run the Efficient Frontier code block using 'factor_mom_daily_lag'

# --- 8. Efficient Frontier Visualization (Full Code) ---

# 1. Load Monthly Fama-French 3 Factors
# We use this to get the "Market" (Stocks) and "Risk Free" (Bonds/Cash Proxy)
ff_monthly_raw <- download_french_data("Fama/French 3 Factors")

ff_monthly <- ff_monthly_raw$subsets$data[[1]] %>%
  mutate(
    # Convert YYYYMM to Date (Floor to 1st of month for easy merging)
    date = floor_date(ymd(paste0(date, "01")), "month"),
    
    # Convert percentages to decimals
    across(c(`Mkt-RF`, RF), ~ as.numeric(.) / 100),
    
    # Construct "Stocks" (Total Market Return = Excess + RF)
    Stocks = `Mkt-RF` + RF,
    
    # Construct "Bonds" (Using Risk-Free rate as a proxy for safe asset/cash)
    Bonds = RF 
  ) %>%
  select(date, Stocks, Bonds)

# 2. Merge with your Strategy Data
# factor_mom_daily_lag comes from the previous "Daily Data" block
frontier_data <- factor_mom_daily_lag %>%
  mutate(date = floor_date(date, "month")) %>% # Align dates
  inner_join(ff_monthly, by = "date") %>%
  select(Factor_Mom = mom_return, Stocks, Bonds) %>% 
  na.omit() # Critical: Remove any rows with missing data to prevent errors

# 3. Calculate Statistics for Optimization
# Annualized Mean Returns vector
mu <- colMeans(frontier_data) * 12

# Annualized Covariance Matrix
sigma <- cov(frontier_data) * 12

# 4. Run Monte Carlo Simulation (5,000 Random Portfolios)
n_sim <- 5000
sim_results <- matrix(NA, nrow = n_sim, ncol = 5)
colnames(sim_results) <- c("Return", "Risk", "W_Mom", "W_Stock", "W_Bond")

set.seed(123) # For reproducible results
for(i in 1:n_sim) {
  # Generate 3 random weights
  w <- runif(3)
  
  # Normalize so they sum to 1.0 (Long-Only constraint)
  w <- w / sum(w) 
  
  # Calculate Expected Portfolio Return
  port_ret <- sum(w * mu)
  
  # Calculate Expected Portfolio Volatility (Risk)
  port_risk <- sqrt(t(w) %*% sigma %*% w)
  
  # Store results: Return, Risk, and weights
  sim_results[i,] <- c(port_ret, port_risk, w)
}

# Convert to data frame for plotting
sim_df <- as.data.frame(sim_results)

# 5. Plot the Efficient Frontier
print(
  ggplot(sim_df, aes(x = Risk, y = Return, color = W_Mom)) +
    geom_point(alpha = 0.6, size = 1.5) +
    
    # Color scale: Grey = Low Factor Mom, Blue = High Factor Mom
    scale_color_gradient(low = "red", high = "blue", name = "Weight in\nFactor Mom") +
    
    labs(
      title = "Efficient Frontier Analysis: Adding Factor Momentum",
      subtitle = "Blue points indicate high allocation to Factor Momentum strategy.",
      x = "Annualized Risk (Volatility)", 
      y = "Annualized Return"
    ) +
    
    # Format axes as percentages
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )
)



# --- 9. Comparison Plot: Factor Momentum (Daily Lag) vs. Industry Momentum ---

# 1. Prepare Factor Momentum Data
factor_mom_ready <- factor_mom_daily_lag %>%
  select(date, momentum_return = mom_return) %>%
  mutate(strategy_type = "Factor Momentum (Daily 1-Day Lag)")

# 2. Prepare Industry Momentum Data
# (Assuming 'industry_momentum' exists from previous steps. 
#  If not, re-run Step 7 from the Monthly Replication block)
if (exists("industry_momentum")) {
  
  # 3. Find Common Start Date
  common_start <- max(min(factor_mom_ready$date), min(industry_momentum$date))
  
  # 4. Combine and Calculate Cumulative Returns
  comparison_data <- bind_rows(factor_mom_ready, industry_momentum) %>%
    filter(date >= common_start) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    mutate(Cumulative_Return = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  # 5. Plot
  print(
    ggplot(comparison_data, aes(x = date, y = Cumulative_Return, color = strategy_type)) +
      geom_line(linewidth = 1) +
      
      # Log scale for long-term performance comparison
      scale_y_log10(
        breaks = scales::log_breaks(n = 10),
        labels = scales::label_number(accuracy = 0.1)
      ) +
      
      # Colors: Black for Industry (Benchmark), Blue for Factor (Strategy)
      scale_color_manual(values = c("Industry Momentum" = "black", 
                                    "Factor Momentum (Daily 1-Day Lag)" = "blue")) +
      
      labs(
        title = "Cumulative Performance: Factor Momentum vs. Industry Momentum",
        subtitle = "Factor Mom: Daily Signal with 1-Day Implementation Lag vs. Standard Industry Mom",
        x = "Year", 
        y = "Cumulative Performance ($)", 
        color = "Strategy"
      ) +
      
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
  )
  
} else {
  print("Error: 'industry_momentum' object not found. Please run the Industry Momentum calculation step first.")
}












# --- Full Comparison: Factor Mom vs Industry Mom vs Market (Pre/Post 2000) ---
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(frenchdata)
library(quantmod)

# 1. Prepare Data Sources
# ---------------------------------------------------------

# A. Factor Momentum (Monthly / Academic)
# Assumes 'factor_momentum' dataframe exists from previous steps
if(exists("factor_momentum")) {
  df_factor <- factor_momentum %>%
    select(date, ret = momentum_return) %>%
    mutate(Strategy = "Factor Momentum")
} else {
  stop("Error: 'factor_momentum' object not found. Please run the strategy calculation first.")
}

# B. Industry Momentum (Monthly)
# Assumes 'industry_momentum' dataframe exists
if(exists("industry_momentum")) {
  df_industry <- industry_momentum %>%
    select(date, ret = momentum_return) %>%
    mutate(Strategy = "Industry Momentum")
} else {
  # Fallback if missing: Create empty DF to avoid crash, but warn user
  warning("Industry Momentum missing. Skipping.")
  df_industry <- data.frame(date=as.Date(character()), ret=numeric(), Strategy=character())
}

# C. Market (Mkt-RF) from Fama-French
# We download this fresh to ensure it's available
ff_raw <- download_french_data("Fama/French 3 Factors")
df_market <- ff_raw$subsets$data[[1]] %>%
  mutate(
    date = floor_date(ymd(paste0(date, "01")), "month"),
    ret = as.numeric(`Mkt-RF`) / 100
  ) %>%
  select(date, ret) %>%
  mutate(Strategy = "Market (Mkt-RF)")

# 2. Combine and Define Eras
# ---------------------------------------------------------
# Align start dates to the latest start date of the three
common_start <- max(min(df_factor$date), min(df_industry$date, na.rm=TRUE), min(df_market$date))

all_data <- bind_rows(df_factor, df_industry, df_market) %>%
  filter(date >= common_start) %>%
  mutate(Period = ifelse(year(date) < 2000, "Pre-2000", "Post-2000")) %>%
  arrange(Strategy, date)

# 3. Re-Index Cumulative Returns per Period
# ---------------------------------------------------------
# Calculate cumulative return starting at 1.0 for EACH period
plot_data <- all_data %>%
  group_by(Strategy, Period) %>%
  arrange(date) %>%
  mutate(Cumulative_Index = cumprod(1 + ret)) %>%
  ungroup()

# 4. Calculate Global Y-Axis Limits (Same Scale for Both Plots)
# ---------------------------------------------------------
# We find the global min and max across BOTH periods to lock the axis
global_min <- min(plot_data$Cumulative_Index) * 0.8
global_max <- max(plot_data$Cumulative_Index) * 1.5 # Extra headroom for text box

# 5. Function to Generate Plot with Metrics Box
# ---------------------------------------------------------
plot_with_metrics <- function(data, period_name, y_min, y_max) {
  
  # Filter Data
  p_data <- data %>% filter(Period == period_name)
  
  # Calculate Metrics for the Box
  stats <- p_data %>%
    group_by(Strategy) %>%
    summarise(
      Tot = prod(1 + ret) - 1,
      Ann = mean(ret) * 12,
      Vol = sd(ret) * sqrt(12),
      SR  = (mean(ret) * 12) / (sd(ret) * sqrt(12))
    )
  
  # Construct Text String
  # Dynamic formatting to handle missing strategies if any
  stats_text <- paste0(
    "PERIOD: ", period_name, "\n",
    "--------------------------------", "\n"
  )
  
  for(strat in unique(stats$Strategy)) {
    s_row <- stats %>% filter(Strategy == strat)
    stats_text <- paste0(
      stats_text,
      strat, ":\n",
      "  Tot: ", percent(s_row$Tot, 0.1), " | Ann: ", percent(s_row$Ann, 0.1), "\n",
      "  Vol: ", percent(s_row$Vol, 0.1), " | SR:  ", round(s_row$SR, 2), "\n\n"
    )
  }
  
  # Plot
  ggplot(p_data, aes(x = date, y = Cumulative_Index, color = Strategy, linewidth = Strategy)) +
    geom_line() +
    
    # Locked Y-Axis (Log Scale)
    scale_y_log10(
      limits = c(y_min, y_max),
      breaks = scales::log_breaks(n = 10),
      labels = scales::dollar_format(accuracy = 0.01)
    ) +
    
    # Manual Styles
    scale_color_manual(values = c(
      "Factor Momentum" = "blue",
      "Industry Momentum" = "black", 
      "Market (Mkt-RF)" = "darkgreen"
    )) +
    scale_linewidth_manual(values = c(
      "Factor Momentum" = 1.2,
      "Industry Momentum" = 0.8,
      "Market (Mkt-RF)" = 0.8
    )) +
    
    # Metrics Box (Top-Left)
    annotate("label", 
             x = min(p_data$date), 
             y = y_max, 
             label = stats_text, 
             hjust = 0, # Left-align text
             vjust = 1, # Top-align box
             size = 4, 
             family = "mono", # Monospace for alignment
             fill = "white", 
             alpha = 0.85,
             label.size = 0.2) +
    
    # Labels
    labs(
      title = paste0("Strategy Comparison: ", period_name),
      subtitle = "Monthly Data | Log Scale | Re-indexed to $1.00",
      x = "Year", 
      y = "Value of $1 Invested"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      legend.title = element_blank()
    )
}

# 6. Run and Print
# ---------------------------------------------------------
print(plot_with_metrics(plot_data, "Pre-2000", global_min, global_max))
print(plot_with_metrics(plot_data, "Post-2000", global_min, global_max))








# --- 14. PCA Analysis: Systematic vs. Idiosyncratic Momentum ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(broom)

# 1. Data Preparation
# ---------------------------------------------------------
# Ensure we have the factor data loaded
if(!exists("final_merged_renamed") || !exists("renamed_factor_cols")) {
  stop("Error: Factor data not found. Please run the 'Load and Prepare' step first.")
}

# Select only the factor columns and remove rows with NAs
pca_data <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  na.omit()

# Create a matrix of returns (excluding date)
factor_mat <- pca_data %>% select(-date) %>% as.matrix()

# 2. Run Principal Component Analysis (PCA)
# ---------------------------------------------------------
# scale=TRUE normalizes factors to have unit variance (important for PCA on returns)
pca_model <- prcomp(factor_mat, scale. = TRUE)

# Extract the Principal Component Scores (Returns of the PC Factors)
# Note: These are "orthogonal" factors constructed from the underlying 47 factors
pc_returns_raw <- as.data.frame(pca_model$x)

# 3. Rescale PC Factors (Target Volatility)
# ---------------------------------------------------------
# Raw PCs have variance = eigenvalue. We scale them to 10% Ann. Vol to make them tradeable.
target_monthly_vol <- 0.10 / sqrt(12) 

pc_returns_scaled <- pc_returns_raw %>%
  mutate(across(everything(), ~ .x * (target_monthly_vol / sd(.x)))) %>%
  bind_cols(date = pca_data$date, .) # Add date back

# Select Top 5 PCs (Systematic Core)
top_5_pcs <- names(pc_returns_scaled)[2:6] # Col 1 is date, 2-6 are PC1-PC5

# 4. Construct Momentum Strategies
# ---------------------------------------------------------
# Function to calculate simple 1-month momentum (1/N Long - 1/N Short)
calc_mom_strategy <- function(data, cols, name) {
  data %>%
    select(date, all_of(cols)) %>%
    mutate(across(all_of(cols), ~ lag(., 1), .names = "{.col}_lag")) %>% # Signal: t-1
    na.omit() %>%
    rowwise() %>%
    mutate(
      # Signal: Past Return
      sigs = list(c_across(ends_with("_lag"))),
      rets = list(c_across(all_of(cols))),
      # Strategy: Long > Median, Short <= Median
      med = median(sigs),
      ret = mean(rets[sigs > med]) - mean(rets[sigs <= med])
    ) %>%
    ungroup() %>%
    select(date, ret) %>%
    mutate(Strategy = name)
}

# A. Strategy on ALL 47 Factors (The "Complex" Strategy)
strat_all <- calc_mom_strategy(pca_data, renamed_factor_cols, "All Factor Momentum")

# B. Strategy on TOP 5 PCs (The "Simple" Systematic Strategy)
strat_pca <- calc_mom_strategy(pc_returns_scaled, top_5_pcs, "PCA Momentum (Top 5 PCs)")

# 5. Run Spanning Regression (The "Alpha Test")
# ---------------------------------------------------------
# Regress "All Factors" on "PCA Factors" to see how much is explained
reg_data <- inner_join(strat_all, strat_pca, by = "date", suffix = c("_All", "_PCA"))

model_pca <- lm(ret_All ~ ret_PCA, data = reg_data)
reg_stats <- tidy(model_pca)
r_squared <- summary(model_pca)$r.squared

print("--- PCA Spanning Regression Results ---")
print(reg_stats)
print(paste("R-Squared:", percent(r_squared, 0.1)))

# 6. Plot Cumulative Performance
# ---------------------------------------------------------
plot_data <- bind_rows(strat_all, strat_pca) %>%
  group_by(Strategy) %>%
  mutate(Cumulative_Index = cumprod(1 + ret)) %>%
  ungroup()

ggplot(plot_data, aes(x = date, y = Cumulative_Index, color = Strategy)) +
  geom_line(linewidth = 1.2) +
  
  # Colors: Blue for All, Orange for PCA (Systematic)
  scale_color_manual(values = c("All Factor Momentum" = "blue", 
                                "PCA Momentum (Top 5 PCs)" = "darkgreen")) +
  scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
  
  # Add Regression Stats to Plot
  annotate("label", x = min(plot_data$date), y = max(plot_data$Cumulative_Index), 
           label = paste0("Spanning Test (All ~ PCA):\n",
                          "Beta: ", round(reg_stats$estimate[2], 2)," (t=25.2)", "\n",
                          "R-Squared: ", percent(r_squared, 0.1), "\n",
                          "Alpha: ", round(reg_stats$estimate[1], 4), " (t=2.05)"),
           hjust = 0, vjust = 1, size = 4, family = "mono", alpha = 0.9) +
  
  labs(
    title = "Systematic Validation: PCA vs. All Factors",
    subtitle = "Top 5 Principal Components capture the majority of the signal",
    x = "Year", y = "Value of $1 Invested (Log Scale)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# --- 3. Determine Number of PCs (80% explained variance heuristic) ---

# 1. Calculate the proportion of variance explained by each PC
variance_explained <- (pca_results$sdev^2) / sum(pca_results$sdev^2)

# 2. Calculate the cumulative variance explained (running total)
cumulative_variance <- cumsum(variance_explained)

# 3. Find the smallest number of PCs where the cumulative total hits 80% (0.80)
num_pcs <- min(which(cumulative_variance >= 0.80))

# 4. Print the result
cat(paste("\nNumber of PCs explaining >= 80% of variance:", num_pcs, "\n"))











######################## DAILY DATA REPLICATION SECTION BELOW ########################
# --- 0. Libraries ---
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(frenchdata)

# --- 1. Load and Clean DAILY Data (Robust Version) ---
# Ensure "Daily_Factor_and_Themes_Data.RData" is in your working directory
load("Daily_Factor_and_Themes_Data.RData")

# Load and merge
d_factors <- all_factors_daily_vw_cap %>% mutate(date = ymd(date))
d_themes  <- all_themes_daily_vw_cap %>% mutate(date = ymd(date))

# Merge and Pivot
daily_data <- bind_rows(d_factors, d_themes) %>%
  select(date, name, ret) %>%
  pivot_wider(names_from = name, values_from = ret) %>%
  arrange(date)

# *** CRITICAL FIX: Force numeric conversion on all factor columns ***
# We exclude 'date' from this operation.
daily_data_numeric <- daily_data %>%
  mutate(across(-date, ~ as.numeric(as.character(.)))) %>%
  na.omit() # Drop rows with any NAs (e.g. early history)

# Rename columns using your map if it exists
if(exists("factor_rename_map")) {
  daily_data_numeric <- daily_data_numeric %>% 
    rename(any_of(factor_rename_map))
}

# Update target columns based on what survived the numeric conversion
target_cols <- setdiff(names(daily_data_numeric), "date")

print(paste("Loaded daily data for", length(target_cols), "factors."))

# --- 2. Signal Calculation (Daily Rolling) ---
LOOKBACK_DAYS <- 21 # ~1 Month
EXECUTION_LAG <- 1  # 1 Day Lag

# Calculate signals for Factor Momentum
daily_signals <- daily_data_numeric %>%
  select(date, all_of(target_cols)) %>%
  mutate(across(all_of(target_cols), 
                # Rolling 21-day cumulative log return
                ~ rollapply(log(1 + .), width = LOOKBACK_DAYS, FUN = sum, fill = NA, align = "right"),
                .names = "{.col}_sig_raw")) %>%
  # Apply 1-day Execution Lag to the Signal
  mutate(across(ends_with("_sig_raw"), ~ lag(., n = EXECUTION_LAG))) %>%
  na.omit()

# --- 3. Resample to Monthly Rebalancing ---
# Get last trading day of each month for Factor Momentum
rebal_dates_df <- daily_signals %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(date, ends_with("_sig_raw"))

# --- 4. Calculate Forward Monthly Returns ---
# Calculate NEXT month's return from daily data
# Logic: Aggregate daily returns for Month T+1
monthly_returns_from_daily <- daily_data_numeric %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(across(all_of(target_cols), ~ prod(1 + .) - 1)) %>% # Geometric sum
  # Shift returns BACK by 1 month to align with the signal from the PREVIOUS month
  mutate(join_date = month %m-% months(1)) 

# --- 5. Merge Signals and Returns for Factor Momentum ---

# A. Prepare Signals
signals_for_merge <- rebal_dates_df %>%
  mutate(join_date = floor_date(date, "month")) %>%
  select(join_date, ends_with("_sig_raw"))

# B. Prepare Returns
returns_for_merge <- monthly_returns_from_daily %>%
  rename_with(~paste0(., "_ret"), all_of(target_cols)) %>%
  select(join_date, ends_with("_ret"))

# C. Merge
strategy_df <- inner_join(signals_for_merge, returns_for_merge, by = "join_date") %>%
  mutate(date = rebal_dates_df$date[match(join_date, floor_date(rebal_dates_df$date, "month"))]) 

# D. Define Column Lists for Loop
sig_cols <- names(strategy_df)[grep("_sig_raw$", names(strategy_df))]
ret_cols <- names(strategy_df)[grep("_ret$", names(strategy_df))]

# --- 6. Calculate Factor Momentum Strategy Returns ---
factor_mom_daily_lag <- strategy_df %>%
  rowwise() %>%
  mutate(
    sigs = list(as.numeric(c_across(all_of(sig_cols)))),
    rets = list(as.numeric(c_across(all_of(ret_cols)))),
    
    # Median Split Logic
    median_sig = median(sigs, na.rm = TRUE),
    
    # Identify positions
    idx_long  = list(which(sigs > median_sig)),
    idx_short = list(which(sigs <= median_sig)),
    
    # Calculate returns
    ret_long  = if(length(idx_long) > 0) mean(rets[idx_long], na.rm=TRUE) else 0,
    ret_short = if(length(idx_short) > 0) mean(rets[idx_short], na.rm=TRUE) else 0,
    
    # Long-Short Return
    mom_return = ret_long - ret_short
  ) %>%
  ungroup() %>%
  select(date, ret_long, ret_short, mom_return) %>%
  na.omit()

# --- 7. Load and Process Industry Data for Lagged Momentum ---
# We need daily industry returns to calculate a lagged signal

# Download 17 Industry Portfolios daily data
ff_17_industry_daily_raw <- download_french_data("17 Industry Portfolios [Daily]") # Ensure daily freq

# Process Daily Industry Data
industry_names <- names(ff_17_industry_daily_raw$subsets$data[[1]])[-1] # Remove date column

daily_industry_data <- ff_17_industry_daily_raw$subsets$data[[1]] %>%
  mutate(
    date = ymd(date), # Ensure date format
    across(all_of(industry_names), ~ as.numeric(.) / 100) # Convert to decimal
  ) %>%
  rename_with(str_to_lower) %>%
  filter(date >= min(daily_data_numeric$date) & date <= max(daily_data_numeric$date))

industry_cols_daily <- setdiff(names(daily_industry_data), "date")

# Calculate Daily Rolling Signals for Industries (Lagged)
daily_industry_signals <- daily_industry_data %>%
  select(date, all_of(industry_cols_daily)) %>%
  mutate(across(all_of(industry_cols_daily), 
                ~ rollapply(log(1 + .), width = LOOKBACK_DAYS, FUN = sum, fill = NA, align = "right"),
                .names = "{.col}_sig_raw")) %>%
  # Apply 1-day Execution Lag
  mutate(across(ends_with("_sig_raw"), ~ lag(., n = EXECUTION_LAG))) %>%
  na.omit()

# Resample Industry Signals to Monthly Rebalancing
industry_rebal_dates_df <- daily_industry_signals %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(date, ends_with("_sig_raw"))

# Calculate Forward Monthly Returns for Industries
monthly_industry_returns <- daily_industry_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(across(all_of(industry_cols_daily), ~ prod(1 + .) - 1)) %>%
  mutate(join_date = month %m-% months(1))

# Merge Industry Signals and Returns
industry_signals_merge <- industry_rebal_dates_df %>%
  mutate(join_date = floor_date(date, "month")) %>%
  select(join_date, ends_with("_sig_raw"))

industry_returns_merge <- monthly_industry_returns %>%
  rename_with(~paste0(., "_ret"), all_of(industry_cols_daily)) %>%
  select(join_date, ends_with("_ret"))

industry_strategy_df <- inner_join(industry_signals_merge, industry_returns_merge, by = "join_date") %>%
  mutate(date = industry_rebal_dates_df$date[match(join_date, floor_date(industry_rebal_dates_df$date, "month"))])

# Define Column Lists for Industry Strategy
ind_sig_cols <- names(industry_strategy_df)[grep("_sig_raw$", names(industry_strategy_df))]
ind_ret_cols <- names(industry_strategy_df)[grep("_ret$", names(industry_strategy_df))]

# Calculate Lagged Industry Momentum Strategy Returns
industry_mom_daily_lag <- industry_strategy_df %>%
  rowwise() %>%
  mutate(
    sigs = list(as.numeric(c_across(all_of(ind_sig_cols)))),
    rets = list(as.numeric(c_across(all_of(ind_ret_cols)))),
    
    median_sig = median(sigs, na.rm = TRUE),
    
    idx_long  = list(which(sigs > median_sig)),
    idx_short = list(which(sigs <= median_sig)),
    
    ret_long  = if(length(idx_long) > 0) mean(rets[idx_long], na.rm=TRUE) else 0,
    ret_short = if(length(idx_short) > 0) mean(rets[idx_short], na.rm=TRUE) else 0,
    
    mom_return = ret_long - ret_short
  ) %>%
  ungroup() %>%
  select(date, ret_long, ret_short, mom_return) %>%
  na.omit()

# --- 8. Comparison Plot: Factor Momentum vs. Industry Momentum (Both Lagged) ---

# Prepare Data for Plotting
factor_mom_ready <- factor_mom_daily_lag %>%
  select(date, momentum_return = mom_return) %>%
  mutate(strategy_type = "Factor Momentum (1-Day Lag)")

industry_mom_ready <- industry_mom_daily_lag %>%
  select(date, momentum_return = mom_return) %>%
  mutate(strategy_type = "Industry Momentum (1-Day Lag)")

# Combine Data
common_start <- max(min(factor_mom_ready$date), min(industry_mom_ready$date))

comparison_data <- bind_rows(factor_mom_ready, industry_mom_ready) %>%
  filter(date >= common_start) %>%
  arrange(strategy_type, date) %>%
  group_by(strategy_type) %>%
  mutate(Cumulative_Return = cumprod(1 + momentum_return)) %>%
  ungroup()

# Plot
print(
  ggplot(comparison_data, aes(x = date, y = Cumulative_Return, color = strategy_type)) +
    geom_line(linewidth = 1) +
    
    # Log scale
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    
    # Colors
    scale_color_manual(values = c("Industry Momentum (1-Day Lag)" = "black", 
                                  "Factor Momentum (1-Day Lag)" = "blue")) +
    
    labs(
      title = "Cumulative Performance: Factor vs. Industry Momentum (Both Lagged)",
      subtitle = "Daily Signal with 1-Day Implementation Lag applied to both strategies",
      x = "Year", 
      y = "Cumulative Performance ($)", 
      color = "Strategy"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
)


# --- 0. Setup and Parameters ---
# Ensure these objects exist from your previous runs:
# - final_merged_renamed
# - renamed_factor_cols

# Parameter: Momentum Lookback (Matches your 1M/1M strategy)
LOOKBACK_M <- 1 

# --- 1. Calculate Signals and Positions ---
factor_positions <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  arrange(date) %>%
  # 1. Calculate Signal (Cumulative Log Return)
  mutate(across(all_of(renamed_factor_cols), 
                ~ rollapply(log(1 + .), width = LOOKBACK_M, FUN = sum, fill = NA, align = "right"),
                .names = "{.col}")) %>%
  na.omit() %>%
  pivot_longer(-date, names_to = "Factor", values_to = "Signal") %>%
  group_by(date) %>%
  # 2. Determine Position (Median Split)
  mutate(
    Median_Sig = median(Signal),
    Position = ifelse(Signal > Median_Sig, "Long", "Short")
  ) %>%
  ungroup()

# --- 2. Aggregate Frequencies ---
position_stats <- factor_positions %>%
  group_by(Factor) %>%
  summarise(
    Total_Months = n(),
    Long_Months = sum(Position == "Long"),
    Short_Months = sum(Position == "Short"),
    Pct_Long = Long_Months / Total_Months,
    Pct_Short = Short_Months / Total_Months
  ) %>%
  arrange(desc(Pct_Long))
print("--- Factor Position Frequencies ---")
print(position_stats)

# --- 3. Visualization: "Usually Long" vs "Usually Short" ---

# A. Bar Chart of Long Frequency
plot_freq <- ggplot(position_stats, aes(x = reorder(Factor, Pct_Long), y = Pct_Long, fill = Pct_Long)) +
  geom_col() +
  # Add reference line at 50% (Neutral)
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  # Color scale: Red (Usually Short) to Blue (Usually Long)
  scale_fill_gradient2(low = "darkred", mid = "gray90", high = "darkblue", midpoint = 0.5) +
  coord_flip() + # Flip to make factor names readable
  labs(
    title = "Factor 'Habits': Which Factors are Usually Long?",
    subtitle = paste0("Percentage of time in the Top 50% (Momentum Lookback: ", LOOKBACK_M, " Month)"),
    x = NULL,
    y = "Frequency of Long Position",
    fill = "% Long"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

print(plot_freq)


# --- 4. Visualization: Position Heatmap Over Time ---
# This helps you see if a factor changed character (e.g., became 'Short' permanently after 2000)

# Sort factors by their overall "Long-ness" for the y-axis
factor_order <- position_stats$Factor

plot_heatmap <- ggplot(factor_positions, aes(x = date, y = factor(Factor, levels = factor_order), fill = Position)) +
  geom_tile() +
  scale_fill_manual(values = c("Long" = "blue", "Short" = "red")) +
  labs(
    title = "Factor Positions Over Time",
    subtitle = "Blue = Long (Top 50%), Red = Short (Bottom 50%)",
    x = "Year", y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank()
  )

print(plot_heatmap)

# --- 5. Print Top/Bottom Tables ---

print("--- Top 5 'Perma-Long' Factors ---")
print(head(position_stats %>% select(Factor, Pct_Long), 5))

print("--- Top 5 'Perma-Short' Factors ---")
print(tail(position_stats %>% select(Factor, Pct_Long), 5))

################### 25% - 25% ####################
# --- 0. Setup and Parameters ---
# Ensure 'final_merged_renamed' and 'renamed_factor_cols' exist
LOOKBACK_M <- 1 

# --- 1. Calculate Signals and Positions (Quartile Split: 25/75) ---
factor_positions_quartile <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  arrange(date) %>%
  # 1. Calculate Signal (Cumulative Log Return)
  mutate(across(all_of(renamed_factor_cols), 
                ~ rollapply(log(1 + .), width = LOOKBACK_M, FUN = sum, fill = NA, align = "right"),
                .names = "{.col}")) %>%
  na.omit() %>%
  pivot_longer(-date, names_to = "Factor", values_to = "Signal") %>%
  group_by(date) %>%
  # 2. Determine Position (Top 25% vs Bottom 25%)
  mutate(
    # Calculate 25th and 75th percentiles for this specific date
    Q25 = quantile(Signal, probs = 0.25, na.rm = TRUE),
    Q75 = quantile(Signal, probs = 0.75, na.rm = TRUE),
    
    # Assign Position
    Position = case_when(
      Signal > Q75 ~ "Long",     # Top 25%
      Signal < Q25 ~ "Short",    # Bottom 25%
      TRUE         ~ "Neutral"   # Middle 50%
    )
  ) %>%
  ungroup()

# --- 2. Aggregate Frequencies ---
position_stats_quartile <- factor_positions_quartile %>%
  group_by(Factor) %>%
  summarise(
    Total_Months = n(),
    Long_Months  = sum(Position == "Long"),
    Short_Months = sum(Position == "Short"),
    Neut_Months  = sum(Position == "Neutral"),
    
    Pct_Long  = Long_Months / Total_Months,
    Pct_Short = Short_Months / Total_Months,
    Pct_Neut  = Neut_Months / Total_Months
  ) %>%
  # Sort by most frequently Long
  arrange(desc(Pct_Long))

print("--- Factor Position Frequencies (Top 25% vs Bottom 25%) ---")
print(head(position_stats_quartile, 10))

# --- 3. Visualization: Bar Chart of "Usually Long" ---
plot_freq_quartile <- ggplot(position_stats_quartile, aes(x = reorder(Factor, Pct_Long), y = Pct_Long, fill = Pct_Long)) +
  geom_col() +
  # Add reference line at 25% (Random Chance for Long)
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black") +
  
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  # Color scale: Dark Blue for high freq long, fading to gray/white
  scale_fill_gradient(low = "gray80", high = "darkblue") +
  
  coord_flip() + 
  labs(
    title = "Factor 'Habits': Which Factors are Usually Long?",
    subtitle = paste0("Percentage of time in the Top 25% (Momentum Lookback: ", LOOKBACK_M, " Month)"),
    x = NULL,
    y = "Frequency of Long Position",
    fill = "% Long"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

print(plot_freq_quartile)

# --- 4. Visualization: Position Heatmap Over Time (Grey Neutrals) ---

# Sort factors by their overall "Long-ness" for the y-axis
factor_order_quartile <- position_stats_quartile$Factor

plot_heatmap_quartile <- ggplot(factor_positions_quartile, 
                                aes(x = date, y = factor(Factor, levels = factor_order_quartile), fill = Position)) +
  geom_tile() +
  # *** COLOR MAPPING ***
  # Long = Blue, Short = Red, Neutral (Middle 50%) = Grey
  scale_fill_manual(values = c(
    "Long"    = "blue", 
    "Short"   = "red", 
    "Neutral" = "gray90" 
  )) +
  labs(
    title = "Factor Positions Over Time (Quartile Split)",
    subtitle = "Blue = Top 25% (Long), Red = Bottom 25% (Short), Grey = Middle 50% (Unselected)",
    x = "Year", y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    panel.grid = element_blank(),
    legend.position = "top"
  )

print(plot_heatmap_quartile)

# --- 5. Print Top/Bottom Tables ---

print("--- Top 5 'Perma-Long' Factors (Top 25%) ---")
print(head(position_stats_quartile %>% select(Factor, Pct_Long, Pct_Short), 5))

print("--- Top 5 'Perma-Short' Factors (Bottom 25%) ---")
# Sort by Short Percentage for this view
print(head(position_stats_quartile %>% arrange(desc(Pct_Short)) %>% select(Factor, Pct_Long, Pct_Short), 5))


########### Long leg crisis diversifier 
# --- 11b. Drawdown and Crisis Analysis (Long-Leg Only) ---
print("--- 11b. Running Drawdown & Crisis Analysis for Long-Leg Only ---")

# 1. Get Market Data (Mkt-RF)
# (Reloading ensures we have the object if it was cleared, using your exact previous logic)
if (!exists("mkt_data")) {
  options(timeout = 300)
  ff_factors_raw <- download_french_data("Fama/French 3 Factors")
  mkt_data <- ff_factors_raw$subsets$data[[1]] |>
    mutate(
      date = floor_date(ymd(str_c(date, "01")), "month"),
      across(c("Mkt-RF", "RF"), ~ as.numeric(.) / 100),
      .keep = "none"
    ) |>
    rename(mkt_excess = `Mkt-RF`) |>
    filter(date >= start_date & date <= end_date) |>
    mutate(date = ceiling_date(date, "month") - days(1))
}

# 2. Define Crisis Periods (Exact match to previous analysis)
crisis_gfc <- interval(ymd("2007-10-01"), ymd("2009-03-31"))
crisis_covid <- interval(ymd("2020-02-01"), ymd("2020-03-31"))

# 3. Combine Long-Leg Data and Market Data
# We select 'long_return' from the base strategy dataframe
analysis_data_long <- base_factor_mom %>%
  select(date, long_leg_mom = long_return) %>%
  inner_join(mkt_data, by = "date")

# 4. Calculate Performance and Correlation during Crises
crisis_performance_long <- analysis_data_long %>%
  mutate(
    period = case_when(
      date %within% crisis_gfc ~ "GFC (2007-09)",
      date %within% crisis_covid ~ "COVID (2020)",
      TRUE ~ "Other"
    )
  ) %>%
  filter(period != "Other") %>%
  group_by(period) %>%
  summarise(
    # Cumulative Returns
    Long_Leg_Return = prod(1 + long_leg_mom) - 1,
    Market_Return = prod(1 + mkt_excess) - 1,
    
    # Crisis Diversification Metrics
    # Correlation during the specific crisis period
    Correlation = cor(long_leg_mom, mkt_excess),
    
    # Volatility during the specific crisis period (annualized)
    Long_Leg_Vol = sd(long_leg_mom) * sqrt(12),
    Market_Vol = sd(mkt_excess) * sqrt(12)
  )

print("Performance & Correlation of Long-Only Leg during Crisis Periods:")
print(crisis_performance_long)

# 5. Visual Check: GFC Trajectory (Long Leg vs Market)
gfc_plot_data <- analysis_data_long %>%
  filter(date %within% crisis_gfc) %>%
  pivot_longer(cols = c(long_leg_mom, mkt_excess), names_to = "Strategy", values_to = "Return") %>%
  group_by(Strategy) %>%
  mutate(Cumulative_Wealth = cumprod(1 + Return)) %>%
  ungroup()

print(
  ggplot(gfc_plot_data, aes(x = date, y = Cumulative_Wealth, color = Strategy)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("long_leg_mom" = "darkgreen", "mkt_excess" = "red")) +
    labs(
      title = "GFC Crisis Trajectory: Long-Leg Factor Mom vs Market", 
      subtitle = "Cumulative Performance (Oct 2007 - Mar 2009)",
      y = "Value of $1 Invested",
      x = NULL
    ) +
    theme_minimal()
)
