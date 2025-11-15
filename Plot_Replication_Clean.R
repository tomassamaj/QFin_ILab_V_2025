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

---
  
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

---
  
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

###########################################################################

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


