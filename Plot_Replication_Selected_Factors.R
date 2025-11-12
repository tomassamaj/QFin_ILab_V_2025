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

















# --- Load Required Libraries ---
# Make sure these are loaded from your previous script
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)
library(zoo) # For rollapply

# --- [Your existing code for Steps 1-5: Loading Data, Merging, Renaming] ---
# Assume final_merged_renamed, industry_cols, renamed_factor_cols exist here

# --- 6. Function to Calculate 1-Month Momentum Returns ---
# (Keep your existing calculate_momentum function as is)
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


# --- NEW: Function for Volatility Scaling ---
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
      # You might need to adjust limits if scaling causes issues
      # limits = c(0.1, max(combined_momentum_scaled$cumulative_return_scaled_log) * 1.1)
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

# --- [Your existing code for Correlation Heatmap] ---
# (Should work as before, using final_merged_renamed and renamed_factor_cols)









# --- Load Required Libraries ---
# (Ensure dplyr, lubridate, frenchdata, broom are loaded from your main script)
library(broom) # For tidying model output

# --- Assume 'final_merged_renamed' dataframe exists from previous steps ---
# It should contain your JKP factors (like Book_to_Market_HML) and dates.

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
# Example: Using the JKP Book-to-Market factor you renamed
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

# --- Sneak Peek into Next Steps ---
# You would loop this process for:
# 1. All relevant JKP factors in 'renamed_factor_cols'.
# 2. Different models (e.g., FF5 + UMD by adding CMA, RMW, UMD).
# 3. Potentially regressing your calculated *factor momentum strategy* returns.






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

# Continue from your provided line:
all_reg_data <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  inner_join(ff3_factors_monthly, by = "date")

cat("\n\n--- Running Full-Period FF3 Regressions ---\n")

# --- 3. Loop, Run Regressions, and Print Results ---

# Loop through each factor name in your list
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



# regression on ff5 -------------------------------------------------------

# adjust so it is end of month
factors_ff5_mom_eom <- factors_ff5_mom_monthly %>%
  mutate(date = ceiling_date(date, "month") - days(1))

# --- 3. Prepare Full Regression Data ---
# (This assumes 'final_merged_renamed' and 'renamed_factor_cols' exist)

# Join your factors with the 6-factor model data
all_reg_data_ff5_mom <- final_merged_renamed %>%
  select(date, all_of(renamed_factor_cols)) %>%
  # Use the date-corrected FF5+Mom data
  inner_join(factors_ff5_mom_eom, by = "date")

cat("\n\n--- Running Full-Period FF5 + Momentum Regressions ---\n")

# --- 4. Loop, Run Regressions, and Print Results ---

# Loop through each factor name in your list
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
  # --- Core Team (8) ---
  "Size_SMB",                      # Size
  "Book_to_Market_HML",            # Value (Classic)
  "Earnings_to_Price",             # Value 
  "Operating_Profitability_RMW",   # Profitability (Classic)
  "Asset_Growth_CMA",              # Investment (Classic)
  "Quality_Minus_Junk_QMJ",        # Quality 
  "Residual_Variance_RVAR",        # Low Volatility 
  "Amihud_Illiquidity",            # Liquidity 
  
  # --- Expansion (7) ---
  "Cash_Flow_to_Price",          # Value (Strong alternative)
  "Gross_Profitability",           # Profitability (Strong alternative)
  "Sales_Growth",                  # Growth (Distinct from Investment)
  "Net_Operating_Assets",          # Earnings Quality (Accruals proxy)
  "One_Year_Share_Issuance",       # Financing (Classic signal)
  "Altman_Z_Score",                # Distress (Classic signal)
  "High_Volume_Premium"            # Liquidity/Sentiment (Turnover)
)

cat("--- Running 'Expanded Team' Strategy with", length(selected_factors), "factors ---\n")
print(selected_factors)

# compute factor momentum strategy just using these factors
selected_factor_momentum <- calculate_momentum(final_merged_renamed, selected_factors, "Selected Factor Momentum")
# Apply volatility scaling
selected_factor_momentum_scaled <- scale_volatility(selected_factor_momentum,
                                                    target_ann_vol = target_vol,
                                                    lookback_months = lookback)
# --- 2. Combine with Industry Momentum and Plot ---
if (!is.null(industry_momentum_scaled) && !is.null(selected_factor_momentum_scaled)) {
  # Find the common start date AFTER scaling (due to lookback)
  common_start_date_selected <- max(min(industry_momentum_scaled$date), min(selected_factor_momentum_scaled$date))
  
  combined_momentum_selected <- bind_rows(industry_momentum_scaled, selected_factor_momentum_scaled) %>%
    filter(date >= common_start_date_selected) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate cumulative return based on SCALED returns
    mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
    ungroup()
  
  # Plotting Scaled Returns
  plot_selected <- ggplot(combined_momentum_selected, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(values = c("Industry Momentum" = "black", "Selected Factor Momentum" = "red")) +
    labs(
      title = paste0("Cumulative Performance of Selected Factor Momentum vs. Industry Momentum (Scaled to ", scales::percent(target_vol, accuracy = 1), " Ann. Volatility)"),
      subtitle = "Value of $1 invested (Log Scale)",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_selected)
  
} else {
  print("Could not calculate both selected factor and industry momentum series; skipping selected factor plot.")
}

# now plot this factor momentum strategy vs the old factor momentum that included all factors
if (!is.null(factor_momentum_scaled) && !is.null(selected_factor_momentum_scaled)) {
  # Find the common start date AFTER scaling (due to lookback)
  common_start_date_comparison <- max(min(factor_momentum_scaled$date), min(selected_factor_momentum_scaled$date))
  
  combined_momentum_comparison <- bind_rows(factor_momentum_scaled %>% mutate(strategy_type = "All Factor Momentum"),
                                            selected_factor_momentum_scaled %>% mutate(strategy_type = "Selected Factor Momentum")) %>%
    filter(date >= common_start_date_comparison) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate cumulative return based on SCALED returns
    mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
    ungroup()
  
  # Plotting Scaled Returns
  plot_comparison <- ggplot(combined_momentum_comparison, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(values = c("All Factor Momentum" = "blue", "Selected Factor Momentum" = "red")) +
    labs(
      title = paste0("Cumulative Performance: All Factor vs. Selected Factor Momentum (Scaled to ", scales::percent(target_vol, accuracy = 1), " Ann. Volatility)"),
      subtitle = "Value of $1 invested (Log Scale)",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_comparison)
  
} else {
  print("Could not calculate both all factor and selected factor momentum series; skipping comparison plot.")
}

# Let's look at excluded factors
excluded_factors <- setdiff(renamed_factor_cols, selected_factors)
print(excluded_factors)

# --- 2. Calculate and Scale "Excluded" Factor Momentum ---
# Assumes 'calculate_momentum' and 'scale_volatility' functions exist
# Assumes 'final_merged_renamed', 'target_vol', and 'lookback' exist
if (length(excluded_factors) >= 2) {
  excluded_factor_momentum <- calculate_momentum(final_merged_renamed, 
                                                 excluded_factors, 
                                                 "Excluded Factor Momentum") 
  
  excluded_factor_momentum_scaled <- scale_volatility(excluded_factor_momentum,
                                                      target_ann_vol = target_vol,
                                                      lookback_months = lookback)
} else {
  cat("\nWarning: Not enough 'excluded' factors to calculate momentum strategy.\n")
  excluded_factor_momentum_scaled <- NULL
}


# --- 3. Combine All Three Scaled Strategies ---
# Assumes 'factor_momentum_scaled' and 'selected_factor_momentum_scaled' exist
if (!is.null(factor_momentum_scaled) && 
    !is.null(selected_factor_momentum_scaled) && 
    !is.null(excluded_factor_momentum_scaled)) {
  
  # Find the common start date AFTER scaling (due to lookback)
  common_start_date_all_three <- max(
    min(factor_momentum_scaled$date), 
    min(selected_factor_momentum_scaled$date),
    min(excluded_factor_momentum_scaled$date)
  )
  
  # Combine, relabeling strategy_type for clarity
  combined_momentum_all_three <- bind_rows(
    factor_momentum_scaled %>% mutate(strategy_type = "All Factors"),
    selected_factor_momentum_scaled %>% mutate(strategy_type = "Selected Factors"),
    excluded_factor_momentum_scaled %>% mutate(strategy_type = "Excluded Factors")
  ) %>%
    filter(date >= common_start_date_all_three) %>%
    arrange(strategy_type, date) %>%
    group_by(strategy_type) %>%
    # Calculate cumulative return based on SCALED returns
    mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
    ungroup()
  
  # --- 4. Plot All Three ---
  
  # Define colors for the three strategies
  strategy_colors <- c(
    "All Factors"      = "blue", 
    "Selected Factors" = "red",
    "Excluded Factors" = "darkgreen" # Or any color you prefer
  )
  
  plot_all_three <- ggplot(combined_momentum_all_three, 
                           aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 10),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_color_manual(values = strategy_colors) +
    labs(
      title = paste0("Cumulative Performance: All vs. Selected vs. Excluded Factors (Scaled to ", scales::percent(target_vol, accuracy = 1), " Ann. Volatility)"),
      subtitle = "Value of $1 invested (Log Scale)",
      x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(plot_all_three)
  
} else {
  print("Could not calculate all three momentum series; skipping combined plot.")
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

