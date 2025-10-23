


# load csv file with factor returns

all_factors_monthly_vw_cap  <- read.csv("[usa]_[all_factors]_[monthly]_[vw_cap].csv")

all_themes_monthly_vw_cap  <- read.csv("[usa]_[all_themes]_[monthly]_[vw_cap].csv")

# make them from long format to wide with dplyer through the name col

library(dplyr)
library(tidyr)

# drop all other cols except name, date and ret
all_factors_monthly_vw_cap <- all_factors_monthly_vw_cap  %>%
  select(date, name, ret)

all_factors_wide <- all_factors_monthly_vw_cap %>%
  pivot_wider(names_from = name, values_from = ret)


all_themes_monthly_vw_cap  <- all_themes_monthly_vw_cap  %>%
  select(date, name, ret)





all_themes_wide <- all_themes_monthly_vw_cap  %>%
  pivot_wider(names_from = name, values_from = ret)

# drop all date before 1960
all_factors_wide <- all_factors_wide %>%
  filter(date >= 196301)
all_themes_wide <- all_themes_wide %>%
  filter(date >= 196301)



str(all_factors_wide)
str(all_themes_wide)

# set date cols to date

all_factors_wide$date <- as.Date(as.character(all_factors_wide$date))
all_themes_wide$date <- as.Date(as.character(all_themes_wide$date))
                                

# merge the datasets by date
merged_factors <- merge(all_themes_wide, all_factors_wide, by = "date")

library(frenchdata)
library(lubridate)
library(stringr)

start_date <- ymd("1964-01-01")
end_date <- ymd("2024-12-31")
#end_date <- ymd("2025-08-31")

factors_ff3_monthly_raw <- download_french_data("Fama/French 3 Factors")
factors_ff3_monthly <- factors_ff3_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(date >= start_date & date <= end_date)

ind_ports[7:23,1]


ff_17_industry_monthly_raw <- download_french_data("17 Industry Portfolios")
ff_17_industry_monthly_raw <- ff_17_industry_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(ind_ports[7:23,1], ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  filter(date >= start_date & date <= end_date)


# set the date col to EOM date in the 17 portfolios
ff_17_industry_monthly <- ff_17_industry_monthly_raw %>%
  mutate(date = as.Date(date))
str(ff_17_industry_monthly)
str(merged_factors)



# from merged factors get colnames
colnames(merged_factors)



# copy the date col from merged_factors to ff_17_industry_monthly simply and name it do date_temp
ff_17_industry_monthly <- ff_17_industry_monthly %>%
  mutate(date_temp = merged_factors$date)
# drop the date col
ff_17_industry_monthly <- ff_17_industry_monthly %>%
  select(-date)
# rename date_temp to date
ff_17_industry_monthly <- ff_17_industry_monthly %>%
  rename(date = date_temp)
# put it to the first col
ff_17_industry_monthly <- ff_17_industry_monthly %>%
  select(date, everything())





# merge the ff17 and the merged factors
final_merged <- merge(ff_17_industry_monthly, merged_factors, by = "date")
















factor_rename_map <- c(
  
  # Common factors
  "Size_SMB" = "market_equity",
  "Book_to_Market_HML" = "be_me",
  "Operating_Profitability_RMW" = "ope_be",
  "Asset_Growth_CMA" = "at_gr1",
  "Long_Term_Reversals_LTREV" = "ret_60_12",
  "Residual_Variance_RVAR" = "ivol_ff_21d", # Note: 'ivol_ff3_21d' is not in your list, using 'ivol_ff_21d' if it exists, or 'ivol_ff3_21d' if that was the typo
  "Quality_Minus_Junk_QMJ" = "qmj",
  "Low_Beta_BAB" = "betabab_1260d",
  
  # Non-fundamental
  "Amihud_Illiquidity" = "ami_126d",
  "Firm_Age" = "age",
  "Nominal_Price" = "prc",
  "High_Volume_Premium" = "dolvol_126d",
  
  # Profitability
  "Gross_Profitability" = "gp_at",
  "Return_on_Equity" = "ni_be",
  "Return_on_Assets" = "niq_at",
  "Profit_Margin" = "ebit_sale",
  "Change_in_Asset_Turnover" = "at_turnover",
  
  # Earnings quality
  "Accruals_Factor" = "oaccruals_at", # Suffix to avoid conflict with 'accruals' cluster
  "Net_Operating_Assets" = "noa_at",
  "Net_Working_Capital_Changes" = "cowc_gr1a", # Corrected from 'grla'
  "Cash_Flow_to_Price" = "ocf_me",
  "Earnings_to_Price" = "ni_me",
  "Enterprise_Multiple" = "ebitda_mev",
  "Sales_to_Price" = "sale_me",
  
  # Investment and growth
  "Growth_in_Inventory" = "inv_gr1",
  "Sales_Growth" = "sale_gr1",
  "Growth_in_Sales_Inventory" = "dsale_dinv",
  "Abnormal_Investment" = "capex_abn",
  "CAPX_Growth_Rate" = "capx_gr1",
  
  # Financing
  "Debt_Issuance_Factor" = "dbnetis_at", # Suffix to avoid conflict with 'debt_issuance' cluster
  "Leverage_Factor" = "at_be",         # Suffix to avoid conflict with 'low_leverage' cluster
  "One_Year_Share_Issuance" = "chcsho_12m",
  "Total_External_Financing" = "netis_at",
  
  # Distress
  "Ohlson_O_Score" = "o_score",
  "Altman_Z_Score" = "z_score",
  
  # Composite
  "Piotroski_F_Score" = "f_score"
)

# 2. Rename the columns in your FINAL merged dataframe
# This creates a new dataframe with the readable names
merged_factors_renamed <- merged_factors %>%
  rename(any_of(factor_rename_map)) # Use any_of() to ignore missing columns




# 3. Create a vector of the *exact* factor names from the image
# (using the readable names you just created)
factors_from_image <- c(
  # "date" will be added separately
  
  # Common factors
  "Size_SMB", "Book_to_Market_HML", "Operating_Profitability_RMW", 
  "Asset_Growth_CMA", "Long_Term_Reversals_LTREV", "Residual_Variance_RVAR",
  "Quality_Minus_Junk_QMJ", "Low_Beta_BAB",
  
  # Non-fundamental
  "Amihud_Illiquidity", "Firm_Age", "Nominal_Price", "High_Volume_Premium",
  
  # Profitability
  "Gross_Profitability", "Return_on_Equity", "Return_on_Assets", 
  "Profit_Margin", "Change_in_Asset_Turnover",
  
  # Earnings quality
  "Accruals_Factor", "Net_Operating_Assets", "Net_Working_Capital_Changes",
  "Cash_Flow_to_Price", "Earnings_to_Price", "Enterprise_Multiple", "Sales_to_Price",
  
  # Investment and growth
  "Growth_in_Inventory", "Sales_Growth", "Abnormal_Investment", "CAPX_Growth_Rate",
  "Growth_in_Sales_Inventory",
  
  # Financing
  "Debt_Issuance_Factor", "Leverage_Factor", "One_Year_Share_Issuance", 
  "Total_External_Financing",
  
  # Distress
  "Ohlson_O_Score", "Altman_Z_Score",
  
  # Composite
  "Piotroski_F_Score"
)


industry_cols <- setdiff(colnames(ff_17_industry_monthly), "date")




cols_to_keep <- c("date", industry_cols, factors_from_image)

final_narrowed_df <- merged_factors_renamed %>%
  select(any_of(cols_to_keep))


print("Columns in the final narrowed dataframe:")
colnames(final_narrowed_df)

#merge final_narrowed_df with ff_17_industry_monthly by date
final_merged <- merge(ff_17_industry_monthly, final_narrowed_df, by = "date")




# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales) # For percent_format
library(lubridate) # Helpful for date manipulation if needed, already loaded

# --- Assuming 'final_merged' and 'ff_17_industry_monthly' are loaded ---
# --- from your previous script steps ---

# --- 1. Define Column Sets ---
industry_cols <- setdiff(colnames(ff_17_industry_monthly), "date")
theme_cols <- colnames(all_themes_wide)[-1] # Assuming all_themes_wide is available
all_cols <- colnames(final_merged)
factor_cols <- setdiff(all_cols, c("date", theme_cols, industry_cols))

# --- 2. Function to Calculate Momentum Returns ---
calculate_momentum <- function(df, target_cols, strategy_name) {
  
  # Check if columns exist
  if (!all(target_cols %in% colnames(df))) {
    missing_cols <- target_cols[!target_cols %in% colnames(df)]
    stop(paste("Error in", strategy_name, "- Missing columns:", paste(missing_cols, collapse=", ")))
  }
  
  momentum_df <- df %>%
    select(date, all_of(target_cols)) %>%
    arrange(date) %>%
    # Calculate lagged returns
    mutate(across(all_of(target_cols), lag, .names = "{.col}_lag1")) %>%
    # Remove first row which now has NAs for lagged returns
    filter(row_number() > 1) %>%
    # --- Start calculations for this row's portfolio weights ---
    rowwise() %>%
    mutate(median_lag1_ret = median(c_across(ends_with("_lag1")), na.rm = TRUE)) %>%
    ungroup() %>%
    # Assign positions based on lagged returns
    mutate(across(ends_with("_lag1"),
                  ~ case_when(
                    !is.na(.) & . > median_lag1_ret ~ 1,
                    !is.na(.) & . <= median_lag1_ret ~ -1,
                    TRUE ~ 0 # Handle NAs in lagged returns
                  ),
                  .names = "{sub('_lag1', '_pos', .col)}"
    )) %>%
    # Calculate counts for normalization
    rowwise() %>%
    mutate(
      n_long = sum(c_across(ends_with("_pos")) == 1),
      n_short = sum(c_across(ends_with("_pos")) == -1)
    ) %>%
    ungroup() %>%
    # Normalize weights
    mutate(across(ends_with("_pos"),
                  ~ case_when(
                    . == 1 & n_long > 0 ~ 1 / n_long,
                    . == -1 & n_short > 0 ~ 1 / n_short, # Note: weight is positive, position is -1
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"
    )) %>%
    # --- Calculate portfolio return for the *current* date using the weights ---
    rowwise() %>%
    mutate(
      momentum_return = sum(
        # Position * Weight * Current Return
        c_across(ends_with("_pos")) *
          c_across(ends_with("_wgt")) *
          c_across(all_of(target_cols)), # Current month returns
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    # Add strategy name
    mutate(strategy_type = strategy_name)
  
  return(momentum_df)
}

# --- 3. Calculate Momentum for Both ---
industry_momentum <- calculate_momentum(final_merged, industry_cols, "Industry Momentum")
factor_momentum <- calculate_momentum(final_merged, factor_cols, "Factor Momentum")

# --- 4. Combine and Calculate Cumulative Returns ---
# Ensure both start from the same date (should be automatic if input df is the same)
common_start_date <- max(min(industry_momentum$date), min(factor_momentum$date))

combined_momentum <- bind_rows(industry_momentum, factor_momentum) %>%
  filter(date >= common_start_date) %>%
  arrange(strategy_type, date) %>%
  group_by(strategy_type) %>%
  # Calculate cumulative return starting from 1 (for log plot)
  mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
  ungroup()

# --- 5. Plot Combined Cumulative Returns on Log Scale ---
ggplot(combined_momentum, aes(x = date, y = cumulative_return_log, color = strategy_type)) +
  geom_line(linewidth = 1) + # Use linewidth instead of size for ggplot2 >= 3.4.0
  scale_y_log10(
    breaks = scales::log_breaks(n = 10), # Adjust number of breaks if needed
    labels = scales::label_number(accuracy = 0.1) # Format labels nicely
    # labels = scales::label_dollar() # Alternative if you prefer dollar values
  ) +
  scale_color_manual(values = c("Industry Momentum" = "black", "Factor Momentum" = "blue")) + # Colors similar to paper
  labs(
    title = "Cumulative Performance of Factor vs. Industry Momentum",
    subtitle = "Value of $1 invested (Log Scale)",
    x = "Year",
    y = "Cumulative Performance ($)",
    color = "Strategy" # Legend title
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# --- Optional: Display Head/Summary ---
print("Industry Momentum Head:")
print(head(industry_momentum))
print("Factor Momentum Head:")
print(head(factor_momentum))
print("Combined Data Head:")
print(head(combined_momentum))






# Load necessary libraries (if not already loaded)
library(dplyr)
library(reshape2) # For melt()
library(ggplot2)
library(RColorBrewer) # For color palettes

# --- Assuming 'final_narrowed_df' exists ---
# --- Assuming 'factors_from_image' vector exists ---

# 1. Identify and select factor columns (same as before)
factors_to_correlate <- intersect(factors_from_image, colnames(final_narrowed_df))

if (length(factors_to_correlate) < 2) {
  stop("Error: Need at least two factor columns.")
}

factor_data_for_corr <- final_narrowed_df %>%
  select(all_of(factors_to_correlate)) %>%
  na.omit()

# 2. Calculate the correlation matrix (same as before)
cor_matrix <- cor(factor_data_for_corr)

# 3. Create a copy of the matrix to modify
cor_matrix_lower <- cor_matrix
# Set upper triangle (excluding diagonal) to NA
cor_matrix_lower[upper.tri(cor_matrix_lower)] <- NA

# 4. Prepare data for ggplot (melt the modified matrix)
melted_cor_matrix_lower <- melt(cor_matrix_lower, na.rm = TRUE) # na.rm=TRUE removes NA values for plotting efficiency

# 5. Create the lower triangular heatmap plot
ggplot(melted_cor_matrix_lower, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8), # Adjust size if needed
    axis.text.y = element_text(size = 8), # Adjust size if needed
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  coord_fixed() +
  labs(title = "Lower Triangular Correlation Heatmap of Selected Factors")

# Optional: Print the lower triangular matrix values

# Load necessary libraries (if not already loaded)
library(dplyr)
library(corrplot) # For the corrplot function
library(RColorBrewer) # For color palettes

# --- Assuming 'final_narrowed_df' exists ---
# --- Assuming 'factors_from_image' vector exists ---

# 1. Identify and select factor columns (same as before)
factors_to_correlate <- intersect(factors_from_image, colnames(final_narrowed_df))

if (length(factors_to_correlate) < 2) {
  stop("Error: Need at least two factor columns.")
}

factor_data_for_corr <- final_narrowed_df %>%
  select(all_of(factors_to_correlate)) %>%
  na.omit()

# 2. Calculate the correlation matrix (same as before)
cor_matrix <- cor(factor_data_for_corr)

# 3. Create the corrplot visualization
# Define a color palette (optional, but often nice)
col_palette <- colorRampPalette(brewer.pal(n = 11, name = "BrBG"))(200) # Similar to ggplot gradient



# Generate the plot
corrplot(cor_matrix,
         method = "color",       # Use color intensity for correlation strength
         type = "upper",         # Display only the upper triangle
         order = "original",     # Keep original variable order (or use "hclust" for clustering)  # Add correlation coefficients in black text
         tl.col = "black",       # Color of text labels (variable names)
         tl.srt = 45,            # Rotate text labels 45 degrees
         tl.cex = 0.5,           # Adjust text label size (cex) if needed
         number.cex = 0.5,       # Adjust coefficient number size (cex) if needed
         col = col_palette,      # Use the custom color palette
         diag = FALSE,           # Do not display the diagonal
         cl.cex = 0.7,           # Adjust color legend label size
         mar = c(0,0,1,0)         # Adjust margins (bottom, left, top, right) if needed
)


# Generate the plot
corrplot(cor_matrix,
         method = "color",       # Use color intensity for correlation strength
         type = "upper",         # Display only the upper triangle
         order = "original",     # Keep original variable order (or use "hclust" for clustering)
         addCoef.col = "black",  # Add correlation coefficients in black text
         tl.col = "black",       # Color of text labels (variable names)
         tl.srt = 45,            # Rotate text labels 45 degrees
         tl.cex = 0.4,           # Adjust text label size (cex) if needed
         number.cex = 0.4,       # Adjust coefficient number size (cex) if needed
         col = col_palette,      # Use the custom color palette
         diag = FALSE,           # Do not display the diagonal
         cl.cex = 0.7,           # Adjust color legend label size
         mar = c(0,0,1,0)         # Adjust margins (bottom, left, top, right) if needed
)

mtext("Upper Triangular Correlation Heatmap of Selected Factors", side=3, line=0.5) # Add title manually