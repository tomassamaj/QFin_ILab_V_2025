# ==============================================================================
# 0. SETUP & LIBRARIES
# ==============================================================================

# Install missing packages if necessary
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, lubridate, stringr, frenchdata, ggplot2, 
               scales, corrplot, RColorBrewer, broom, zoo, purrr)

# ==============================================================================
# 1. DATA LOADING & PREPARATION
# ==============================================================================

# !!! USER ACTION REQUIRED: SET YOUR LOCAL FILE PATHS HERE !!!
# Ensure these CSV files are in your working directory
path_to_factors_csv  <- "[usa]_[all_factors]_[monthly]_[vw_cap].csv" 
path_to_themes_csv   <- "[usa]_[all_themes]_[monthly]_[vw_cap].csv"
path_to_industry_csv <- "17_Industry_Portfolios.csv"

# Validation Check
if (!file.exists(path_to_factors_csv)) stop("Factor CSV file not found!")
if (!file.exists(path_to_industry_csv)) stop("Industry CSV file not found!")

# Define Timeframe (Arnott et al. 2023 timeframe is approx 1963-2020+)
start_date <- ymd("1963-07-01")
end_date   <- ymd("2024-12-31")

# --- 1a. Load Proprietary Factors (JKP) ---
all_factors <- read.csv(path_to_factors_csv) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= start_date & date <= end_date) %>%
  select(date, name, ret) %>%
  pivot_wider(names_from = name, values_from = ret)

all_themes <- read.csv(path_to_themes_csv) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= start_date & date <= end_date) %>%
  select(date, name, ret) %>%
  pivot_wider(names_from = name, values_from = ret)

merged_factors <- inner_join(all_themes, all_factors, by = "date")

# --- 1b. Load Fama-French Industry Data ---
ind_ports <- read.csv(path_to_industry_csv)
industry_names <- ind_ports[7:23, 1] # Extracting industry names from typical file structure

ff_17_raw <- download_french_data("17 Industry Portfolios")
ff_17_monthly <- ff_17_raw$subsets$data[[1]] %>%
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    # FF data is usually in percent (e.g., 5.00), convert to decimal (0.05)
    across(all_of(industry_names), ~ as.numeric(.) / 100) 
  ) %>%
  rename_with(str_to_lower) %>%
  filter(date >= start_date & date <= end_date) %>%
  # Align dates to End-of-Month to match JKP data
  mutate(date = ceiling_date(date, "month") - days(1)) %>%
  select(date, all_of(str_to_lower(industry_names)))

# --- 1c. Merge All Data ---
final_merged <- inner_join(ff_17_monthly, merged_factors, by = "date")

# --- 1d. Rename Factors for Readability ---
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

final_merged_renamed <- final_merged %>% rename(any_of(factor_rename_map))

# Define Column Sets
industry_cols <- str_to_lower(industry_names)
renamed_factor_cols <- intersect(names(factor_rename_map), colnames(final_merged_renamed))

# ==============================================================================
# 2. CORE MOMENTUM FUNCTION (FIXED)
# ==============================================================================

#' Calculates simple 1-month cross-sectional momentum (Median Split)
#' Fixes: Ensures weights are signed correctly (+ for long, - for short)
#'        Uses vectorized operations instead of mapply for stability.
calculate_momentum <- function(df, target_cols, strategy_name) {
  
  cols_exist <- intersect(target_cols, colnames(df))
  if (length(cols_exist) < 2) return(NULL)
  
  df %>%
    select(date, all_of(cols_exist)) %>%
    arrange(date) %>%
    # Calculate ranking signal (Lagged 1 month)
    mutate(across(all_of(cols_exist), lag, .names = "{.col}_lag")) %>%
    filter(!if_all(ends_with("_lag"), is.na)) %>% # Remove initial NA rows
    rowwise() %>%
    mutate(
      median_ret = median(c_across(ends_with("_lag")), na.rm = TRUE),
      
      # Determine counts for weighting (1/N)
      # Note: For even N, median split is exact. For odd, median goes to short usually.
      n_long = sum(c_across(ends_with("_lag")) > median_ret, na.rm=TRUE),
      n_short = sum(c_across(ends_with("_lag")) <= median_ret, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    # Calculate weights for each asset: 
    #   Weight = +1/N_long if Lag > Median
    #   Weight = -1/N_short if Lag <= Median
    mutate(across(all_of(cols_exist), 
                  ~ {
                    lag_col <- get(paste0(cur_column(), "_lag"))
                    case_when(
                      is.na(lag_col) ~ 0,
                      lag_col > median_ret & n_long > 0 ~ 1 / n_long,
                      lag_col <= median_ret & n_short > 0 ~ -1 / n_short,
                      TRUE ~ 0
                    )
                  },
                  .names = "{.col}_wgt"
    )) %>%
    # Calculate Portfolio Return: Sum(Weight * Return)
    rowwise() %>%
    mutate(
      momentum_return = sum(c_across(ends_with("_wgt")) * c_across(all_of(cols_exist)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    mutate(strategy_type = strategy_name)
}
# --- Calculate Base Strategies ---
industry_mom <- calculate_momentum(final_merged_renamed, industry_cols, "Industry Momentum")
factor_mom   <- calculate_momentum(final_merged_renamed, renamed_factor_cols, "Factor Momentum")


# ==============================================================================
# 3. GENERALIZED STRATEGY FUNCTION (FIXED)
# ==============================================================================

#' Flexible Momentum Strategy
#' Fixes: Converts Log returns back to Arithmetic Mean for portfolio aggregation
calculate_momentum_strategy <- function(df, target_cols, 
                                        lookback_months = 1, 
                                        holding_months = 1, 
                                        signal_lag = 1, 
                                        long_quantile = 0.5, 
                                        short_quantile = 0.5) {
  
  target_cols <- intersect(target_cols, colnames(df))
  if (length(target_cols) < 2) return(NULL)
  
  # 1. Signal Generation (Cumulative Log Returns over Lookback)
  signals <- df %>%
    select(date, all_of(target_cols)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols), 
                  ~ rollapply(log(1 + .), width = lookback_months, FUN = sum, fill = NA, align = "right"),
                  .names = "{.col}_sig")) %>%
    mutate(across(ends_with("_sig"), ~ lag(., n = signal_lag))) # Lag the signal
  
  # 2. Forward Returns (Cumulative Log Returns over Holding Period)
  returns <- df %>%
    select(date, all_of(target_cols)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols), 
                  ~ rollapply(log(1 + .), width = holding_months, FUN = sum, fill = NA, align = "left"),
                  .names = "{.col}_ret")) %>%
    select(date, ends_with("_ret"))
  
  # 3. Merge & Filter for Rebalancing
  full_dat <- inner_join(signals, returns, by = "date") %>% na.omit()
  
  # 4. Portfolio Construction (Row-wise)
  full_dat %>%
    rowwise() %>%
    mutate(
      sigs = list(c_across(ends_with("_sig"))),
      rets = list(c_across(ends_with("_ret"))),
      res = list({
        # Thresholds
        q_l = quantile(sigs, long_quantile, na.rm=TRUE)
        q_s = quantile(sigs, short_quantile, na.rm=TRUE)
        
        # Indices
        if (long_quantile == short_quantile && long_quantile == 0.5) {
          idx_l <- which(sigs > q_l)
          idx_s <- which(sigs <= q_s)
        } else {
          idx_l <- which(sigs >= q_l)
          idx_s <- which(sigs <= q_s)
        }
        
        # *** KEY FIX: Geometric to Arithmetic Conversion ***
        # We summed log returns earlier. To average them cross-sectionally, 
        # convert back to simple returns: exp(r) - 1
        simple_rets <- exp(rets) - 1
        
        # Arithmetic Mean of Longs and Shorts
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
}

# ==============================================================================
# 4. BASIC PLOTTING (CUMULATIVE RETURNS)
# ==============================================================================

if (!is.null(industry_mom) && !is.null(factor_mom)) {
  
  combo <- bind_rows(industry_mom, factor_mom) %>%
    group_by(strategy_type) %>%
    arrange(date) %>%
    # Start cumulative return at 1 (Wealth Index) for log scale safety
    mutate(wealth_index = cumprod(1 + momentum_return)) %>%
    ungroup()
  
  p1 <- ggplot(combo, aes(x = date, y = wealth_index, color = strategy_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(labels = comma) +
    scale_color_manual(values = c("black", "blue")) +
    labs(title = "Replication: Factor vs. Industry Momentum",
         subtitle = "Unscaled, 1-Month Lookback/Holding",
         y = "Growth of $1 (Log Scale)") +
    theme_minimal() + theme(legend.position="top")
  
  print(p1)
}

# ==============================================================================
# 5. VOLATILITY SCALING FUNCTION
# ==============================================================================

scale_volatility <- function(df, col_name = "momentum_return", target_vol = 0.10, lookback = 36) {
  df %>%
    arrange(date) %>%
    mutate(
      # Calculate rolling annualized volatility
      rolling_sd = rollapply(.data[[col_name]], width = lookback, FUN = sd, fill = NA, align = "right", partial = 12),
      ann_vol = rolling_sd * sqrt(12),
      
      # Calculate leverage (Lagged to avoid look-ahead bias)
      leverage = lag(target_vol / ann_vol),
      # Cap leverage at 5x to prevent explosions in low-vol periods
      leverage = pmin(leverage, 5.0), 
      leverage = replace_na(leverage, 1), # Default to 1x at start
      
      scaled_return = .data[[col_name]] * leverage
    ) %>%
    filter(!is.na(ann_vol)) # Trim initialization period
}

# Apply Scaling
if (!is.null(factor_mom)) {
  factor_mom_scaled <- scale_volatility(factor_mom) %>%
    mutate(strategy_type = "Factor Mom (10% Vol)")
  
  industry_mom_scaled <- scale_volatility(industry_mom) %>%
    mutate(strategy_type = "Industry Mom (10% Vol)")
  
  # Plot Scaled
  scaled_combo <- bind_rows(factor_mom_scaled, industry_mom_scaled) %>%
    group_by(strategy_type) %>%
    mutate(wealth = cumprod(1 + scaled_return))
  
  p2 <- ggplot(scaled_combo, aes(x=date, y=wealth, color=strategy_type)) +
    geom_line(linewidth=1) +
    scale_y_log10(labels=comma) +
    labs(title = "Volatility Scaled Momentum (Target 10%)",
         y = "Growth of $1 (Log Scale)") +
    theme_minimal() + theme(legend.position="top")
  
  print(p2)
}

# ==============================================================================
# 6. LONG VS SHORT LEG ANALYSIS (Using Generalized Function)
# ==============================================================================

# Run generalized strategy (1M lookback, 1M hold, 1M lag)
gen_mom <- calculate_momentum_strategy(final_merged_renamed, renamed_factor_cols)

# Prepare Data for Plotting
ls_data <- gen_mom %>%
  select(date, Long=long_return, Short=short_return, Net=long_short_return) %>%
  pivot_longer(-date, names_to="Leg", values_to="Ret") %>%
  # Invert short for visual comparison (how much did shorts contribute?)
  mutate(Ret = ifelse(Leg == "Short", -Ret, Ret)) %>% 
  group_by(Leg) %>%
  mutate(Wealth = cumprod(1 + Ret))

p3 <- ggplot(ls_data, aes(x=date, y=Wealth, color=Leg)) +
  # Bubble Burst Highlight
  annotate("rect", xmin=ymd("2000-03-01"), xmax=ymd("2002-10-01"), 
           ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
  geom_line() +
  scale_y_log10(labels=comma) +
  labs(title = "Factor Momentum: Long vs Short Legs",
       subtitle = "'Short' line is inverted return (Short PnL). Red Area = DotCom Crash.") +
  theme_minimal()

print(p3)

# ==============================================================================
# 7. REGRESSIONS (FF3 & FF5)
# ==============================================================================

# 7a. Download Benchmark Data
# We download the Fama-French 5 Factor data (includes RF and Mkt-RF)
ff5_raw <- download_french_data("Fama/French 5 Factors (2x3)")
ff5_data <- ff5_raw$subsets$data[[1]] %>%
  mutate(
    date = ceiling_date(floor_date(ymd(str_c(date, "01")), "month"), "month") - days(1),
    across(c(`Mkt-RF`, SMB, HML, RMW, CMA, RF), ~ as.numeric(.) / 100)
  ) %>%
  rename(mkt_excess = `Mkt-RF`, rf = RF, smb = SMB, hml = HML, rmw = RMW, cma = CMA) %>%
  filter(date >= start_date & date <= end_date)

# Download Momentum Factor separately
umd_raw <- download_french_data("Momentum Factor (Mom)")
umd_data <- umd_raw$subsets$data[[1]] %>%
  mutate(
    date = ceiling_date(floor_date(ymd(str_c(date, "01")), "month"), "month") - days(1),
    mom = as.numeric(Mom) / 100
  ) %>%
  select(date, mom) %>%
  filter(date >= start_date & date <= end_date)

# Join benchmarks
benchmarks <- inner_join(ff5_data, umd_data, by="date")

# 7b. Run Regressions on Individual Factors
reg_results <- list()

for (fac in renamed_factor_cols) {
  # Prep data
  tmp <- final_merged_renamed %>%
    select(date, all_of(fac)) %>%
    inner_join(benchmarks, by="date") %>%
    mutate(ex_ret = .data[[fac]] - rf) %>%
    na.omit()
  
  if(nrow(tmp) > 24) { # Ensure enough data points
    # Model 1: FF3
    m1 <- lm(ex_ret ~ mkt_excess + smb + hml, data=tmp)
    # Model 2: FF5 + Mom
    m2 <- lm(ex_ret ~ mkt_excess + smb + hml + rmw + cma + mom, data=tmp)
    
    # Store Alphas
    reg_results[[fac]] <- tibble(
      Factor = fac,
      Alpha_FF3 = coef(m1)["(Intercept)"],
      t_FF3 = tidy(m1) %>% filter(term == "(Intercept)") %>% pull(statistic),
      Alpha_FF6 = coef(m2)["(Intercept)"],
      t_FF6 = tidy(m2) %>% filter(term == "(Intercept)") %>% pull(statistic)
    )
  }
}

reg_summary <- bind_rows(reg_results)
print(head(reg_summary))

# ==============================================================================
# 8. CLUSTER & CORRELATION ANALYSIS
# ==============================================================================

# Correlation Matrix
fac_mat <- final_merged_renamed %>% select(all_of(renamed_factor_cols)) %>% na.omit()
cor_mat <- cor(fac_mat)

corrplot(cor_mat, method="color", order="hclust", tl.cex=0.6, title="Factor Correlations")

# Clustering
dist_mat <- as.dist(1 - abs(cor_mat))
hc <- hclust(dist_mat, method="ward.D2")
clusters <- cutree(hc, k=5)

# Cluster Performance
# Loop through clusters, calc momentum for that subset of factors
cluster_res <- map_dfr(unique(clusters), function(k) {
  sub_factors <- names(clusters)[clusters == k]
  if(length(sub_factors) < 2) return(NULL)
  
  calculate_momentum(final_merged_renamed, sub_factors, paste("Cluster", k))
})

if(!is.null(cluster_res)) {
  p_clust <- cluster_res %>%
    group_by(strategy_type) %>%
    arrange(date) %>%
    mutate(wealth = cumprod(1 + momentum_return)) %>%
    ggplot(aes(x=date, y=wealth, color=strategy_type)) +
    geom_line() +
    scale_y_log10() +
    labs(title="Momentum by Factor Cluster") + theme_minimal()
  print(p_clust)
}

# ==============================================================================
# 9. DRAWDOWN ANALYSIS (FIXED SCALE)
# ==============================================================================

# Calculate Drawdowns for the base Factor Momentum Strategy
dd_data <- gen_mom %>%
  arrange(date) %>%
  mutate(
    cum_ret = cumprod(1 + long_short_return),
    peak = cummax(cum_ret),
    drawdown = (cum_ret - peak) / peak
  )

p_dd <- ggplot(dd_data, aes(x=date, y=drawdown)) +
  geom_area(fill="darkred", alpha=0.5) +
  geom_line(color="darkred") +
  # FIX: Use continuous scale for negative numbers, NOT log
  scale_y_continuous(labels = percent) + 
  labs(title = "Factor Momentum Drawdowns", y = "Drawdown %") +
  theme_minimal()

print(p_dd)