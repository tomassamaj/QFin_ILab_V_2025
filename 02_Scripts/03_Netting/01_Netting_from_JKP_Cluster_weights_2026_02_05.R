# ==============================================================================
# STRATEGY: REALISTIC FACTOR MOMENTUM (Monthly Rebal, Post-1960)
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, arrow, lubridate, zoo, data.table, ggplot2, scales)

# --- 1. CONFIGURATION ---
DATA_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs"
START_DATE <- as.Date("1960-01-01") 

# Factors definitions 
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
# 2. PREPARE WEIGHTS
# ------------------------------------------------------------------------------
cat("--- 1. Processing Monthly Weights (Post-1960) ---\n")
weights <- read_parquet(file.path(DATA_DIR, "usa_factor_weights.parquet")) %>%
  filter(characteristic %in% target_factors) %>%
  filter(eom >= START_DATE) %>% # <--- FILTER
  select(id, eom, characteristic, weight, leg) %>%
  collect()

setDT(weights)
weights[, direction := ifelse(characteristic %in% factors_to_flip, -1L, 1L)]
weights[, net_weight := weight * leg * direction]
weights[, c("weight", "leg", "direction") := NULL]
weights[, rebal_date := eom]

# ------------------------------------------------------------------------------
# 3. CALCULATE MONTHLY SIGNALS
# ------------------------------------------------------------------------------
cat("--- 2. Calculating Signals (Post-1960) ---\n")
pfs_daily <- read_parquet(file.path(DATA_DIR, "pfs_daily.parquet")) %>%
  filter(excntry == "USA", characteristic %in% target_factors) %>%
  filter(date >= START_DATE - days(60)) %>% # Load a bit earlier for 21d rolling calc
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  collect()

setDT(pfs_daily)

# Factor Returns
factor_rets <- pfs_daily[,
  .(
    factor_ret = ret[pf == max(pf)] - ret[pf == min(pf)]
  ),
  by = .(date, characteristic)
]

# Signal Generation
factor_rets <- factor_rets[order(characteristic, date)]
factor_rets[,
  raw_signal := frollsum(log(1 + factor_ret), n = 21),
  by = characteristic
]
factor_rets[, mom_signal_lag := shift(raw_signal, 1), by = characteristic]

# Sample at Month-End
factor_rets[, month := floor_date(date, "month")]
monthly_signals <- factor_rets[
  date >= START_DATE,
  .SD[date == max(date)],
  by = .(characteristic, month)
]

# Rank
monthly_signals[, rank := frank(mom_signal_lag), by = month]
monthly_signals[, n_facts := .N, by = month]

# Allocations
monthly_signals[,
  pos_quartile_lo := fifelse(rank > 0.75 * n_facts, 1 / (0.25 * n_facts), 0)
]
monthly_signals[,
  pos_median_ls := fifelse(
    rank > 0.5 * n_facts,
    1 / (0.5 * n_facts),
    -1 / (0.5 * n_facts)
  )
]

active_signals <- monthly_signals[, .(
  month,
  rebal_date = date,
  characteristic,
  pos_quartile_lo,
  pos_median_ls
)]

# ------------------------------------------------------------------------------
# 4. MERGE & NET
# ------------------------------------------------------------------------------
cat("--- 3. Merging Portfolio ---\n")
weights[, month := floor_date(eom, "month")]

portfolio_constituents <- merge(
  active_signals,
  weights,
  by = c("month", "characteristic"),
  allow.cartesian = TRUE
)

monthly_portfolio <- portfolio_constituents[,
  .(
    w_quartile_lo = sum(net_weight * pos_quartile_lo),
    w_median_ls = sum(net_weight * pos_median_ls)
  ),
  by = .(rebal_date.x, id)
]
setnames(monthly_portfolio, "rebal_date.x", "rebal_date")
# ------------------------------------------------------------------------------
# 5. BACKTEST
# ------------------------------------------------------------------------------
cat("--- 4. Backtesting ---\n")
stock_rets <- read_parquet(file.path(DATA_DIR, "USA_daily_rets.parquet")) %>%
  filter(date >= START_DATE) %>% 
  select(id, date, ret = ret_exc) %>%
  collect()
setDT(stock_rets)
stock_rets[, month := floor_date(date, "month")]

monthly_portfolio[, trade_month := floor_date(rebal_date, "month") + months(1)]

daily_holdings <- merge(
  stock_rets,
  monthly_portfolio,
  by.x = c("month", "id"),
  by.y = c("trade_month", "id"),
  all.x = FALSE,
  all.y = FALSE
)

daily_perf <- daily_holdings[,
  .(
    ret_quartile_lo = sum(w_quartile_lo * ret, na.rm = TRUE),
    ret_median_ls = sum(w_median_ls * ret, na.rm = TRUE)
  ),
  by = date
]

daily_perf <- daily_perf[order(date)]
daily_perf[, cum_quartile_lo := cumprod(1 + ret_quartile_lo)]
daily_perf[, cum_median_ls := cumprod(1 + ret_median_ls)]

# ------------------------------------------------------------------------------
# 6. RESULTS
# ------------------------------------------------------------------------------
cat("--- 5. Results (Since 1960) ---\n")

plot_data <- melt(
  daily_perf,
  id.vars = "date",
  measure.vars = c("cum_quartile_lo", "cum_median_ls")
)

ggplot(plot_data, aes(x = date, y = value, color = variable)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Factor Momentum (Monthly Rebal, Since 1960)",
    subtitle = "Top 25% Long Only vs. Median L/S",
    y = "Cumulative Wealth (Log)",
    color = "Strategy"
  ) +
  theme_minimal()

stats <- daily_perf[, .(
  Strategy = c("Quartile LO", "Median LS"),
  Ann_Ret = c(mean(ret_quartile_lo) * 252, mean(ret_median_ls) * 252),
  Ann_Vol = c(sd(ret_quartile_lo) * sqrt(252), sd(ret_median_ls) * sqrt(252))
)]
stats[, Sharpe := Ann_Ret / Ann_Vol]
print(stats)


# Output path
OUTPUT_FILE <- file.path(
  DATA_DIR,
  "final_monthly_strategy_weights_1960.parquet"
)

# Save 
write_parquet(monthly_portfolio, OUTPUT_FILE)

cat("Netted positions saved to:", OUTPUT_FILE, "\n")

# ==============================================================================
# EVENT STUDY: ZOOMING IN ON CRISES
# ==============================================================================

plot_event <- function(data, start_date, end_date, event_name) {
  # Filter data
  zoom_data <- data %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
    melt(id.vars = "date", measure.vars = c("cum_quartile_lo", "cum_median_ls"))


  zoom_data[, value := value / first(value), by = variable]

  # Plot
  p <- ggplot(zoom_data, aes(x = date, y = value, color = variable)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste("Event Study:", event_name),
      subtitle = paste(start_date, "to", end_date),
      y = "Cumulative Return (Rebased to 1.0)",
      color = "Strategy"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
}

# --- 1. The Dot-Com Crash (2000 - 2002) ---
plot_event(daily_perf, "1999-01-01", "2002-12-31", "Dot-Com Bubble & Crash")

# --- 2. The COVID-19 Crash (2020) ---
plot_event(daily_perf, "2019-06-01", "2021-06-01", "COVID-19 Crash & Rebound")

show_top_factors <- function(target_date) {
  target_month <- floor_date(as.Date(target_date), "month")

  holdings <- active_signals %>%
    filter(month == target_month) %>%
    filter(pos_quartile_lo > 0) %>% # Look at Long Only portfolio
    select(characteristic)

  print(paste("--- Active Factors in:", target_month, "---"))
  print(holdings$characteristic)
}

# Dot-Com Peak (March 2000)
show_top_factors("2000-03-01")

# Dot-Com Bottom (Sept 2002)
show_top_factors("2002-09-01")

# COVID Crash (March 2020)
show_top_factors("2020-03-01")

# COVID Recovery (Nov 2020: Vaccine Announcement)
show_top_factors("2020-11-01")

# Check for jump in 2020
daily_perf %>%
  filter(year(date) == 2018) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(
    Month_Return = prod(1 + ret_quartile_lo) - 1
  ) %>%
  mutate(Month_Return_Pct = scales::percent(Month_Return, accuracy = 0.1)) %>%
  print()

daily_perf %>%
  filter(year(date) == 2019) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(
    Month_Return = prod(1 + ret_quartile_lo) - 1
  ) %>%
  mutate(Month_Return_Pct = scales::percent(Month_Return, accuracy = 0.1)) %>%
  print()

daily_perf %>%
  filter(year(date) == 2020) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(
    Month_Return = prod(1 + ret_quartile_lo) - 1
  ) %>%
  mutate(Month_Return_Pct = scales::percent(Month_Return, accuracy = 0.1)) %>%
  print()


# 1. FACTORS: What signals were active?

print("--- Active Factors for Dec 2018 Trading ---")
active_signals %>%
  filter(month == as.Date("2018-11-01")) %>% # Signal generated end of Nov for Dec
  filter(pos_quartile_lo > 0) %>%
  select(characteristic) %>%
  print()

# 2. STOCKS: What companies?

print("--- Top 10 Stock Holdings (Dec 2018) ---")

top_holdings_dec18 <- monthly_portfolio %>%
  filter(rebal_date == as.Date("2018-11-30")) %>%
  arrange(desc(w_quartile_lo)) %>%
  head(15) %>%
  select(id, w_quartile_lo)

print(top_holdings_dec18)

# 3. STOCK PERFORMANCE: How did these stocks perform in Dec 2018?
print("--- Performance of Top Holdings in Dec 2018 ---")

audit_dec18 <- top_holdings_dec18 %>%
  left_join(stock_rets, by = c("id")) %>%
  filter(date >= as.Date("2018-12-01") & date <= as.Date("2018-12-31")) %>%
  group_by(id) %>%
  summarise(
    Weight = first(w_quartile_lo),
    Total_Return_Dec = prod(1 + ret) - 1
  ) %>%
  arrange(desc(Total_Return_Dec))

print(audit_dec18)


# Find the stock with the highest return in Dec 2018 regardless of its weight in the portfolio
print("--- Searching for Data Errors (Dec 2018) ---")

culprit <- monthly_portfolio %>%
  # Filter for the Dec 2018 trading month
  filter(rebal_date == as.Date("2018-11-30")) %>%
  left_join(stock_rets, by = "id") %>%
  filter(date >= as.Date("2018-12-01") & date <= as.Date("2018-12-31")) %>%
  group_by(id) %>%
  summarise(
    Weight = first(w_quartile_lo),
    Total_Return_Dec = prod(1 + ret) - 1,
    Max_Daily_Return = max(ret)
  ) %>%
  arrange(desc(Total_Return_Dec)) %>%
  head(5)

print(culprit)
