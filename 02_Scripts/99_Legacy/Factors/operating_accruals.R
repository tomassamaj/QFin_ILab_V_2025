# ==============================================================================
# BUILDER TASK: Operating Accruals (Sloan 1996) Replication
# ==============================================================================
# Factor: Earnings Quality / Accruals ('oaccruals_at')
# Formula: (Delta_CA - Delta_Cash) - (Delta_CL - Delta_STD - Delta_TP) - Dep
#          Scaled by Total Assets (t-1).
# Strategy: Long Low Accruals (Q1) - Short High Accruals (Q5).
# ==============================================================================

# 1. Setup & Data Loading ------------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(scales)
library(lubridate)
library(RPostgres)

# Connect to DB
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

message("Connecting to WRDS for Balance Sheet Components...")
# (Assuming WRDS connection credentials are set in environment)
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# Fetch Accrual Components
# act: Current Assets
# che: Cash & Short-Term Investments
# lct: Current Liabilities
# dlc: Debt in Current Liabilities
# txp: Income Taxes Payable
# dp: Depreciation and Amortization
# at: Total Assets
comp_acc_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  select(gvkey, datadate, at, act, che, lct, dlc, txp, dp) |>
  collect() |>
  mutate(datadate = ymd(datadate))

dbDisconnect(wrds)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect() |> mutate(date = ymd(date))

# 2. Construct Signal (Operating Accruals) -------------------------------------
message("Constructing Accruals Signal...")

accruals_calc <- comp_acc_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(
    # Calculate Changes (Delta)
    # Use lag() to get t-1 values
    d_act = act - lag(act),
    d_che = che - lag(che),
    d_lct = lct - lag(lct),
    d_dlc = dlc - lag(dlc),
    d_txp = txp - lag(txp),
    
    # Lagged Assets for Scaling (Sloan 1996 uses Average Assets, but JKP uses Lagged)
    at_lag = lag(at)
  ) |>
  ungroup() |>
  mutate(
    # Operating Accruals Formula (Balance Sheet Method)
    # (Delta CA - Delta Cash) - (Delta CL - Delta STD - Delta TP) - Dep
    accruals_raw = (d_act - d_che) - (d_lct - d_dlc - d_txp) - dp,
    
    # Scale by Total Assets (t-1)
    oaccruals_at = accruals_raw / at_lag
  ) |>
  # Filter for valid data
  filter(
    !is.na(oaccruals_at) & is.finite(oaccruals_at) & at_lag > 0
  ) |>
  # Lag the signal to July of year t
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
  select(gvkey, sorting_date, oaccruals_at)

# 3. Merge with June Market Data for Sorting -----------------------------------
sorting_data <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, gvkey, sorting_date, mktcap_june = mktcap) |>
  inner_join(accruals_calc, by = c("gvkey", "sorting_date"))

# 4. Portfolio Construction (NYSE Breakpoints) ---------------------------------
assign_acc_portfolio <- function(data) {
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(oaccruals_at) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  
  data |>
    mutate(portfolio = findInterval(oaccruals_at, nyse_breaks, all.inside = TRUE))
}

portfolios_formed <- sorting_data |>
  group_by(sorting_date) |>
  group_split() |>
  map_dfr(assign_acc_portfolio) |>
  select(permno, sorting_date, portfolio_acc = portfolio)

# 5. Factor Returns (Value Weighted) -------------------------------------------
acc_factor <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
  group_by(date, portfolio_acc) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_acc, values_from = ret, names_prefix = "Q") |>
  mutate(
    # DIRECTION: High Accruals (Q5) predict LOW returns.
    # Therefore, we go LONG Q1 (Low Accruals) and SHORT Q5 (High Accruals).
    oaccruals_at_replicated = Q1 - Q5
  ) |>
  select(date, oaccruals_at_replicated)

# 6. Save & Report -------------------------------------------------------------
dbWriteTable(tidy_finance, "factor_accruals", acc_factor, overwrite = TRUE)

stats_rep <- acc_factor |> summarize(
  Mean_Ann = mean(oaccruals_at_replicated) * 12,
  Sharpe = Mean_Ann / (sd(oaccruals_at_replicated) * sqrt(12))
)
print("--- Replicated Accruals (Sloan 1996) Stats ---")
print(stats_rep)

# ==============================================================================
# VALIDATION: Replicated Accruals vs. Benchmark
# ==============================================================================

# 1. Load Data (Aligned Dates)
replicated_factor <- acc_factor |>
  mutate(
    date = ymd(date),
    date = ceiling_date(date, "month") - days(1)
  ) |>
  select(date, ret_replicated = oaccruals_at_replicated)

# Load Uploaded Benchmark
# (Assuming the file is in the working directory)
bench_file <- "benchmark_data/[usa]_[oaccruals_at]_[monthly]_[vw_cap].csv" 
if(file.exists(bench_file)) {
  benchmark_factor <- read_csv(bench_file) |>
    mutate(date = ymd(date)) |>
    select(date, ret_benchmark = ret)
  
  # 2. Merge & Validate
  comparison_data <- replicated_factor |>
    inner_join(benchmark_factor, by = "date") |>
    arrange(date) |>
    drop_na()
  
  print(paste("Matched Observations:", nrow(comparison_data)))
  
  if(nrow(comparison_data) > 0) {
    stats <- comparison_data |>
      summarize(
        Correlation = cor(ret_replicated, ret_benchmark),
        Alpha_Ann = coef(lm(ret_replicated ~ ret_benchmark))[1] * 12,
        Beta = coef(lm(ret_replicated ~ ret_benchmark))[2],
        Tracking_Error_Ann = sd(ret_replicated - ret_benchmark) * sqrt(12)
      )
    
    print("--- VALIDATION METRICS (Accruals Replication) ---")
    print(stats)
    
    # 3. Plot
    plot_data <- comparison_data |>
      mutate(
        Cum_Rep = cumprod(1 + ret_replicated) - 1,
        Cum_Bench = cumprod(1 + ret_benchmark) - 1
      ) |>
      pivot_longer(cols = c(Cum_Rep, Cum_Bench), names_to = "Series", values_to = "Cumulative_Return") |>
      mutate(Series = if_else(Series == "Cum_Rep", "Replicated (Yours)", "Benchmark (JKP)"))
    
    p <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Series, linetype = Series)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = c("Benchmark (JKP)" = "purple", "Replicated (Yours)" = "black")) +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = "Validation: Operating Accruals (Sloan, 1996)",
        subtitle = "Strategy: Long Low Accruals (Q1) - Short High Accruals (Q5)",
        y = "Cumulative Excess Return", x = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    print(p)
  } else {
    print("Error: No matching dates found.")
  }
} else {
  print("Benchmark file not found. Ensure '[usa]_[oaccruals_at]_[monthly]_[vw_cap].csv' is in the directory.")
}

dbDisconnect(tidy_finance)