# ==============================================================================
# BUILDER TASK: Cash Flow to Price (ocf_me) Replication
# ==============================================================================
# Methodology: Desai et al. (2004) / JKP 'ocf_me'
# Formula: Operating Cash Flow (OANCF) / Market Equity (Dec t-1)
# Data Extension: Pre-1987 (SFAS 95), proxy OCF = IB (Earnings) + DP (Depreciation)
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

message("Connecting to WRDS for Cash Flow Data...")
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# Fetch specific Cash Flow variables
# oancf: Operating Activities - Net Cash Flow (Post-1987)
# ib: Income Before Extraordinary Items
# dp: Depreciation and Amortization
comp_cf_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  select(gvkey, datadate, oancf, ib, dp) |>
  collect() |>
  mutate(datadate = ymd(datadate))

dbDisconnect(wrds)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect() |> mutate(date = ymd(date))

# 2. Construct Signal (OCF / Price) --------------------------------------------
message("Constructing OCF Signal...")

ocf_signal <- comp_cf_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(
    # Logic: Use OANCF if available (Post-1987), else Proxy (IB + DP)
    ocf = coalesce(oancf, ib + dp)
  ) |>
  ungroup() |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |> # Lag to July
  drop_na(ocf) |>
  select(gvkey, sorting_date, ocf)

# Get Dec t-1 Market Cap for the Denominator
dec_market_cap <- crsp_monthly |>
  filter(month(date) == 12) |>
  select(permno, gvkey, mktcap_dec = mktcap, date) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |>
  select(-date)

# Combine to create 'ocf_me'
# Join by gvkey for accounting, keep permno for returns
ocf_data <- ocf_signal |>
  inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
  mutate(
    ocf_me = ocf / mktcap_dec
  ) |>
  filter(!is.na(ocf_me) & is.finite(ocf_me)) |>
  select(permno, sorting_date, ocf_me)

# 3. Merge with June Market Data for Sorting -----------------------------------
sorting_data <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, sorting_date, mktcap_june = mktcap) |>
  inner_join(ocf_data, by = c("permno", "sorting_date"))

# 4. Portfolio Construction (NYSE Breakpoints) ---------------------------------
assign_ocf_portfolio <- function(data) {
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(ocf_me) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  
  data |>
    mutate(portfolio = findInterval(ocf_me, nyse_breaks, all.inside = TRUE))
}

portfolios_formed <- sorting_data |>
  group_by(sorting_date) |>
  group_split() |>
  map_dfr(assign_ocf_portfolio) |>
  select(permno, sorting_date, portfolio_ocf = portfolio)

# 5. Factor Returns ------------------------------------------------------------
ocf_factor <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
  group_by(date, portfolio_ocf) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_ocf, values_from = ret, names_prefix = "Q") |>
  mutate(ocf_me_replicated = Q5 - Q1) |> # Long High OCF/P, Short Low OCF/P
  select(date, ocf_me_replicated)

# 6. Save & Report -------------------------------------------------------------
dbWriteTable(tidy_finance, "factor_ocf_price", ocf_factor, overwrite = TRUE)

stats_rep <- ocf_factor |> summarize(
  Mean_Ann = mean(ocf_me_replicated) * 12,
  Sharpe = Mean_Ann / (sd(ocf_me_replicated) * sqrt(12))
)
print("--- Replicated Cash Flow to Price (OCF/P) Stats ---")
print(stats_rep)

# ==============================================================================
# VALIDATION: Replicated OCF vs. Benchmark FCF
# ==============================================================================

# 1. Load Data (Aligned Dates) -------------------------------------------------
replicated_factor <- ocf_factor |>
  mutate(
    date = ymd(date),
    date = ceiling_date(date, "month") - days(1)
  ) |>
  select(date, ret_replicated = ocf_me_replicated)

# Load Uploaded Benchmark (FCF_ME)
benchmark_factor <- read_csv("benchmark_data/[usa]_[ocf_me]_[monthly]_[vw_cap].csv") |>
  mutate(date = ymd(date)) |>
  select(date, ret_benchmark = ret)

# 2. Merge & Validate ----------------------------------------------------------
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
  
  print("--- VALIDATION METRICS (Replicated OCF vs Benchmark FCF) ---")
  print(stats)
  print("Note: Benchmark is Free Cash Flow (FCF). Replication is Operating Cash Flow (OCF). Deviation expected (CAPX).")
  
  # 3. Plot --------------------------------------------------------------------
  plot_data <- comparison_data |>
    mutate(
      Cum_Rep = cumprod(1 + ret_replicated) - 1,
      Cum_Bench = cumprod(1 + ret_benchmark) - 1
    ) |>
    pivot_longer(cols = c(Cum_Rep, Cum_Bench), names_to = "Series", values_to = "Cumulative_Return") |>
    mutate(Series = if_else(Series == "Cum_Rep", "Replicated (OCF)", "Benchmark (FCF)"))
  
  p <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Series, linetype = Series)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Benchmark (FCF)" = "gray40", "Replicated (OCF)" = "blue")) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Factor Check: Operating vs. Free Cash Flow",
      subtitle = "Comparison: Replicated OCF (Desai 2004) vs Benchmark FCF (Lakonishok 1994)",
      y = "Cumulative Excess Return", x = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
} else {
  print("Error: No matching dates found.")
}

dbDisconnect(tidy_finance)