# ==============================================================================
# BUILDER TASK: Free Cash Flow to Price (fcf_me) Replication
# ==============================================================================
# Benchmark: Lakonishok et al. (1994) / JKP 'fcf_me'
# Formula: (Operating Cash Flow - CAPX) / Market Equity (Dec t-1)
# Data Handling: 
#   - Post-1987: oancf - capx
#   - Pre-1987: (ib + dp) - capx
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

message("Connecting to WRDS for FCF components...")
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# Fetch Variables: OANCF (Ops Cash Flow), CAPX (Capital Expenditures), IB, DP
comp_fcf_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  select(gvkey, datadate, oancf, capx, ib, dp) |>
  collect() |>
  mutate(datadate = ymd(datadate))

dbDisconnect(wrds)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect() |> mutate(date = ymd(date))

# 2. Construct Signal (Free Cash Flow / Price) ---------------------------------
message("Constructing FCF Signal...")

fcf_signal <- comp_fcf_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(
    # Construct Proxy for Cash Flow (Pre-1987 logic)
    # Primary: OANCF. Backup: Earnings (IB) + Depreciation (DP)
    cash_flow_gross = coalesce(oancf, ib + dp),
    
    # Free Cash Flow = Cash Flow - Capital Expenditures
    # Note: If CAPX is missing, we assume 0 (common in some research, but risky)
    # Better approach: filter out missing CAPX to be precise
    fcf = cash_flow_gross - coalesce(capx, 0)
  ) |>
  ungroup() |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |> # Lag to July
  filter(!is.na(fcf)) |>
  select(gvkey, sorting_date, fcf)

# Get Dec t-1 Market Cap for the Denominator
dec_market_cap <- crsp_monthly |>
  filter(month(date) == 12) |>
  select(permno, gvkey, mktcap_dec = mktcap, date) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |>
  select(-date)

# Combine to create 'fcf_me'
fcf_data <- fcf_signal |>
  inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
  mutate(fcf_me = fcf / mktcap_dec) |>
  filter(!is.na(fcf_me) & is.finite(fcf_me)) |>
  select(permno, sorting_date, fcf_me)

# 3. Merge with June Market Data for Sorting -----------------------------------
sorting_data <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, sorting_date, mktcap_june = mktcap) |>
  inner_join(fcf_data, by = c("permno", "sorting_date"))

# 4. Portfolio Construction (NYSE Breakpoints) ---------------------------------
assign_fcf_portfolio <- function(data) {
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(fcf_me) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  
  data |>
    mutate(portfolio = findInterval(fcf_me, nyse_breaks, all.inside = TRUE))
}

portfolios_formed <- sorting_data |>
  group_by(sorting_date) |>
  group_split() |>
  map_dfr(assign_fcf_portfolio) |>
  select(permno, sorting_date, portfolio_fcf = portfolio)

# 5. Factor Returns ------------------------------------------------------------
fcf_factor <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
  group_by(date, portfolio_fcf) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_fcf, values_from = ret, names_prefix = "Q") |>
  mutate(fcf_me_replicated = Q5 - Q1) |> # Long High FCF/P, Short Low FCF/P
  select(date, fcf_me_replicated)

# 6. Save & Report -------------------------------------------------------------
dbWriteTable(tidy_finance, "factor_fcf_price", fcf_factor, overwrite = TRUE)

stats_rep <- fcf_factor |> summarize(
  Mean_Ann = mean(fcf_me_replicated) * 12,
  Sharpe = Mean_Ann / (sd(fcf_me_replicated) * sqrt(12))
)
print("--- Replicated Free Cash Flow (FCF/P) Stats ---")
print(stats_rep)

# ==============================================================================
# VALIDATION: Replicated FCF vs. Benchmark FCF (Direct Match)
# ==============================================================================

# 1. Load Data (Aligned Dates)
replicated_factor <- fcf_factor |>
  mutate(
    date = ymd(date),
    date = ceiling_date(date, "month") - days(1)
  ) |>
  select(date, ret_replicated = fcf_me_replicated)

# Load Uploaded Benchmark (FCF_ME)
benchmark_factor <- read_csv("benchmark_data/[usa]_[fcf_me]_[monthly]_[vw_cap].csv") |>
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
  
  print("--- VALIDATION METRICS (FCF/P Replication) ---")
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
    scale_color_manual(values = c("Benchmark (JKP)" = "black", "Replicated (Yours)" = "green")) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Validation: Free Cash Flow to Price (Lakonishok et al., 1994)",
      subtitle = "Comparison: Replicated FCF (OCF - CAPX) vs Benchmark FCF",
      y = "Cumulative Excess Return", x = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
} else {
  print("Error: No matching dates found.")
}

dbDisconnect(tidy_finance)