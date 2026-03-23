# ==============================================================================
# BUILDER TASK: Earnings-to-Price (Basu 1983) Replication
# ==============================================================================
# Formula: Net Income (ni) / Market Equity (Dec t-1)
# ==============================================================================

# 1. Load Data -----------------------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(scales)
library(lubridate)
library(RPostgres)

# Connect to DB
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

message("Connecting to WRDS to ensure latest Compustat Data...")
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# Fetch Net Income if not already present or to ensure freshness
comp_ni_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  select(gvkey, datadate, ni) |>
  collect() |>
  mutate(datadate = ymd(datadate))

dbDisconnect(wrds)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect() |> mutate(date = ymd(date))

# 2. Construct Signal (Earnings / Price) ---------------------------------------
# Step A: Get Annual Net Income
earnings_signal <- comp_ni_raw |>
  select(gvkey, datadate, ni) |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |> # Lag to July
  drop_na()

# Step B: Get Dec t-1 Market Cap for the Denominator
dec_market_cap <- crsp_monthly |>
  filter(month(date) == 12) |>
  select(permno, gvkey, mktcap_dec = mktcap, date) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |> # Lag to July
  select(-date)

# Step C: Combine to create 'ni_me'
ep_signal <- earnings_signal |>
  inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
  mutate(ni_me = ni / mktcap_dec) |>
  select(permno, sorting_date, ni_me) |>
  filter(!is.na(ni_me))

# 3. Merge with June Market Data for Sorting -----------------------------------
sorting_data <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, sorting_date, mktcap_june = mktcap) |>
  inner_join(ep_signal, by = c("permno", "sorting_date"))

# 4. Portfolio Construction (NYSE Breakpoints) ---------------------------------
assign_ep_portfolio <- function(data) {
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(ni_me) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  
  data |>
    mutate(portfolio = findInterval(ni_me, nyse_breaks, all.inside = TRUE))
}

portfolios_formed <- sorting_data |>
  group_by(sorting_date) |>
  group_split() |>
  map_dfr(assign_ep_portfolio) |>
  select(permno, sorting_date, portfolio_ep = portfolio)

# 5. Factor Returns ------------------------------------------------------------
ep_factor <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
  group_by(date, portfolio_ep) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_ep, values_from = ret, names_prefix = "Q") |>
  mutate(ni_me_replicated = Q5 - Q1) |> 
  select(date, ni_me_replicated)

# 6. Save & Report -------------------------------------------------------------
dbWriteTable(tidy_finance, "factor_earnings_price", ep_factor, overwrite = TRUE)

stats <- ep_factor |> summarize(
  Mean_Ann = mean(ni_me_replicated) * 12,
  Sharpe = Mean_Ann / (sd(ni_me_replicated) * sqrt(12))
)
print("--- Earnings-to-Price Replicated ---")
print(stats)

# ==============================================================================
# VALIDATION SCRIPT: Earnings-to-Price (Replicated vs. Benchmark)
# ==============================================================================

# 2. Load Data -----------------------------------------------------------------

# A. Load Replicated Factor 
replicated_factor <- tbl(tidy_finance, "factor_earnings_price") |>
  collect() |>
  mutate(
    date = ymd(date),
    # Convert 1st-of-month to End-of-month
    date = ceiling_date(date, "month") - days(1) 
  ) |>
  select(date, ret_replicated = ni_me_replicated)

# B. Load Benchmark CSV
benchmark_factor <- read_csv("benchmark_data/[usa]_[ni_me]_[monthly]_[vw_cap].csv") |>
  mutate(date = ymd(date)) |>
  select(date, ret_benchmark = ret)

# 3. Merge & Clean -------------------------------------------------------------
comparison_data <- replicated_factor |>
  inner_join(benchmark_factor, by = "date") |>
  arrange(date) |>
  drop_na()

print(paste("Matched Observations:", nrow(comparison_data)))

# 4. Statistical Validation ----------------------------------------------------
if(nrow(comparison_data) > 0) {
  stats <- comparison_data |>
    summarize(
      Correlation = cor(ret_replicated, ret_benchmark),
      Alpha_Ann = coef(lm(ret_replicated ~ ret_benchmark))[1] * 12,
      Beta = coef(lm(ret_replicated ~ ret_benchmark))[2],
      Tracking_Error_Ann = sd(ret_replicated - ret_benchmark) * sqrt(12)
    )
  
  print("--- VALIDATION METRICS (Earnings-to-Price) ---")
  print(stats)
  
  # 5. Visual Validation (Cumulative Returns) ----------------------------------
  plot_data <- comparison_data |>
    mutate(
      Cum_Rep = cumprod(1 + ret_replicated) - 1,
      Cum_Bench = cumprod(1 + ret_benchmark) - 1
    ) |>
    pivot_longer(cols = c(Cum_Rep, Cum_Bench), 
                 names_to = "Series", 
                 values_to = "Cumulative_Return") |>
    mutate(
      Series = if_else(Series == "Cum_Rep", "Replicated (Yours)", "Benchmark (JKP)")
    )
  
  p <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Series, linetype = Series)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Benchmark (JKP)" = "black", "Replicated (Yours)" = "blue")) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Factor Replication: Earnings-to-Price (Basu, 1983)",
      subtitle = "Long High E/P - Short Low E/P (Value Weighted)\nComparison: Your CIZ Replication vs. JKP Historical Benchmark",
      y = "Cumulative Excess Return",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  
} else {
  print("Error: No matching dates found. Check date alignment logic.")
}

# Cleanup
dbDisconnect(tidy_finance)