# ==============================================================================
# BUILDER TASK: Asset Growth (Cooper, Gulen, Schill 2008) Replication
# ==============================================================================
# Factor: Investment / Asset Growth ('at_gr1')
# Formula: Percentage change in Total Assets (AT) year-over-year.
# Direction: Low Growth outperforms High Growth (Reversal).
# Strategy: Long Q1 (Low Growth) - Short Q5 (High Growth).
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

message("Connecting to WRDS for Total Assets...")
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# Fetch Total Assets (AT)
comp_at_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  select(gvkey, datadate, at) |>
  collect() |>
  mutate(datadate = ymd(datadate))

dbDisconnect(wrds)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect() |> mutate(date = ymd(date))

# 2. Construct Signal (Asset Growth) -------------------------------------------
message("Constructing Asset Growth Signal...")

# We need strictly year-over-year growth. 
# Best practice: Create a 'year' column and self-join t with t-1.

at_annual <- comp_at_raw |>
  mutate(year = year(datadate)) |>
  # Handle duplicates (rare, but take latest datadate per year)
  group_by(gvkey, year) |>
  filter(datadate == max(datadate)) |>
  ungroup() |>
  select(gvkey, year, datadate, at)

at_lagged <- at_annual |>
  mutate(year = year + 1) |> # Shift year forward to match t with t-1
  select(gvkey, year, at_lag = at)

at_growth_signal <- at_annual |>
  inner_join(at_lagged, by = c("gvkey", "year")) |>
  mutate(
    # Formula: (AT_t - AT_t-1) / AT_t-1
    at_gr1 = (at - at_lag) / at_lag
  ) |>
  filter(!is.na(at_gr1) & is.finite(at_gr1) & at_lag > 0) |>
  # Lag the signal to July of year t
  mutate(sorting_date = ymd(str_c(year + 1, "0701"))) |>
  select(gvkey, sorting_date, at_gr1)

# 3. Merge with June Market Data for Sorting -----------------------------------
sorting_data <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, gvkey, sorting_date, mktcap_june = mktcap) |>
  inner_join(at_growth_signal, by = c("gvkey", "sorting_date"))

# 4. Portfolio Construction (NYSE Breakpoints) ---------------------------------
assign_at_portfolio <- function(data) {
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(at_gr1) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  
  data |>
    mutate(portfolio = findInterval(at_gr1, nyse_breaks, all.inside = TRUE))
}

portfolios_formed <- sorting_data |>
  group_by(sorting_date) |>
  group_split() |>
  map_dfr(assign_at_portfolio) |>
  select(permno, sorting_date, portfolio_at = portfolio)

# 5. Factor Returns (Low Growth Minus High Growth) -----------------------------
at_factor <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
  group_by(date, portfolio_at) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_at, values_from = ret, names_prefix = "Q") |>
  mutate(
    # CRITICAL: We go LONG Low Growth (Q1) and SHORT High Growth (Q5)
    # The benchmark file likely stores this as 'Q1 - Q5' or vice-versa. 
    # JKP documentation usually defines factor direction so premium is positive.
    # Cooper et al. (2008) finds Low Growth > High Growth.
    at_gr1_replicated = Q1 - Q5
  ) |>
  select(date, at_gr1_replicated)

# 6. Save & Report -------------------------------------------------------------
dbWriteTable(tidy_finance, "factor_asset_growth", at_factor, overwrite = TRUE)

stats_rep <- at_factor |> summarize(
  Mean_Ann = mean(at_gr1_replicated) * 12,
  Sharpe = Mean_Ann / (sd(at_gr1_replicated) * sqrt(12))
)
print("--- Replicated Asset Growth (Low - High) Stats ---")
print(stats_rep)

# ==============================================================================
# VALIDATION: Replicated Asset Growth vs. Benchmark
# ==============================================================================

# 1. Load Data (Aligned Dates)
replicated_factor <- at_factor |>
  mutate(
    date = ymd(date),
    date = ceiling_date(date, "month") - days(1)
  ) |>
  select(date, ret_replicated = at_gr1_replicated)

# Load Uploaded Benchmark (at_gr1)
benchmark_factor <- read_csv("benchmark_data/[usa]_[at_gr1]_[monthly]_[vw_cap].csv") |>
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
  
  print("--- VALIDATION METRICS (Asset Growth Replication) ---")
  print(stats)
  
  # 3. Plot
  plot_data <- comparison_data |>
    mutate(
      Cum_Rep = cumprod(1 + ret_replicated) - 1,
      Cum_Bench = cumprod(1 + ret_benchmark) - 1
    ) |>
    pivot_longer(cols = c(Cum_Rep, Cum_Bench), names_to = "Series", values_to = "Cumulative_Return") |>
    mutate(Series = if_else(Series == "Cum_Rep", "Replicated (Yours)", "Benchmark (Cooper 2008)"))
  
  p <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Series, linetype = Series)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Benchmark (Cooper 2008)" = "darkred", "Replicated (Yours)" = "black")) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Validation: Asset Growth (Cooper et al., 2008)",
      subtitle = "Comparison: Replicated vs JKP Benchmark\nStrategy: Long Low Growth (Q1) - Short High Growth (Q5)",
      y = "Cumulative Excess Return", x = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
} else {
  print("Error: No matching dates found.")
}

dbDisconnect(tidy_finance)