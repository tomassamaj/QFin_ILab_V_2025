# ==============================================================================
# BUILDER TASK: Gross Profitability (Novy-Marx 2013) Replication
# ==============================================================================
# Objective: Replicate the 'Gross Profitability' factor with minimal tracking error.
# Reference: Novy-Marx (2013), "The other side of value".
# Formula: (Total Revenue - COGS) / Total Assets.
# ==============================================================================

# 1. Setup ---------------------------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(scales)
library(lubridate)
library(RPostgres) # Needed for WRDS Refetch

# Connect to your existing Local DB
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

# 2. Targeted WRDS Fetch (Updating for Accuracy) -------------------------------
# We need 'revt' (Total Revenue) for Novy-Marx, your previous fetch had 'sale'.
# If you are not connected to WRDS, you can skip this and use 'sale' in Step 3.

message("Connecting to WRDS for precision update...")
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# Fetch specific Profitability variables
comp_prof_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  # Novy-Marx specific items: REVT (Revenue), COGS (Cost of Goods), AT (Assets)
  select(gvkey, datadate, revt, cogs, at) |>
  collect() |>
  mutate(datadate = ymd(datadate))

dbDisconnect(wrds)

# 3. Construct the Signal ------------------------------------------------------
message("Constructing Gross Profitability Signal...")

gp_signal <- comp_prof_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(
    # Novy-Marx (2013) Definition:
    # Numerator: Gross Profit = Revenue - Cost of Goods Sold
    gp = revt - cogs,
    # Denominator: Total Assets
    # Signal: GP / AT
    gp_at = gp / at
  ) |>
  ungroup() |>
  # Clean data: Assets must be positive, Signal must exist
  filter(at > 0 & !is.na(gp_at)) |>
  select(gvkey, datadate, gp_at)

# 4. Timeline Alignment (The "Fama-French" Lag) --------------------------------
# Accounting data for fiscal year ending in t-1 is valid for returns starting July t.

gp_signal_lagged <- gp_signal |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
  select(gvkey, sorting_date, gp_at)

# 5. Load Market Data & Link ---------------------------------------------------
# We need the CRSP Monthly data you already processed (for returns & market cap)
crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> 
  select(permno, gvkey, date, ret_excess, mktcap, mktcap_lag, exchange) |>
  collect() |> 
  mutate(date = ymd(date))

# Create the Sorting Variable table (June Market Cap + GP Signal)
# We align the GP signal (valid from July t) with Stocks existing in June t
sorting_data <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |> # Shift June to July for the join
  select(permno, gvkey, exchange, sorting_date, mktcap_june = mktcap) |>
  inner_join(gp_signal_lagged, join_by(gvkey, sorting_date)) |>
  drop_na()

# 6. Portfolio Construction (NYSE Breakpoints) ---------------------------------
# CRITICAL FOR TRACKING ERROR: 
# Novy-Marx uses NYSE-only breakpoints for the quintiles.

assign_nm_portfolio <- function(data) {
  # Define breakpoints using ONLY NYSE stocks (Exchange code 1 or "NYSE")
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(gp_at) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7) # Type 7 is standard in R/Finance
  
  # Apply these breakpoints to ALL stocks (NYSE, AMEX, NASDAQ)
  data |>
    mutate(
      portfolio = findInterval(gp_at, nyse_breaks, all.inside = TRUE)
    )
}

# Apply sorting per year
portfolios_formed <- sorting_data |>
  group_by(sorting_date) |>
  group_split() |>
  map_dfr(assign_nm_portfolio) |>
  select(permno, sorting_date, portfolio_gp = portfolio)

# 7. Calculate Factor Returns (Value Weighted) ---------------------------------
# Returns run from July t to June t+1

gp_factor_monthly <- crsp_monthly |>
  # Assign the sorting year to each month
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios_formed, join_by(permno, sorting_date)) |>
  group_by(date, portfolio_gp) |>
  # Value-weighting using lagged market cap (standard)
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_gp, values_from = ret, names_prefix = "Q") |>
  mutate(
    # Novy-Marx Factor: Long High Profitability (Q5) - Short Low Profitability (Q1)
    pmu_replicated = Q5 - Q1
  ) |>
  select(date, pmu_replicated, everything())

# 8. Store and Verify ----------------------------------------------------------

# Save to DB
dbWriteTable(tidy_finance, "factor_gross_profitability", gp_factor_monthly, overwrite = TRUE)

# Quick Validation Stats
stats <- gp_factor_monthly |>
  summarize(
    Mean_Ann = mean(pmu_replicated) * 12,
    Vol_Ann = sd(pmu_replicated) * sqrt(12),
    Sharpe = Mean_Ann / Vol_Ann
  )

print("--- Replicated Gross Profitability (PMU) Stats ---")
print(stats)
print("Note: Returns reflect CIZ tape (Daily Reinvestment) as per Schwarz et al. (2025)")

dbDisconnect(tidy_finance)



# ==============================================================================
# FIX: Date Alignment for Validation
# ==============================================================================

# A. Load Replicated Factor (Force End-of-Month Date)
replicated_factor <- tbl(tidy_finance, "factor_gross_profitability") |>
  collect() |>
  mutate(
    date = ymd(date),
    # Convert 1st-of-month to End-of-month to match benchmark
    date = ceiling_date(date, "month") - days(1) 
  ) |>
  select(date, ret_replicated = pmu_replicated)

# B. Load Benchmark CSV (Already End-of-Month)
benchmark_factor <- read_csv("[usa]_[gp_at]_[monthly]_[vw_cap].csv") |>
  mutate(date = ymd(date)) |>
  select(date, ret_benchmark = ret)

# 3. Merge & Clean (Should now find matches)
comparison_data <- replicated_factor |>
  inner_join(benchmark_factor, by = "date") |>
  arrange(date) |>
  drop_na()

# Check if data exists now
print(paste("Matched Observations:", nrow(comparison_data)))

# 4. Statistical Validation (Retry)
if(nrow(comparison_data) > 0) {
  stats <- comparison_data |>
    summarize(
      Correlation = cor(ret_replicated, ret_benchmark),
      Alpha_Ann = coef(lm(ret_replicated ~ ret_benchmark))[1] * 12,
      Beta = coef(lm(ret_replicated ~ ret_benchmark))[2],
      Tracking_Error_Ann = sd(ret_replicated - ret_benchmark) * sqrt(12)
    )
  print("--- VALIDATION METRICS ---")
  print(stats)
  
  # 5. Visual Validation
  plot_data <- comparison_data |>
    mutate(
      Cum_Rep = cumprod(1 + ret_replicated) - 1,
      Cum_Bench = cumprod(1 + ret_benchmark) - 1
    ) |>
    pivot_longer(cols = c(Cum_Rep, Cum_Bench), 
                 names_to = "Series", 
                 values_to = "Cumulative_Return")
  
  p <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Series)) +
    geom_line(linewidth = 1) +
    labs(title = "Validation: Replicated vs Benchmark (Fixed Date Alignment)") +
    theme_minimal()
  print(p)
} else {
  print("Error: Still no matching dates. Check date ranges.")
  print(summary(replicated_factor$date))
  print(summary(benchmark_factor$date))
}