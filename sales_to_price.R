# ==============================================================================
# BUILDER TASK: Sales-to-Price (Barbee, Mukherji, Raines 1996) Replication
# ==============================================================================
# Factor: Value / Sales-to-Price ('sale_me')
# Formula: Total Revenue (SALE) / Market Equity (Dec t-1).
# Strategy: Long High S/P (Value) - Short Low S/P (Growth).
# ==============================================================================

# 1. Setup & Data Loading ------------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(scales)
library(lubridate)

# Connect to DB
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> collect() |> mutate(date = ymd(date))
compustat <- tbl(tidy_finance, "compustat") |> collect() |> mutate(datadate = ymd(datadate))

# 2. Construct Signal (Sales / Price) ------------------------------------------
message("Constructing Sales-to-Price Signal...")

# Step A: Get Annual Sales (Revenue)
# Note: 'sale' is already in your compustat table from the master script.
sales_signal <- compustat |>
  select(gvkey, datadate, sale) |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |> # Lag to July
  drop_na(sale) |>
  filter(sale > 0)

# Step B: Get Dec t-1 Market Cap for the Denominator
dec_market_cap <- crsp_monthly |>
  filter(month(date) == 12) |>
  select(permno, gvkey, mktcap_dec = mktcap, date) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |> # Lag to July
  select(-date)

# Step C: Combine to create 'sale_me'
sp_signal <- sales_signal |>
  inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
  mutate(sale_me = sale / mktcap_dec) |>
  select(permno, sorting_date, sale_me) |>
  filter(!is.na(sale_me) & is.finite(sale_me))

# 3. Merge with June Market Data for Sorting -----------------------------------
sorting_data <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |>
  select(permno, exchange, sorting_date, mktcap_june = mktcap) |>
  inner_join(sp_signal, by = c("permno", "sorting_date"))

# 4. Portfolio Construction (NYSE Breakpoints) ---------------------------------
assign_sp_portfolio <- function(data) {
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(sale_me) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  
  data |>
    mutate(portfolio = findInterval(sale_me, nyse_breaks, all.inside = TRUE))
}

portfolios_formed <- sorting_data |>
  group_by(sorting_date) |>
  group_split() |>
  map_dfr(assign_sp_portfolio) |>
  select(permno, sorting_date, portfolio_sp = portfolio)

# 5. Factor Returns (Value Weighted) -------------------------------------------
sp_factor <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
  group_by(date, portfolio_sp) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_sp, values_from = ret, names_prefix = "Q") |>
  mutate(
    # Long High S/P (Value, Q5) - Short Low S/P (Growth, Q1)
    sale_me_replicated = Q5 - Q1
  ) |>
  select(date, sale_me_replicated)

# 6. Save & Report -------------------------------------------------------------
dbWriteTable(tidy_finance, "factor_sales_price", sp_factor, overwrite = TRUE)

stats_rep <- sp_factor |> summarize(
  Mean_Ann = mean(sale_me_replicated) * 12,
  Sharpe = Mean_Ann / (sd(sale_me_replicated) * sqrt(12))
)
print("--- Replicated Sales-to-Price Stats ---")
print(stats_rep)

dbDisconnect(tidy_finance)