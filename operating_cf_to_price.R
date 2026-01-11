# ==============================================================================
# MASTER SCRIPT: OCF/P with Point-in-Time (PiT) Alignment
# ==============================================================================
# Methodology: JKP 'ocf_me'
# 1. Numerator: OANCF (Post-87) or IB + DP (Pre-87 Robust Proxy)
# 2. Denominator: Market Equity (Monthly Lagged)
# 3. Timing: Available at FYE + 4 Months (Not fixed to July)
# ==============================================================================

# 1. Setup ---------------------------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(scales)
library(lubridate)
library(RPostgres)

tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# 2. Fetch Compustat (Robust) --------------------------------------------------
comp_cf_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  select(
    gvkey, datadate, fyear, sich,
    oancf, ib, dp # Simplified columns for robust proxy
  ) |>
  collect() |>
  mutate(
    datadate = ymd(datadate),
    sic = as.numeric(sich)
  )

# 3. Signal Construction (Point-in-Time) ---------------------------------------
ocf_pit <- comp_cf_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(
    # JKP/Robust Proxy: If OANCF missing, use IB + DP
    # This avoids the noise of 'wc_accruals' in the pre-1987 era
    proxy_ocf = replace_na(ib, 0) + replace_na(dp, 0),
    ocf_final = coalesce(oancf, proxy_ocf)
  ) |>
  ungroup() |>
  drop_na(ocf_final) |>
  mutate(
    # --- STRUCTURAL FIX: TIMING ---
    # Data is assumed public 4 months after FYE. 
    # Valid until the NEXT annual report comes out (or 12 months max).
    public_date = floor_date(datadate %m+% months(4), "month"),
    end_date    = floor_date(datadate %m+% months(16), "month")
  ) |>
  select(gvkey, public_date, end_date, ocf_final, sic)

# 4. Monthly Expansion & Merging -----------------------------------------------
# Instead of a simple range join, we expand the Compustat data to monthly rows
# based on the "public_date". This ensures we always use the latest known data.

# A. Create Monthly Spine for Fundamentals
monthly_fund_spine <- ocf_pit |>
  mutate(date = map2(public_date, end_date, ~ seq(.x, .y - days(1), by = "month"))) |>
  unnest(date) |>
  select(gvkey, date, ocf_final, sic) |>
  # Handle Overlaps: If two reports cover the same month (rare), take the latest
  group_by(gvkey, date) |>
  slice_tail(n = 1) |> 
  ungroup()

# B. Merge with CRSP
crsp_monthly_clean <- tbl(tidy_finance, "crsp_monthly") |> 
  collect() |> 
  mutate(date = ymd(date)) |>
  select(permno, gvkey, date, mktcap_lag, ret_excess, exchange, siccd) |>
  # Floor CRSP dates to month-start to match spine
  mutate(date_month = floor_date(date, "month"))

merged_signal <- crsp_monthly_clean |>
  inner_join(monthly_fund_spine, by = c("gvkey", "date_month" = "date")) |>
  mutate(
    # Industry Filter
    final_sic = coalesce(sic, as.numeric(siccd))
  ) |>
  filter(!(final_sic >= 6000 & final_sic <= 6999)) |>
  mutate(
    # Ratio: Annual OCF / Monthly Lagged Market Cap
    ocf_me = ocf_final / mktcap_lag
  ) |>
  filter(is.finite(ocf_me)) |>
  select(permno, date, exchange, mktcap_lag, ret_excess, ocf_me)

# 5. Portfolio Sorts -----------------------------------------------------------
assign_ocf_portfolio_monthly <- function(data) {
  nyse_breaks <- data |>
    filter(exchange == "NYSE") |>
    pull(ocf_me) |>
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
  
  data |>
    mutate(portfolio = findInterval(ocf_me, nyse_breaks, all.inside = TRUE))
}

portfolios_monthly <- merged_signal |>
  group_by(date) |>
  group_split() |>
  map_dfr(assign_ocf_portfolio_monthly) |>
  select(permno, date, portfolio_ocf = portfolio)

# 6. Returns -------------------------------------------------------------------
ocf_factor <- merged_signal |>
  inner_join(portfolios_monthly, by = c("permno", "date")) |>
  group_by(date, portfolio_ocf) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio_ocf, values_from = ret, names_prefix = "Q") |>
  mutate(
    ocf_me_replicated = Q5 - Q1
  ) |>
  select(date, ocf_me_replicated)

# 7. Validation ----------------------------------------------------------------
benchmark_factor <- read_csv("benchmark_data/[usa]_[ocf_me]_[monthly]_[vw_cap].csv") |>
  mutate(date = floor_date(ymd(date), "month")) |>
  select(date, ret_benchmark = ret)

validation <- ocf_factor |>
  inner_join(benchmark_factor, by = "date") |>
  drop_na()

stats <- validation |>
  summarize(
    Correlation = cor(ocf_me_replicated, ret_benchmark),
    Alpha = coef(lm(ocf_me_replicated ~ ret_benchmark))[1],
    Beta = coef(lm(ocf_me_replicated ~ ret_benchmark))[2]
  )

print("--- PiT VALIDATION STATS ---")
print(stats)

# Plot
plot_data <- validation |>
  mutate(
    Cum_Rep = cumprod(1 + ocf_me_replicated) - 1,
    Cum_Bench = cumprod(1 + ret_benchmark) - 1
  ) |>
  pivot_longer(cols = c(Cum_Rep, Cum_Bench), names_to = "Series", values_to = "Cumulative_Return")

p <- ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Series)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("gray40", "blue")) +
  theme_minimal() +
  labs(title = "Replication: OCF/P (Point-in-Time)", subtitle = "Method: FYE+4 Month Lag")

print(p)

# Save
dbWriteTable(tidy_finance, "factor_ocf_price", ocf_factor, overwrite = TRUE)
dbDisconnect(tidy_finance)
dbDisconnect(wrds)