# ==============================================================================
# MASTER SCRIPT: Free Cash Flow / Price (fcf_me)
# ==============================================================================
# Benchmark: Lakonishok et al. (1994) / JKP 'fcf_me'
# Formula: (Operating Cash Flow - CAPX) / Market Equity (Lagged)
# ==============================================================================

# 1. Setup & Functions ---------------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(scales)
library(lubridate)
library(RPostgres)

# Custom Winsorize Function
winsorize_robust <- function(x, cut = 0.01) {
  if (all(is.na(x))) return(x)
  lims <- quantile(x, probs = c(cut, 1 - cut), na.rm = TRUE)
  x[x < lims[1]] <- lims[1]
  x[x > lims[2]] <- lims[2]
  return(x)
}

# Connect DBs
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

# 2. Fetch Compustat (Hybrid FCF) ----------------------------------------------
message("Fetching Compustat Data...")
comp_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= "1960-01-01"
  ) |>
  select(gvkey, datadate, fyear, sich, oancf, capx, ib, dp) |>
  collect() |>
  mutate(datadate = ymd(datadate), sic = as.numeric(sich))

# Construct Signal
comp_signal <- comp_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(
    # 1. Calculate Operating Cash Flow (Hybrid)
    proxy_ocf = replace_na(ib, 0) + replace_na(dp, 0),
    ocf_val = coalesce(oancf, proxy_ocf),
    
    # 2. Calculate Free Cash Flow (OCF - CAPX)
    # If CAPX is NA, we treat it as 0 (standard assumption for small firms)
    fcf_val = ocf_val - replace_na(capx, 0)
  ) |>
  ungroup() |>
  drop_na(fcf_val) |>
  mutate(
    public_date = floor_date(datadate %m+% months(4), "month"),
    end_date    = floor_date(datadate %m+% months(16), "month")
  ) |>
  select(gvkey, public_date, end_date, fcf_val, sic)

# 3. Monthly CRSP Merge --------------------------------------------------------
message("Merging with CRSP...")
crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |> 
  collect() |> 
  mutate(date = ymd(date), date_month = floor_date(date, "month")) |>
  filter(exchange %in% c("NYSE", "AMEX", "NASDAQ")) |> 
  select(permno, gvkey, date, date_month, mktcap_lag, ret_excess, exchange, siccd)

# Range Join
merged_data <- crsp_monthly |>
  inner_join(
    comp_signal, 
    by = join_by(gvkey, date_month >= public_date, date_month <= end_date)
  ) |>
  arrange(permno, date, desc(public_date)) |>
  distinct(permno, date, .keep_all = TRUE) |>
  mutate(
    final_sic = coalesce(sic, as.numeric(siccd)),
    # Signal: Free Cash Flow / Lagged Market Cap
    fcf_me = fcf_val / mktcap_lag
  ) |>
  # Filters: Exclude Financials (SIC 6000-6999)
  filter(!(final_sic >= 6000 & final_sic <= 6999)) |>
  filter(is.finite(fcf_me))

# 4. Winsorize & Sort (Deciles) ------------------------------------------------
message("Sorting Portfolios...")

merged_clean <- merged_data |>
  group_by(date) |>
  mutate(fcf_me = winsorize_robust(fcf_me, cut = 0.01)) |>
  ungroup()

assign_portfolios <- function(data) {
  # NYSE Breakpoints
  nyse_ref <- data |> filter(exchange == "NYSE")
  if(nrow(nyse_ref) < 50) nyse_ref <- data 
  
  breaks <- quantile(nyse_ref$fcf_me, probs = seq(0, 1, 0.1), na.rm = TRUE, type = 7)
  
  data |> mutate(portfolio = findInterval(fcf_me, breaks, all.inside = TRUE))
}

portfolios <- merged_clean |>
  group_by(date) |>
  group_split() |>
  map_dfr(assign_portfolios) |>
  select(permno, date, portfolio)

# 5. Returns -------------------------------------------------------------------
fcf_factor <- merged_clean |>
  inner_join(portfolios, by = c("permno", "date")) |>
  group_by(date, portfolio) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  pivot_wider(names_from = portfolio, values_from = ret, names_prefix = "P") |>
  mutate(fcf_me_replicated = P10 - P1) |>
  select(date, fcf_me_replicated)

# 6. Validation ----------------------------------------------------------------
benchmark <- read_csv("benchmark_data/[usa]_[fcf_me]_[monthly]_[vw_cap].csv") |>
  mutate(date = floor_date(ymd(date), "month")) |>
  select(date, ret_bench = ret)

stats <- fcf_factor |>
  inner_join(benchmark, by = "date") |>
  drop_na() |>
  summarize(
    Correlation = cor(fcf_me_replicated, ret_bench),
    Vol_Rep = sd(fcf_me_replicated) * sqrt(12),
    Vol_Bench = sd(ret_bench) * sqrt(12),
    Vol_Ratio = Vol_Rep / Vol_Bench,
    Alpha_Ann = coef(lm(fcf_me_replicated ~ ret_bench))[1] * 12
  )

print("--- FCF/P REPLICATION STATS ---")
print(stats)

# Plot
plot_data <- fcf_factor |>
  inner_join(benchmark, by = "date") |>
  mutate(
    Rep = cumprod(1 + fcf_me_replicated) - 1,
    Bench = cumprod(1 + ret_bench) - 1
  ) |>
  pivot_longer(cols = c(Rep, Bench), names_to = "Series", values_to = "CumRet")

p <- ggplot(plot_data, aes(x = date, y = CumRet, color = Series)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("gray40", "darkgreen")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Replication: Free Cash Flow / Price (fcf_me)", 
    subtitle = paste0("Corr: ", round(stats$Correlation, 2), " | Vol Ratio: ", round(stats$Vol_Ratio, 2)),
    y = "Cumulative Excess Return", x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)

# Save
dbWriteTable(tidy_finance, "factor_fcf_price", fcf_factor, overwrite = TRUE)
dbDisconnect(tidy_finance)
dbDisconnect(wrds)