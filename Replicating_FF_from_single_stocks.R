# ==============================================================================
# MASTER SCRIPT: Fama-French 5-Factor Replication from Single Stock Data
# ==============================================================================
# Description:
#   1. Downloads public benchmark data (FF, Macro, CPI).
#   2. Queries WRDS for proprietary stock (CRSP) and fundamental (Compustat) data.
#   3. Processes and links datasets (CCM).
#   4. Replicates SMB, HML, RMW, and CMA factors using NYSE breakpoints.
#   5. Validates replication via regression against official factors.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Environment Setup
# ------------------------------------------------------------------------------
library(tidyverse)    # Data manipulation
library(tidyfinance)  # Financial data utilities
library(scales)       # Formatting helper
library(RSQLite)      # Local database engine
library(frenchdata)   # Ken French Data Library API
library(httr2)        # HTTP requests (for FRED/CPI)
library(dbplyr)       # Database backend for dplyr
library(RPostgres)    # WRDS connection
library(lubridate)    # Date manipulation

# Define analysis window
start_date <- ymd("1960-01-01")
end_date   <- ymd("2024-12-31")

# Ensure local data directory exists
if (!dir.exists("data")) dir.create("data")

# Initialize local SQLite connection
# extended_types = TRUE ensures Dates are stored correctly, not as doubles
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

# ==============================================================================
# PHASE 1: PUBLIC DATA DOWNLOADS
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Fama-French 3 Factors (Benchmark)
# ------------------------------------------------------------------------------
message("Downloading FF3 Factors...")
# Fetch raw data from Ken French Library
factors_ff3_monthly_raw <- download_french_data("Fama/French 3 Factors")

# Extract monthly data, normalize dates to first of month, scale returns to decimal
factors_ff3_monthly <- factors_ff3_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(date >= start_date & date <= end_date)

dbWriteTable(tidy_finance, "factors_ff3_monthly", factors_ff3_monthly, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 1.2 Fama-French 5 Factors (Benchmark for Validation)
# ------------------------------------------------------------------------------
message("Downloading FF5 Factors...")
factors_ff5_monthly_raw <- download_french_data("Fama/French 5 Factors (2x3)")

factors_ff5_monthly <- factors_ff5_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(date >= start_date & date <= end_date)

dbWriteTable(tidy_finance, "factors_ff5_monthly", factors_ff5_monthly, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 1.3 Macroeconomic Predictors (Goyal/Welch)
# ------------------------------------------------------------------------------
message("Downloading Macro Predictors...")
# Direct CSV fetch from Goyal's Google Sheet
sheet_id <- "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
sheet_name <- "Monthly"
macro_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/gviz/tq?tqx=out:csv&sheet=", sheet_name)

macro_predictors <- read_csv(macro_url) |>
  mutate(date = ym(yyyymm)) |>
  mutate(across(where(is.character), as.numeric)) |>
  mutate(
    # Construct financial ratios and derived metrics
    IndexDiv = Index + D12,
    logret   = log(IndexDiv) - log(lag(IndexDiv)),
    Rfree    = log(Rfree + 1),
    rp_div   = lead(logret - Rfree, 1), 
    dp       = log(D12) - log(Index), 
    dy       = log(D12) - log(lag(Index)), 
    ep       = log(E12) - log(Index), 
    de       = log(D12) - log(E12), 
    tms      = lty - tbl, 
    dfy      = BAA - AAA 
  ) |>
  select(date, rp_div, dp, dy, ep, de, svar, bm = `b/m`, ntis, tbl, lty, ltr, tms, dfy, infl) |>
  filter(date >= start_date & date <= end_date) |>
  drop_na()

dbWriteTable(tidy_finance, "macro_predictors", macro_predictors, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 1.4 CPI Data (FRED)
# ------------------------------------------------------------------------------
message("Downloading CPI Data...")
series <- "CPIAUCNS" # CPI All Urban Consumers
cpi_url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series)

# Use httr2 for robust HTTP request
cpi_monthly <- request(cpi_url) |>
  req_perform() |>
  resp_body_string() |>
  read_csv() |>
  mutate(
    date = floor_date(as.Date(observation_date), "month"),
    value = as.numeric(.data[[series]]),
    # Normalize CPI to base 1 at the end of the sample
    cpi = value / value[date == max(date)],
    .keep = "none"
  )

dbWriteTable(tidy_finance, "cpi_monthly", cpi_monthly, overwrite = TRUE)


# ==============================================================================
# PHASE 2: PROPRIETARY DATA (WRDS)
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 WRDS Connection
# ------------------------------------------------------------------------------
message("Connecting to WRDS...")
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD")
)

# ------------------------------------------------------------------------------
# 2.2 CRSP Monthly (Returns) - CORRECTED
# ------------------------------------------------------------------------------
message("Processing CRSP Monthly Data...")

# Lazy query definitions
msf_db <- tbl(wrds, I("crsp.msf_v2")) 
stk_info_db <- tbl(wrds, I("crsp.stksecurityinfohist")) 

# Join MSF with Security Info to filter valid common stocks
crsp_raw <- msf_db |>
  filter(mthcaldt >= start_date & mthcaldt <= end_date) |>
  select(-c(siccd, primaryexch, conditionaltype, tradingstatusflg)) |> 
  inner_join(
    stk_info_db |>
      filter(
        sharetype == "NS" &                   
          securitytype == "EQTY" &              
          securitysubtype == "COM" &            
          usincflg == "Y" &                     
          issuertype %in% c("ACOR", "CORP") &   
          primaryexch %in% c("N", "A", "Q") &   
          conditionaltype %in% c("RW", "NW") &  
          tradingstatusflg == "A"               
      ) |> 
      select(permno, secinfostartdt, secinfoenddt, primaryexch, siccd),
    join_by(permno)
  ) |> 
  filter(mthcaldt >= secinfostartdt & mthcaldt <= secinfoenddt) |>
  mutate(date = floor_date(mthcaldt, "month")) |>
  select(permno, date, ret = mthret, shrout, prc = mthprc, primaryexch, siccd) |>
  collect() |> 
  mutate(date = ymd(date), shrout = shrout * 1000)

# Load Risk Free Rate from DB
factors_ff3_mem <- tbl(tidy_finance, "factors_ff3_monthly") |> 
  select(date, rf) |> 
  collect() |> 
  mutate(date = ymd(date))

# STEP 1: Calculate Market Cap and clean Exchange
crsp_monthly <- crsp_raw |>
  mutate(
    mktcap = shrout * prc / 10^6,
    mktcap = na_if(mktcap, 0),
    exchange = case_when(
      primaryexch == "N" ~ "NYSE",
      primaryexch == "A" ~ "AMEX",
      primaryexch == "Q" ~ "NASDAQ",
      .default = "Other"
    )
  )

# STEP 2: Now calculate Lagged Market Cap (using the updated object)
crsp_monthly <- crsp_monthly |>
  left_join(
    crsp_monthly |> 
      mutate(date = date %m+% months(1)) |> 
      select(permno, date, mktcap_lag = mktcap),
    join_by(permno, date)
  ) |>
  left_join(factors_ff3_mem, join_by(date)) |>
  mutate(ret_excess = ret - rf, ret_excess = pmax(ret_excess, -1)) |> 
  select(-rf) |>
  drop_na(ret_excess, mktcap, mktcap_lag)

# Save to DB
dbWriteTable(tidy_finance, "crsp_monthly", crsp_monthly, overwrite = TRUE)
# ------------------------------------------------------------------------------
# 2.3 Compustat (Fundamentals)
# ------------------------------------------------------------------------------
message("Processing Compustat Data...")

# Fetch Fundamental Annual Data
compustat_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= start_date & datadate <= end_date
  ) |>
  select(gvkey, datadate, seq, ceq, at, lt, txditc, txdb, itcb, pstkrv, pstkl, pstk, capx, oancf, sale, cogs, xint, xsga) |>
  collect()

# Calculate Derived Accounting Variables
# Step 1: BE and OP, and define 'year'
compustat_processed <- compustat_raw |>
  mutate(
    # Book Equity (BE) = SHE + Deferred Taxes - Preferred Stock
    be = coalesce(seq, ceq + pstk, at - lt) + 
      coalesce(txditc, txdb + itcb, 0) - 
      coalesce(pstkrv, pstkl, pstk, 0),
    be = if_else(be <= 0, NA, be),
    # Operating Profitability (OP) = Revenues - COGS - SGA - Interest / Book Equity
    op = (sale - coalesce(cogs, 0) - coalesce(xsga, 0) - coalesce(xint, 0)) / be,
    year = year(datadate)
  ) |>
  group_by(gvkey, year) |>
  filter(datadate == max(datadate)) |> # Handle duplicate fiscal year-ends
  ungroup()

# Step 2: Investment (INV) = Asset Growth
# Requires self-join to get t-1 assets
compustat_final <- compustat_processed |>
  left_join(
    compustat_processed |> select(gvkey, year, at_lag = at) |> mutate(year = year + 1),
    join_by(gvkey, year)
  ) |>
  mutate(inv = at / at_lag - 1, inv = if_else(at_lag <= 0, NA, inv))

dbWriteTable(tidy_finance, "compustat", compustat_final, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 2.4 Linking CRSP and Compustat (CCM)
# ------------------------------------------------------------------------------
message("Linking CRSP and Compustat...")

ccm_link <- tbl(wrds, I("crsp.ccmxpf_lnkhist")) |>
  filter(linktype %in% c("LU", "LC") & linkprim %in% c("P", "C")) |> # Link Primary/Composite, Link User/System
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, today()))

# Link gvkey to permno based on date ranges
ccm_merged <- crsp_monthly |>
  inner_join(ccm_link, join_by(permno), relationship = "many-to-many") |>
  filter(!is.na(gvkey) & (date >= linkdt & date <= linkenddt)) |>
  select(permno, gvkey, date)

# Attach gvkey to main CRSP table
crsp_monthly_linked <- crsp_monthly |>
  left_join(ccm_merged, join_by(permno, date))

dbWriteTable(tidy_finance, "crsp_monthly", crsp_monthly_linked, overwrite = TRUE)

# Disconnect WRDS (Processing Complete)
dbDisconnect(wrds)


# ==============================================================================
# PHASE 3: FACTOR REPLICATION
# ==============================================================================
message("Starting Factor Replication...")

# ------------------------------------------------------------------------------
# 3.1 Load Local Data
# ------------------------------------------------------------------------------
crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, gvkey, date, ret_excess, mktcap, mktcap_lag, exchange) |>
  collect() |> mutate(date = ymd(date))

compustat <- tbl(tidy_finance, "compustat") |>
  select(gvkey, datadate, be, op, inv) |>
  collect() |> mutate(datadate = ymd(datadate))

# ------------------------------------------------------------------------------
# 3.2 Prepare Sorting Variables (Timeline Alignment)
# ------------------------------------------------------------------------------
# Fama-French Logic: 
# Returns from July(t) to June(t+1) are matched with:
# - Size from June(t)
# - Accounting data from fiscal year ending in (t-1)

# 1. Size Variable (Market Cap at June t)
size <- crsp_monthly |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |> # Shift to July for joining
  select(permno, exchange, sorting_date, size = mktcap)

# 2. Market Equity (Market Cap at Dec t-1 for B/M denominator)
market_equity <- crsp_monthly |>
  filter(month(date) == 12) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |> # Shift to next July
  select(permno, gvkey, sorting_date, me = mktcap)

# 3. Accounting Variables (Fiscal Year t-1 applied to July t)
other_sorting_variables <- compustat |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
  select(gvkey, sorting_date, be, op, inv) |>
  inner_join(market_equity, join_by(gvkey, sorting_date)) |>
  mutate(bm = be / me) |> # Book-to-Market
  select(permno, sorting_date, me, be, bm, op, inv)

# 4. Master Sorting Table
sorting_variables <- size |>
  inner_join(other_sorting_variables, join_by(permno, sorting_date)) |>
  drop_na() |>
  distinct(permno, sorting_date, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 3.3 Portfolio Construction (2x3 Sorts)
# ------------------------------------------------------------------------------
# Logic: Calculate NYSE breakpoints, then assign ALL stocks to bins based on those points.

assign_portfolio <- function(data, sorting_variable, percentiles) {
  breakpoints <- data |>
    filter(exchange == "NYSE") |> # NYSE-only breakpoints
    pull({{ sorting_variable }}) |>
    quantile(probs = c(0, percentiles, 1), na.rm = TRUE, names = FALSE)
  
  findInterval(data |> pull({{ sorting_variable }}), breakpoints, all.inside = TRUE)
}

# Assign Portfolios
portfolios <- sorting_variables |>
  group_by(sorting_date) |>
  mutate(
    portfolio_size = assign_portfolio(pick(everything()), "size", c(0.5)),
  ) |>
  group_by(sorting_date, portfolio_size) |>
  mutate(
    portfolio_bm  = assign_portfolio(pick(everything()), "bm", c(0.3, 0.7)),
    portfolio_op  = assign_portfolio(pick(everything()), "op", c(0.3, 0.7)),
    portfolio_inv = assign_portfolio(pick(everything()), "inv", c(0.3, 0.7))
  ) |>
  ungroup() |>
  select(permno, sorting_date, portfolio_size, portfolio_bm, portfolio_op, portfolio_inv)

# Map annual portfolios to monthly returns (July t -> June t+1)
portfolios_long <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios, join_by(permno, sorting_date))

# ------------------------------------------------------------------------------
# 3.4 Factor Calculation (Value-Weighted)
# ------------------------------------------------------------------------------
# HML (Value): High B/M - Low B/M
factors_value <- portfolios_long |>
  group_by(portfolio_size, portfolio_bm, date) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  group_by(date) |>
  summarize(hml_replicated = mean(ret[portfolio_bm == 3]) - mean(ret[portfolio_bm == 1]))

# RMW (Profitability): Robust OP - Weak OP
factors_prof <- portfolios_long |>
  group_by(portfolio_size, portfolio_op, date) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  group_by(date) |>
  summarize(rmw_replicated = mean(ret[portfolio_op == 3]) - mean(ret[portfolio_op == 1]))

# CMA (Investment): Conservative (Low) Inv - Aggressive (High) Inv
factors_inv <- portfolios_long |>
  group_by(portfolio_size, portfolio_inv, date) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  group_by(date) |>
  summarize(cma_replicated = mean(ret[portfolio_inv == 1]) - mean(ret[portfolio_inv == 3])) 

# SMB (Size): Small - Big (Average across all 3 characteristic splits)
factors_size <- bind_rows(
  portfolios_long |> group_by(portfolio_size, portfolio_bm, date) |> summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop"),
  portfolios_long |> group_by(portfolio_size, portfolio_op, date) |> summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop"),
  portfolios_long |> group_by(portfolio_size, portfolio_inv, date) |> summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop")
) |>
  group_by(date) |>
  summarize(smb_replicated = mean(ret[portfolio_size == 1]) - mean(ret[portfolio_size == 2]))

# Merge Factors
factors_replicated <- factors_size |>
  full_join(factors_value, join_by(date)) |>
  full_join(factors_prof, join_by(date)) |>
  full_join(factors_inv, join_by(date))

# Save Proprietary Factors
dbWriteTable(tidy_finance, "factors_replicated", factors_replicated, overwrite = TRUE)

# ==============================================================================
# PHASE 4: EVALUATION
# ==============================================================================
message("Evaluating Replication...")

# Load Official FF5 for comparison
factors_ff5_monthly <- tbl(tidy_finance, "factors_ff5_monthly") |>
  select(date, smb, hml, rmw, cma) |>
  collect() |> mutate(date = ymd(date))

test_data <- factors_ff5_monthly |>
  inner_join(factors_replicated, join_by(date))

# Regressions (Target: Alpha ~ 0, Beta ~ 1, R2 > 0.9)
print("--- SMB Regression ---")
print(summary(lm(smb ~ smb_replicated, data = test_data)))

print("--- HML Regression ---")
print(summary(lm(hml ~ hml_replicated, data = test_data)))

print("--- RMW Regression ---")
print(summary(lm(rmw ~ rmw_replicated, data = test_data)))

print("--- CMA Regression ---")
print(summary(lm(cma ~ cma_replicated, data = test_data)))

# Cleanup
dbDisconnect(tidy_finance)
message("Script Complete.")