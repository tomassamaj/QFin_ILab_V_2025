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
library(zoo)          # For rolling window calculations

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

# ------------------------------------------------------------------------------
# 1.5 Fama-French Momentum Factor (Benchmark)
# ------------------------------------------------------------------------------
message("Downloading Momentum Factor...")
factors_mom_monthly_raw <- download_french_data("Momentum Factor (Mom)")

factors_mom_monthly <- factors_mom_monthly_raw$subsets$data[[1]] |>
  mutate(
    date = floor_date(ymd(str_c(date, "01")), "month"),
    mom = as.numeric(Mom) / 100
  ) |>
  select(date, mom) |>
  filter(date >= start_date & date <= end_date)

dbWriteTable(tidy_finance, "factors_mom_monthly", factors_mom_monthly, overwrite = TRUE)


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
  user = Sys.getenv("WRDS_USER"),   # WRDS username from environment variable
  password = Sys.getenv("WRDS_PASSWORD") # WRDS password from environment variable
)



# ------------------------------------------------------------------------------
# 2.2 CRSP Monthly (Returns, Momentum, Age)
# ------------------------------------------------------------------------------
message("Processing CRSP Monthly Data...")

# 1. Fetch Monthly Returns (MSF)
# Note: We strictly query existing columns to be safe
msf_db <- tbl(wrds, I("crsp.msf_v2")) 
stk_info_db <- tbl(wrds, I("crsp.stksecurityinfohist")) 

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

# 2. Load Risk Free Rate
factors_ff3_mem <- tbl(tidy_finance, "factors_ff3_monthly") |> 
  select(date, rf) |> 
  collect() |> 
  mutate(date = ymd(date))

# 3. Calculate Factors (Age, Momentum, Reversal)
# We SKIP chcsho_12m because cfacshr is missing
crsp_monthly <- crsp_raw |>
  arrange(permno, date) |>
  group_by(permno) |>
  mutate(
    mktcap = shrout * prc / 10^6,
    mktcap = na_if(mktcap, 0),
    
    # FACTOR: Firm Age (Months since first appearance)
    age = row_number(),
    
    # FACTOR: Long Term Reversal (Ret_60_12)
    # Cumulative return from t-60 to t-13
    log_ret = log(1 + ret),
    # Roll sum 48 months, lag 13 months
    ret_60_12 = zoo::rollsum(log_ret, k = 48, fill = NA, align = "right"),
    ret_60_12 = lag(ret_60_12, 13),
    
    exchange = case_when(
      primaryexch == "N" ~ "NYSE",
      primaryexch == "A" ~ "AMEX",
      primaryexch == "Q" ~ "NASDAQ",
      .default = "Other"
    )
  ) |>
  ungroup()

# Lagged Market Cap
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

dbWriteTable(tidy_finance, "crsp_monthly", crsp_monthly, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 2.3 Compustat (Fundamentals & Advanced Factors)
# ------------------------------------------------------------------------------
message("Processing Compustat Data...")

# 1. Fetch Expanded Fundamental Data
compustat_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= start_date & datadate <= end_date
  ) |>
  select(
    gvkey, datadate, 
    # Core
    seq, ceq, pstk, at, lt, txditc, txdb, itcb, pstkrv, pstkl, 
    # Income Statement
    sale, cogs, xsga, xint, ib, ni, dp, oibdp, pi, txt, xi, do,
    # Balance Sheet
    act, lct, che, rect, invt, aco, ap, lco, dltt, dlc, lo, re,
    # Cash Flow
    oancf, ivncf, fincf, capx, sstk, prstkc, 
    # Price/Shares (Fiscal Year End)
    prcc_f, csho
  ) |>
  collect()

# 2. Filter 2-Year History
compustat_filtered <- compustat_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(firm_age = row_number()) |>
  ungroup() |>
  filter(firm_age > 2)

# 3. Calculate "Green Light" Factors
# Note: We removed dbnetis_at to avoid the previous error
compustat_processed <- compustat_filtered |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  mutate(
    year = year(datadate),
    
    # Lags
    at_lag1 = lag(at, 1),
    sale_lag1 = lag(sale, 1),
    rect_lag1 = lag(rect, 1),
    invt_lag1 = lag(invt, 1),
    
    # Book Equity
    be = coalesce(seq, ceq + pstk, at - lt) + 
      coalesce(txditc, txdb + itcb, 0) - 
      coalesce(pstkrv, pstkl, pstk, 0),
    be = if_else(be <= 0, NA, be),
    
    # 1. GROSS PROFITABILITY
    gp = sale - coalesce(cogs, 0),
    gp_at = gp / at,
    
    # 2. PIOTROSKI F-SCORE
    f_roa = if_else(ib > 0, 1, 0),
    f_cfo = if_else(oancf > 0, 1, 0),
    f_droa = if_else((ib/at_lag1) > (lag(ib)/lag(at, 2)), 1, 0),
    f_acc = if_else(oancf > ib, 1, 0), 
    lev_curr = (dltt + dlc) / at,
    lev_prev = (lag(dltt) + lag(dlc)) / at_lag1,
    f_lev = if_else(lev_curr < lev_prev, 1, 0),
    curr_ratio_curr = act / lct,
    curr_ratio_prev = lag(act) / lag(lct),
    f_liq = if_else(curr_ratio_curr > curr_ratio_prev, 1, 0),
    f_eqis = if_else(is.na(sstk) | sstk <= 0, 1, 0), 
    gm_curr = gp / sale,
    gm_prev = lag(gp) / lag(sale),
    f_gm = if_else(gm_curr > gm_prev, 1, 0),
    ato_curr = sale / at_lag1,
    ato_prev = lag(sale) / lag(at, 2),
    f_ato = if_else(ato_curr > ato_prev, 1, 0),
    f_score = f_roa + f_cfo + f_droa + f_acc + f_lev + f_liq + f_eqis + f_gm + f_ato,
    
    # 3. ACCRUALS
    avg_at = (at + at_lag1) / 2,
    oaccruals_at = (ib - oancf) / avg_at,
    
    # 4. NET OPERATING ASSETS
    oa = at - che,
    ol = at - (dltt + dlc) - be, 
    noa_at = (oa - ol) / at_lag1,
    
    # 5. ALTMAN Z & OHLSON O
    me_fiscal = prcc_f * csho,
    wc_at = (act - lct) / at,
    re_at = coalesce(re, 0) / at, 
    ebit_at = (ib + xint + txdb) / at, 
    me_tl = me_fiscal / lt,
    sale_at = sale / at,
    z_score = 1.2*wc_at + 1.4*re_at + 3.3*ebit_at + 0.6*me_tl + 1.0*sale_at,
    
    lat = log(at),
    tlta = lt / at,
    clca = lct / act,
    nita = ni / at,
    futl = (pi + dp) / lt,
    intwo = if_else(ni < 0 & lag(ni) < 0, 1, 0),
    chin = (ni - lag(ni)) / (abs(ni) + abs(lag(ni))),
    o_score = -1.32 - 0.407*lat + 6.03*tlta - 1.43*wc_at + 0.076*clca - 2.37*nita - 1.83*futl + 0.285*intwo - 0.52*chin,
    
    # 6. INVESTMENT & ISSUANCE
    inv = at / at_lag1 - 1,
    capx_gr1 = capx / lag(capx) - 1,
    sale_gr1 = sale / sale_lag1 - 1,
    inv_gr1  = invt / invt_lag1 - 1,
    netis_at = (sstk - prstkc) / at,
    
    # 7. LEVERAGE
    at_be = at / be,
    op = (sale - coalesce(cogs, 0) - coalesce(xsga, 0) - coalesce(xint, 0)) / be
    
  ) |>
  ungroup() |>
  # Select final columns (REMOVED dbnetis_at)
  select(gvkey, year, datadate, be, op, inv, 
         f_score, z_score, o_score, gp_at, oaccruals_at, noa_at, 
         capx_gr1, sale_gr1, inv_gr1, 
         netis_at, at_be) |>
  filter(datadate == max(datadate), .by = c(gvkey, year))

dbWriteTable(tidy_finance, "compustat", compustat_processed, overwrite = TRUE)
# ------------------------------------------------------------------------------
# 2.4 Linking CRSP and Compustat (CCM)
# ------------------------------------------------------------------------------
message("Linking CRSP and Compustat...")

ccm_link <- tbl(wrds, I("crsp.ccmxpf_lnkhist")) |>
  filter(linktype %in% c("LU", "LC") & linkprim %in% c("P", "C")) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, today()))

crsp_monthly_mem <- tbl(tidy_finance, "crsp_monthly") |>
  collect() |>
  mutate(date = ymd(date))

ccm_merged <- crsp_monthly_mem |>
  select(permno, date) |> # Optimize size
  inner_join(ccm_link, by = "permno", relationship = "many-to-many") |>
  filter(date >= linkdt & date <= linkenddt) |>
  select(permno, gvkey, date)

# Attach gvkey to main CRSP table
crsp_monthly_linked <- crsp_monthly_mem |>
  left_join(ccm_merged, by = c("permno", "date"))

dbWriteTable(tidy_finance, "crsp_monthly", crsp_monthly_linked, overwrite = TRUE)


# ==============================================================================
# PHASE 3: FACTOR REPLICATION (GREEN LIGHT FACTORS) - ROBUST VERSION
# ==============================================================================
message("Starting Factor Replication...")

# 3.1 Load Data
crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, gvkey, date, ret_excess, mktcap, mktcap_lag, exchange, ret) |>
  collect() |> mutate(date = ymd(date))

# Ensure gvkey exists
if(!"gvkey" %in% colnames(crsp_monthly)) stop("GVKEY missing in CRSP data. Run Phase 2.4!")

compustat <- tbl(tidy_finance, "compustat") |>
  # REMOVED dbnetis_at from selection
  select(gvkey, datadate, be, op, inv, 
         f_score, z_score, o_score, gp_at, oaccruals_at, noa_at, 
         capx_gr1, sale_gr1, inv_gr1, netis_at, at_be) |>
  collect() |> mutate(datadate = ymd(datadate))

# 3.2 Prepare Sorting Variables
# Recalculate price factors locally to be safe
crsp_factors <- crsp_monthly |>
  arrange(permno, date) |>
  group_by(permno) |>
  mutate(
    age = row_number(),
    log_ret = log(1 + ret),
    ret_60_12 = zoo::rollsum(log_ret, k = 48, fill = NA, align = "right"),
    ret_60_12 = lag(ret_60_12, 13)
  ) |>
  ungroup() |>
  select(permno, date, exchange, mktcap, age, ret_60_12)

size_vars <- crsp_factors |>
  filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |> 
  select(permno, exchange, sorting_date, size = mktcap, age, ret_60_12)

market_equity <- crsp_monthly |>
  filter(month(date) == 12) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |>
  select(permno, gvkey, sorting_date, me = mktcap)

accounting_vars <- compustat |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
  inner_join(market_equity, join_by(gvkey, sorting_date)) |>
  mutate(bm = be / me) |>
  select(permno, sorting_date, bm, op, inv,
         f_score, z_score, o_score, gp_at, oaccruals_at, noa_at, 
         capx_gr1, sale_gr1, inv_gr1, netis_at, at_be)

sorting_variables <- size_vars |>
  inner_join(accounting_vars, join_by(permno, sorting_date)) |>
  drop_na(size) |> 
  distinct(permno, sorting_date, .keep_all = TRUE)

# 3.3 Portfolio Construction (Robust Function)
# Handles missing data safely
assign_portfolio <- function(data, sorting_variable, percentiles) {
  nyse_vec <- data |> filter(exchange == "NYSE") |> pull({{ sorting_variable }})
  if (sum(!is.na(nyse_vec)) < 5) return(rep(NA_integer_, nrow(data)))
  
  breakpoints <- quantile(nyse_vec, probs = c(0, percentiles, 1), na.rm = TRUE, names = FALSE)
  if (any(is.na(breakpoints))) return(rep(NA_integer_, nrow(data)))
  
  sort_vec <- data |> pull({{ sorting_variable }})
  assigned <- findInterval(sort_vec, breakpoints, all.inside = TRUE)
  is.na(assigned) <- is.na(sort_vec)
  return(assigned)
}

# Adjusted Factor List (No dbnetis_at, No chcsho_12m)
factor_list <- c("bm", "op", "inv", 
                 "f_score", "z_score", "o_score", "gp_at", "oaccruals_at", "noa_at",
                 "capx_gr1", "sale_gr1", "inv_gr1", "netis_at", "at_be",
                 "ret_60_12", "age")

portfolios <- sorting_variables |>
  group_by(sorting_date) |>
  mutate(portfolio_size = assign_portfolio(pick(everything()), "size", c(0.5))) |>
  mutate(across(all_of(factor_list), 
                ~assign_portfolio(pick(everything()), ., c(0.3, 0.7)),
                .names = "portfolio_{.col}")) |>
  ungroup()

portfolios_long <- crsp_monthly |>
  mutate(sorting_date = case_when(
    month(date) <= 6 ~ ymd(str_c(year(date) - 1, "0701")),
    month(date) >= 7 ~ ymd(str_c(year(date), "0701"))
  )) |>
  inner_join(portfolios, join_by(permno, sorting_date))

# 3.4 Calculate Returns (Manual Loop to Avoid 'Reduce' Errors)
# This is slower but safer and easier to debug
results_list <- list()

for (f in factor_list) {
  port_col <- paste0("portfolio_", f)
  
  if (port_col %in% names(portfolios_long)) {
    # Check if we have valid data
    valid_rows <- portfolios_long |> filter(!is.na(.data[[port_col]]) & !is.na(portfolio_size))
    
    if (nrow(valid_rows) > 0) {
      temp <- valid_rows |>
        group_by(date, portfolio_size, .data[[port_col]]) |>
        summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
        group_by(date) |>
        summarize(!!paste0(f, "_replicated") := mean(ret[.data[[port_col]] == 3]) - mean(ret[.data[[port_col]] == 1]))
      
      results_list[[f]] <- temp
    }
  }
}

message("Merging Results...")
factors_replicated_green <- results_list |> reduce(full_join, by = "date")

dbWriteTable(tidy_finance, "factors_replicated_green", factors_replicated_green, overwrite = TRUE)
message("Green Light Factors Replicated Successfully.")

# ==============================================================================
# PHASE 3.5: MARKET & RF REPLICATION
# ==============================================================================
message("Replicating Market and RF...")

# ------------------------------------------------------------------------------
# 1. Replicate Risk-Free Rate (RF)
# ------------------------------------------------------------------------------
# Fama-French use the 1-month T-Bill return from Ibbotson Associates.
# A common public proxy is the 1-Month Treasury Constant Maturity Rate (GS1M) from FRED.
# Note: This is a rate (yield), so we divide by 1200 to approximate monthly return.

rf_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS1M"

rf_replicated <- request(rf_url) |>
  req_perform() |>
  resp_body_string() |>
  read_csv() |>
  mutate(
    date = floor_date(as.Date(observation_date), "month"),
    # Convert annualized percentage yield to monthly decimal return approximation
    rf_proxy = as.numeric(GS1M) / 12 / 100 
  ) |>
  select(date, rf_proxy)

# ------------------------------------------------------------------------------
# 2. Replicate Market Factor (Mkt-RF)
# ------------------------------------------------------------------------------
# The Market Factor is the value-weighted excess return of all CRSP firms 
# that meet the screening criteria (which you already filtered in Phase 2.2).

# We use the 'crsp_monthly' table loaded in Phase 3.1
mkt_replicated <- crsp_monthly |>
  group_by(date) |>
  summarize(
    # Weighted average of (Ret - RF) = Mkt - RF
    mkt_excess_replicated = weighted.mean(ret_excess, mktcap_lag, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 3. Merge into Main Factor Table
# ------------------------------------------------------------------------------
factors_replicated_full <- factors_replicated |>
  left_join(mkt_replicated, join_by(date)) |>
  left_join(rf_replicated, join_by(date))

# Save the fully replicated set
dbWriteTable(tidy_finance, "factors_replicated_full", factors_replicated_full, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 4. Validate Market Replication
# ------------------------------------------------------------------------------
# Compare your Mkt-RF against the official FF Mkt-RF
validation_mkt <- factors_ff5_monthly |>
  select(date, mkt_excess) |> # From Official FF5
  inner_join(factors_replicated_full, join_by(date))

print("--- Market Factor (Mkt-RF) Regression ---")
print(summary(lm(mkt_excess ~ mkt_excess_replicated, data = validation_mkt)))


# ==============================================================================
# PHASE 3.6: MOMENTUM FACTOR REPLICATION (Zoo Version)
# ==============================================================================
message("Replicating Momentum Factor...")

# 1. Load Data
crsp_mom_data <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, date, ret, mktcap, mktcap_lag, exchange) |>
  collect() |>
  mutate(date = ymd(date))

# 2. Calculate Momentum Signal (12-1 Month Return) using ZOO
# Logic: 
#   1. Calculate 11-month cumulative return (compounded) ending at month t.
#   2. Lag this result by 2 months. 
#   Result: At month t, the signal represents returns from t-12 to t-2.

crsp_mom_signal <- crsp_mom_data |>
  arrange(permno, date) |>
  group_by(permno) |>
  mutate(
    # Helper: Gross Return = 1 + r
    gross_ret = 1 + ret,
    
    # Calculate 11-month rolling product (right-aligned)
    # width = 11 means looking at current + 10 previous months
    roll_ret_11m = zoo::rollapplyr(gross_ret, width = 11, FUN = prod, fill = NA) - 1,
    
    # Lag by 2 months to create the "Skip Month" (t-1)
    mom_signal = lag(roll_ret_11m, 2)
  ) |>
  ungroup() |>
  filter(!is.na(mom_signal) & !is.na(mktcap_lag)) |>
  select(permno, date, exchange, mom_signal, mktcap_lag, ret_excess = ret)

# 3. Portfolio Sorting (2x3)
# Function to assign portfolios based on Monthly Breakpoints
assign_mom_portfolio <- function(data) {
  # NYSE-only Breakpoints
  nyse_subset <- data |> filter(exchange == "NYSE")
  
  # Size Breakpoint (Median)
  size_bp <- quantile(nyse_subset$mktcap_lag, probs = 0.5, na.rm = TRUE)
  
  # Momentum Breakpoints (30th and 70th percentiles)
  mom_bp <- quantile(nyse_subset$mom_signal, probs = c(0.3, 0.7), na.rm = TRUE)
  
  data |>
    mutate(
      # Size Sort
      portfolio_size = if_else(mktcap_lag <= size_bp, "Small", "Big"),
      
      # Momentum Sort
      portfolio_mom = case_when(
        mom_signal <= mom_bp[1] ~ "Low",    # Losers
        mom_signal > mom_bp[2] ~ "High",    # Winners
        TRUE ~ "Neutral"
      )
    )
}

# Apply sorting per month
# Note: map_dfr is from purrr (part of tidyverse)
mom_portfolios <- crsp_mom_signal |>
  group_by(date) |>
  group_split() |>
  map_dfr(assign_mom_portfolio)

# 4. Construct Factor (Winners - Losers)
mom_replicated <- mom_portfolios |>
  group_by(date, portfolio_size, portfolio_mom) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  group_by(date) |>
  summarize(
    # (Small High + Big High)/2 - (Small Low + Big Low)/2
    mom_replicated = mean(ret[portfolio_mom == "High"]) - mean(ret[portfolio_mom == "Low"])
  )

# Save to DB
dbWriteTable(tidy_finance, "mom_replicated", mom_replicated, overwrite = TRUE)

# 5. Validation
# Ensure you have run Phase 1.5 (Benchmark Download) from the previous step before this
if(dbExistsTable(tidy_finance, "factors_mom_monthly")) {
  validation_mom <- tbl(tidy_finance, "factors_mom_monthly") |>
    collect() |>
    mutate(date = ymd(date)) |>
    inner_join(mom_replicated, join_by(date))
  
  print("--- Momentum Factor (MOM) Regression ---")
  print(summary(lm(mom ~ mom_replicated, data = validation_mom)))
} else {
  message("Official Momentum Factor data not found in DB. Skipping validation regression.")
}

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





# ==============================================================================
# PHASE 4.2: EVALUATION OF GREEN LIGHT FACTORS
# ==============================================================================
message("Evaluating Green Light Factors...")

# 1. Load Data
factors_green <- tbl(tidy_finance, "factors_replicated_green") |> collect()
factors_ff5   <- tbl(tidy_finance, "factors_ff5_monthly") |> select(date, mkt_excess, smb, hml, rmw, cma, rf) |> collect()

# Merge
analysis_data <- factors_green |>
  inner_join(factors_ff5, by = "date") |>
  mutate(date = ymd(date)) |>
  drop_na()

# ------------------------------------------------------------------------------
# 2. Summary Statistics (Annualized)
# ------------------------------------------------------------------------------
# We calculate Mean Return, Volatility, and Sharpe Ratio for each factor
factor_stats <- analysis_data |>
  select(ends_with("_replicated")) |>
  pivot_longer(everything(), names_to = "factor", values_to = "ret") |>
  group_by(factor) |>
  summarize(
    Mean_Ann = mean(ret) * 12,
    Vol_Ann = sd(ret) * sqrt(12),
    Sharpe = Mean_Ann / Vol_Ann,
    t_stat = (mean(ret) / (sd(ret) / sqrt(n()))),
    .groups = "drop"
  ) |>
  arrange(desc(Sharpe))

print("--- Factor Performance (Ranked by Sharpe Ratio) ---")
print(factor_stats, n = 20)

# ------------------------------------------------------------------------------
# 3. Correlation with Fama-French 5 Factors
# ------------------------------------------------------------------------------
# Are these factors just repackaged Value (HML) or Profitability (RMW)?
cor_matrix <- analysis_data |>
  select(ends_with("_replicated"), mkt_excess, smb, hml, rmw, cma) |>
  cor(use = "pairwise.complete.obs")

# Filter to show only how NEW factors correlate with OLD factors
ff_correlations <- cor_matrix[grep("_replicated", rownames(cor_matrix)), 
                              c("mkt_excess", "smb", "hml", "rmw", "cma")]

print("--- Correlation with Official FF5 Factors ---")
print(round(ff_correlations, 2))

# ------------------------------------------------------------------------------
# 4. Alpha Analysis (CAPM & FF5 Alphas)
# ------------------------------------------------------------------------------
# Do they generate alpha controlling for standard factors?
run_alphas <- function(data, factor_name) {
  # CAPM Alpha
  model_capm <- lm(data[[factor_name]] ~ mkt_excess, data = data)
  alpha_capm <- coef(model_capm)[1] * 12 # Annualized
  
  # FF5 Alpha
  model_ff5 <- lm(data[[factor_name]] ~ mkt_excess + smb + hml + rmw + cma, data = data)
  alpha_ff5 <- coef(model_ff5)[1] * 12 # Annualized
  
  tibble(Factor = factor_name, Alpha_CAPM = alpha_capm, Alpha_FF5 = alpha_ff5)
}

# Run for all new factors
alpha_summary <- names(analysis_data) |>
  keep(~str_detect(., "_replicated")) |>
  map_dfr(~run_alphas(analysis_data, .)) |>
  arrange(desc(Alpha_FF5))

print("--- Factor Alphas (Annualized) ---")
print(alpha_summary, n = 20)

# ------------------------------------------------------------------------------
# 5. Visualization: Cumulative Returns
# ------------------------------------------------------------------------------
# Plot the top 3 performing factors vs Market
top_3_factors <- factor_stats$factor

plot_data <- analysis_data |>
  select(date, mkt_excess, all_of(top_3_factors)) |>
  pivot_longer(-date, names_to = "Factor", values_to = "Return") |>
  group_by(Factor) |>
  mutate(Cumulative_Return = cumprod(1 + Return) - 1)

ggplot(plot_data, aes(x = date, y = Cumulative_Return, color = Factor)) +
  geom_line(linewidth = 1) +
  labs(title = "Cumulative Returns: Top 3 Replicated Factors vs Market",
       y = "Cumulative Return", x = "Date") +
  theme_minimal() +
  scale_y_continuous(labels = percent)




# ==============================================================================
# PHASE 4: EVALUATION & SANITY CHECKS
# ==============================================================================
message("Evaluating Green Light Factors...")

# 1. Load Data
factors_green <- tbl(tidy_finance, "factors_replicated_green") |> collect()
factors_ff5   <- tbl(tidy_finance, "factors_ff5_monthly") |> 
  select(date, mkt_excess, smb, hml, rmw, cma, rf) |> 
  collect()

# Merge
analysis_data <- factors_green |>
  inner_join(factors_ff5, by = "date") |>
  mutate(date = ymd(date)) |>
  drop_na()

# ------------------------------------------------------------------------------
# 2. Performance Statistics (The "Sanity Check")
# ------------------------------------------------------------------------------
# We calculate the Mean Annualized Return for each factor.
# Crucial: Check if the SIGN matches the "Expected Sign" below.

factor_stats <- analysis_data |>
  select(ends_with("_replicated")) |>
  pivot_longer(everything(), names_to = "factor", values_to = "ret") |>
  group_by(factor) |>
  summarize(
    Mean_Ann = mean(ret) * 12,
    Vol_Ann = sd(ret) * sqrt(12),
    Sharpe = Mean_Ann / Vol_Ann,
    t_stat = (mean(ret) / (sd(ret) / sqrt(n()))),
    .groups = "drop"
  ) |>
  # Add "Expected Sign" based on Academic Literature
  mutate(
    Short_Name = str_remove(factor, "_replicated"),
    Expected_Sign = case_when(
      Short_Name %in% c("f_score", "gp_at", "op", "z_score", "o_score") ~ "Positive (+)",
      Short_Name %in% c("inv", "capx_gr1", "sale_gr1", "netis_at", "oaccruals_at", "noa_at", "ret_60_12") ~ "Negative (-)",
      TRUE ~ "?"
    ),
    Check_Passed = case_when(
      Expected_Sign == "Positive (+)" & t_stat > 1.5 ~ "OK",
      Expected_Sign == "Negative (-)" & t_stat < -1.5 ~ "OK",
      TRUE ~ "Weak/Fail"
    )
  ) |>
  select(Short_Name, Expected_Sign, Mean_Ann, t_stat, Check_Passed) |>
  arrange(desc(Expected_Sign), desc(t_stat))

print("--- Sanity Check: Do signs match theory? ---")
print(factor_stats, n = 20)

# ------------------------------------------------------------------------------
# 3. Correlation Matrix
# ------------------------------------------------------------------------------
# Check if your new factors are just duplicates of FF5
# e.g., 'gp_at' should be highly correlated with 'rmw' (Profitability)

cor_matrix <- analysis_data |>
  select(ends_with("_replicated"), mkt_excess, smb, hml, rmw, cma) |>
  cor(use = "pairwise.complete.obs")

# Extract correlations with Official FF5
ff_correlations <- cor_matrix[grep("_replicated", rownames(cor_matrix)), 
                              c("mkt_excess", "smb", "hml", "rmw", "cma")]

print("--- Correlation with Official Factors ---")
print(round(ff_correlations, 2))

# ------------------------------------------------------------------------------
# 4. Alpha Analysis (Does it add value?)
# ------------------------------------------------------------------------------
# We check if these factors generate alpha relative to the standard FF5 model
run_alphas <- function(data, factor_name) {
  model <- lm(data[[factor_name]] ~ mkt_excess + smb + hml + rmw + cma, data = data)
  
  tibble(
    Factor = str_remove(factor_name, "_replicated"),
    Alpha_Ann = coef(model)[1] * 12,
    t_stat = summary(model)$coefficients[1, "t value"]
  )
}

alpha_summary <- names(analysis_data) |>
  keep(~str_detect(., "_replicated")) |>
  map_dfr(~run_alphas(analysis_data, .)) |>
  arrange(desc(abs(t_stat)))

print("--- FF5 Alphas (Unexplained Return) ---")
print(alpha_summary, n = 20)






# ==============================================================================
# MASTER REPLICATION VALIDATION SCRIPT (FIXED)
# ==============================================================================

# 1. SETUP & LIBRARIES
library(tidyverse)
library(RSQLite)
library(DBI)
library(scales)
library(gt)
library(lubridate)

# --- Configuration ---
DB_PATH <- "data/tidy_finance_r.sqlite" 
JKP_FILE <- "[usa]_[all_factors]_[monthly]_[vw_cap].csv" 
START_DATE <- ymd("1963-07-01")
END_DATE   <- ymd("2023-12-31")

# ==============================================================================
# 2. LOAD & INSPECT DATA
# ==============================================================================

# --- A. Load User's Replicated Factors ---
message("Loading User Replication Data...")
con <- dbConnect(SQLite(), DB_PATH, extended_types = TRUE)

if(dbExistsTable(con, "factors_replicated_green")) {
  user_factors_raw <- tbl(con, "factors_replicated_green") |> collect()
} else {
  stop("Table 'factors_replicated_green' not found in database! Run Phase 3 first.")
}
dbDisconnect(con)

# --- B. Load Official JKP Factors ---
message("Loading Official JKP Benchmark...")
if(!file.exists(JKP_FILE)) stop("JKP CSV file not found.")

jkp_factors_raw <- read.csv(JKP_FILE)

# ==============================================================================
# 3. CRITICAL FIX: DATE ALIGNMENT
# ==============================================================================
message("Aligning Dates (Forcing End-of-Month)...")

# 1. Prepare User Data
user_factors <- user_factors_raw |>
  # Ensure date is Date object
  mutate(date = as.Date(date)) |>
  # FORCE to End-of-Month (e.g., 1963-07-01 -> 1963-07-31)
  mutate(date = ceiling_date(date, "month") - days(1)) |>
  filter(date >= START_DATE & date <= END_DATE)

# 2. Prepare JKP Data
jkp_factors <- jkp_factors_raw |>
  as_tibble() |>
  select(date, name, ret) |>
  mutate(date = as.Date(date)) |>
  # FORCE to End-of-Month
  mutate(date = ceiling_date(date, "month") - days(1)) |>
  filter(date >= START_DATE & date <= END_DATE) |>
  pivot_wider(names_from = name, values_from = ret)

# 3. DEBUG: Check Date Overlap
user_dates <- range(user_factors$date)
jkp_dates  <- range(jkp_factors$date)

cat(sprintf("User Date Range: %s to %s\n", user_dates[1], user_dates[2]))
cat(sprintf("JKP  Date Range: %s to %s\n", jkp_dates[1], jkp_dates[2]))

common_dates <- intersect(user_factors$date, jkp_factors$date)
cat(sprintf("Number of Overlapping Months: %d\n", length(common_dates)))

if(length(common_dates) < 12) stop("CRITICAL: Less than 12 overlapping months found. Check your date filters!")

# ==============================================================================
# 4. PERFORM VALIDATION CHECKS (ROBUST LOOP)
# ==============================================================================

# Define Map
mapping <- tribble(
  ~User_Factor,      ~JKP_Factor,      ~Description,
  "bm_replicated",   "be_me",          "Book-to-Market (Value)",
  "op_replicated",   "ope_be",         "Operating Profitability",
  "inv_replicated",  "at_gr1",         "Investment (Asset Growth)",
  "gp_at_replicated","gp_at",          "Gross Profitability",
  "f_score_replicated","f_score",      "Piotroski F-Score",
  "z_score_replicated","z_score",      "Altman Z-Score",
  "o_score_replicated","o_score",      "Ohlson O-Score",
  "noa_at_replicated", "noa_at",       "Net Operating Assets",
  "oaccruals_at_replicated", "oaccruals_at", "Accruals",
  "netis_at_replicated", "netis_at",   "Net Equity Issuance",
  "at_be_replicated", "at_be",         "Leverage",
  "sale_gr1_replicated", "sale_gr1",   "Sales Growth",
  "inv_gr1_replicated", "inv_gr1",     "Inventory Growth",
  "ret_60_12_replicated", "ret_60_12", "Long Term Reversal"
)

message("Running Validation Logic...")
results_store <- list()

for(i in 1:nrow(mapping)) {
  u_name <- mapping$User_Factor[i]
  j_name <- mapping$JKP_Factor[i]
  
  # Check if columns exist
  if(u_name %in% names(user_factors) && j_name %in% names(jkp_factors)) {
    
    # Merge specific pair
    temp_df <- inner_join(
      user_factors |> select(date, user_ret = all_of(u_name)),
      jkp_factors  |> select(date, jkp_ret = all_of(j_name)),
      by = "date"
    ) |> drop_na()
    
    # SAFETY CHECK: Do we have enough data points?
    if(nrow(temp_df) > 12) {
      # 1. Correlation
      cor_val <- cor(temp_df$user_ret, temp_df$jkp_ret)
      
      # 2. Regression (User ~ JKP)
      model <- lm(user_ret ~ jkp_ret, data = temp_df)
      beta  <- coef(model)[2]
      alpha <- coef(model)[1]
      
      # 3. Scale Check
      mean_diff <- mean(temp_df$user_ret) - mean(temp_df$jkp_ret)
      
      # Store
      results_store[[u_name]] <- tibble(
        Factor_User = u_name,
        Factor_JKP = j_name,
        N_Months = nrow(temp_df),
        Correlation = cor_val,
        Beta = beta,
        Alpha_Monthly = alpha,
        Scale_Diff = mean_diff
      )
    } else {
      warning(paste("Skipping", u_name, "- Not enough overlapping data points."))
    }
  } else {
    # Print missing columns for debugging
    if(!u_name %in% names(user_factors)) warning(paste("Missing in USER data:", u_name))
    if(!j_name %in% names(jkp_factors))  warning(paste("Missing in JKP  data:", j_name))
  }
}

# Bind Results
if(length(results_store) > 0) {
  validation_report <- bind_rows(results_store)
  
  # ==============================================================================
  # 5. GENERATE REPORT
  # ==============================================================================
  CORR_THRESHOLD_PASS <- 0.90
  CORR_THRESHOLD_WARN <- 0.75
  
  final_report <- validation_report |> 
    mutate(
      Status = case_when(
        Correlation >= CORR_THRESHOLD_PASS ~ "PASS (Green)",
        Correlation >= CORR_THRESHOLD_WARN ~ "WARNING (Yellow)",
        Correlation < 0 ~ "FAIL (Inverted Sign!)",
        TRUE ~ "FAIL (Low Correlation)"
      )
    ) |> 
    arrange(desc(Correlation))
  
  print(as.data.frame(final_report)) # Use base dataframe print for full visibility
  
  # ==============================================================================
  # 6. VISUALIZATION (Top 4)
  # ==============================================================================
  # Safely pick top factors
  if(nrow(final_report) >= 2) {
    factors_to_plot <- head(final_report, 4)
    
    plot_data <- list()
    for(i in 1:nrow(factors_to_plot)) {
      u_name <- factors_to_plot$Factor_User[i]
      j_name <- factors_to_plot$Factor_JKP[i]
      
      df <- inner_join(
        user_factors |> select(date, user_ret = all_of(u_name)),
        jkp_factors  |> select(date, jkp_ret = all_of(j_name)),
        by = "date"
      ) |> 
        mutate(
          User_Cum = cumprod(1 + user_ret),
          JKP_Cum  = cumprod(1 + jkp_ret),
          Factor   = u_name
        )
      plot_data[[i]] <- df
    }
    
    plot_df <- bind_rows(plot_data) |> 
      pivot_longer(cols = c(User_Cum, JKP_Cum), names_to = "Source", values_to = "Wealth")
    
    p <- ggplot(plot_df, aes(x = date, y = Wealth, color = Source, linetype = Source)) +
      geom_line(size = 0.8) +
      facet_wrap(~Factor, scales = "free_y") +
      scale_y_log10() +
      scale_color_manual(values = c("JKP_Cum" = "black", "User_Cum" = "#2ecc71")) +
      labs(title = "Replication Sanity Check: Cumulative Returns (Top Matches)",
           y = "Wealth Index (Log Scale)") +
      theme_minimal()
    
    print(p)
  }
  
} else {
  stop("No factors were successfully validated. Check 'Missing' warnings above.")
}
