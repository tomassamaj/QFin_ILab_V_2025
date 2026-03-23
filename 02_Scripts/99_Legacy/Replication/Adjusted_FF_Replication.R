# ==============================================================================
# MASTER SCRIPT: Fama-French 5-Factor Replication (Full History Fix)
# ==============================================================================
# Description:
#   1. Downloads public benchmark data (FF, Macro, CPI).
#   2. Queries WRDS and collects raw data to R memory immediately.
#   3. Corrects for Delisting Bias, Accounting Shifts, and "Intersection Fallacy".
#   4. Removes FRED Risk-Free dependency to ensure full 1960-2024 plots.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Environment Setup
# ------------------------------------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(frenchdata)
library(httr2)
library(dbplyr)
library(RPostgres)
library(lubridate)
library(zoo)
library(scales)
library(modelsummary)

# Define analysis window
start_date <- ymd("1960-01-01")
end_date   <- ymd("2024-12-31")

if (!dir.exists("data")) dir.create("data")
tidy_finance <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite", extended_types = TRUE)

# ==============================================================================
# PHASE 1: PUBLIC DATA DOWNLOADS
# ==============================================================================

# 1.1 Fama-French 3 Factors (Benchmark)
message("Downloading FF3 Factors...")
factors_ff3_monthly_raw <- download_french_data("Fama/French 3 Factors")
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

# 1.2 Fama-French 5 Factors (Benchmark)
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

# 1.3 Macroeconomic Predictors
message("Downloading Macro Predictors...")
sheet_id <- "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
sheet_name <- "Monthly"
macro_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/gviz/tq?tqx=out:csv&sheet=", sheet_name)

macro_predictors <- read_csv(macro_url) |>
  mutate(date = ym(yyyymm)) |>
  mutate(across(where(is.character), as.numeric)) |>
  mutate(
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

# 1.4 CPI Data
message("Downloading CPI Data...")
cpi_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCNS"
cpi_monthly <- request(cpi_url) |>
  req_perform() |>
  resp_body_string() |>
  read_csv() |>
  mutate(
    date = floor_date(as.Date(observation_date), "month"),
    value = as.numeric(CPIAUCNS),
    cpi = value / value[date == max(date)],
    .keep = "none"
  )
dbWriteTable(tidy_finance, "cpi_monthly", cpi_monthly, overwrite = TRUE)

# 1.5 Momentum Factor (Benchmark)
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
# 2.1 CRSP Monthly (Collect-Then-Process)
# ------------------------------------------------------------------------------
message("Processing CRSP Monthly Data (Local Memory Processing)...")

msf_db <- tbl(wrds, I("crsp.msf_v2"))
stk_info_db <- tbl(wrds, I("crsp.stksecurityinfohist"))
delist_db <- tbl(wrds, I("crsp.stkdelists"))

# 1. Collect Delisting Data FIRST
delist_raw <- delist_db |>
  collect() |>
  rename_with(str_to_lower)

# Standardize Delisting Columns (Local R Logic)
if ("mthdlret" %in% names(delist_raw)) delist_raw <- rename(delist_raw, dlret = mthdlret)
if ("mthdlstcd" %in% names(delist_raw)) delist_raw <- rename(delist_raw, dlstcd = mthdlstcd)
if ("mthdlstdt" %in% names(delist_raw)) delist_raw <- rename(delist_raw, dlstdt = mthdlstdt)

# Fallback for missing columns
if (!"dlret" %in% names(delist_raw))  delist_raw$dlret <- 0
if (!"dlstcd" %in% names(delist_raw)) delist_raw$dlstcd <- 0
if (!"dlstdt" %in% names(delist_raw)) delist_raw$dlstdt <- as.Date(NA)

delist_final <- delist_raw |>
  mutate(
    dlret = as.numeric(dlret),
    dlstcd = as.numeric(dlstcd),
    dlstdt = as.Date(dlstdt)
  ) |>
  filter(!is.na(dlstdt)) |>
  mutate(date = floor_date(dlstdt, "month")) |>
  select(permno, dlret, dlstcd, date)

# 2. Collect Valid Stock Universe
valid_stocks <- stk_info_db |>
  filter(
    sharetype == "NS" & securitytype == "EQTY" & securitysubtype == "COM" &
      usincflg == "Y" & issuertype %in% c("ACOR", "CORP") &
      primaryexch %in% c("N", "A", "Q") & conditionaltype %in% c("RW", "NW")
  ) |>
  select(permno, secinfostartdt, secinfoenddt, primaryexch) |>
  collect() 

# 3. Collect Raw Monthly Data
crsp_raw <- msf_db |>
  filter(mthcaldt >= start_date & mthcaldt <= end_date) |>
  select(permno, permco, mthcaldt, mthret, shrout, mthprc) |> 
  collect()

# 4. Join and Process LOCALLY in R
crsp_monthly_processed <- crsp_raw |>
  inner_join(valid_stocks, by = "permno", relationship = "many-to-many") |>
  filter(mthcaldt >= secinfostartdt & mthcaldt <= secinfoenddt) |>
  mutate(date = floor_date(mthcaldt, "month")) |>
  left_join(delist_final, by = c("permno", "date")) |>
  mutate(
    date = ymd(date),
    shrout = shrout * 1000,
    exchange = case_when(
      primaryexch == "N" ~ "NYSE",
      primaryexch == "A" ~ "AMEX",
      primaryexch == "Q" ~ "NASDAQ",
      .default = "Other"
    ),
    # Shumway Correction
    dlret_adj = case_when(
      !is.na(dlret) & dlret != 0 ~ dlret,
      dlstcd %in% c(500, 520, 580, 584) | (dlstcd >= 551 & dlstcd <= 574) ~ 
        if_else(exchange == "NASDAQ", -0.55, -0.30),
      TRUE ~ 0
    ),
    ret = case_when(
      !is.na(mthret) ~ (1 + mthret) * (1 + dlret_adj) - 1,
      is.na(mthret) & dlret_adj != 0 ~ dlret_adj,
      TRUE ~ mthret
    ),
    mktcap_permno = abs(shrout * mthprc) / 10^6,
    mktcap_permno = na_if(mktcap_permno, 0)
  )

permco_mktcap <- crsp_monthly_processed |>
  group_by(date, permco) |>
  summarize(mktcap_permco = sum(mktcap_permno, na.rm = TRUE), .groups = "drop")

crsp_monthly <- crsp_monthly_processed |>
  left_join(permco_mktcap, join_by(date, permco)) |>
  mutate(mktcap = mktcap_permco) |>
  left_join(
    crsp_monthly_processed |> mutate(date = date %m+% months(1)) |> select(permno, date, mktcap_lag = mktcap_permno),
    join_by(permno, date)
  ) |>
  inner_join(factors_ff5_monthly |> select(date, rf), by = "date") |>
  mutate(ret_excess = ret - rf, ret_excess = pmax(ret_excess, -1)) |>
  drop_na(ret_excess, mktcap, mktcap_lag)

dbWriteTable(tidy_finance, "crsp_monthly", crsp_monthly, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 2.2 Compustat with Forensic Accounting Adjustments
# ------------------------------------------------------------------------------
message("Processing Compustat Data...")

compustat_raw <- tbl(wrds, I("comp.funda")) |>
  filter(
    indfmt == "INDL" & datafmt == "STD" & consol == "C" & curcd == "USD" &
      datadate >= start_date & datadate <= end_date
  ) |>
  select(gvkey, datadate, seq, ceq, at, lt, txditc, txdb, itcb, 
         pstkrv, pstkl, pstk, sale, cogs, xint, xsga, mib) |> 
  collect()

compustat_processed <- compustat_raw |>
  arrange(gvkey, datadate) |>
  group_by(gvkey) |>
  filter(row_number() > 2) |>
  mutate(
    year = year(datadate),
    # Forensic: FASB 109 rule
    txditc_adj = if_else(year >= 1993, 0, coalesce(txditc, txdb + itcb, 0)),
    
    # Forensic: Book Equity (Set to NA if negative)
    be = coalesce(seq, ceq + pstk, at - lt) + txditc_adj - coalesce(pstkrv, pstkl, pstk, 0),
    be = if_else(be <= 0, NA, be),
    
    # Forensic: 2018 MIB Profitability Revision
    denom_op = if_else(year >= 2018, be + coalesce(mib, 0), be),
    op = (sale - coalesce(cogs, 0) - coalesce(xsga, 0) - coalesce(xint, 0)) / denom_op
  ) |>
  group_by(gvkey, year) |>
  filter(datadate == max(datadate)) |>
  ungroup() |>
  left_join(
    compustat_raw |> 
      select(gvkey, datadate, at_lag = at) |> 
      mutate(year = year(datadate) + 1) |>
      select(gvkey, year, at_lag), 
    join_by(gvkey, year)
  ) |>
  # CMA FIX: Ensure strictly positive assets to avoid Infinite growth or weird zeros
  filter(at > 0 & at_lag > 0) |> 
  mutate(inv = at / at_lag - 1)

dbWriteTable(tidy_finance, "compustat", compustat_processed, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 2.3 Linking
# ------------------------------------------------------------------------------
ccm_link <- tbl(wrds, I("crsp.ccmxpf_lnkhist")) |>
  filter(linktype %in% c("LU", "LC") & linkprim %in% c("P", "C")) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, today()))

crsp_monthly_linked <- tbl(tidy_finance, "crsp_monthly") |> collect() |>
  inner_join(ccm_link, join_by(permno), relationship = "many-to-many") |>
  filter(date >= linkdt & date <= linkenddt)

dbWriteTable(tidy_finance, "crsp_monthly", crsp_monthly_linked, overwrite = TRUE)
dbDisconnect(wrds)

# ==============================================================================
# PHASE 3: FACTOR REPLICATION (INDEPENDENT SORTS)
# ==============================================================================
message("Replicating Factors (Independent Sorts)...")

# 1. Prepare Sorting Data
size <- crsp_monthly_linked |> filter(month(date) == 6) |>
  mutate(sorting_date = date %m+% months(1)) |> select(permno, exchange, sorting_date, size = mktcap)

market_equity <- crsp_monthly_linked |> filter(month(date) == 12) |>
  mutate(sorting_date = ymd(str_c(year(date) + 1, "0701"))) |> select(permno, gvkey, sorting_date, me = mktcap)

# Combine, but DO NOT drop_na all at once. Keep variables sparse.
sorting_variables <- compustat_processed |>
  mutate(sorting_date = ymd(str_c(year(datadate) + 1, "0701"))) |>
  inner_join(market_equity, by = c("gvkey", "sorting_date")) |>
  mutate(bm = be / me) |>
  inner_join(size, by = c("permno", "sorting_date")) 
# Note: No global drop_na() here!

assign_portfolio <- function(data, var, p) {
  # Calculate breakpoints ONLY on NYSE stocks that have valid data for 'var'
  valid_nyse <- data |> filter(exchange == "NYSE" & !is.na(!!sym(var)))
  breaks <- quantile(valid_nyse[[var]], probs = c(0, p, 1), na.rm = TRUE, names = FALSE)
  findInterval(data[[var]], breaks, all.inside = TRUE)
}

# 2. HML Construction (Universe: Valid Size & BM)
portfolios_hml <- sorting_variables |>
  filter(!is.na(size) & !is.na(bm)) |> # Independent Filter
  group_by(sorting_date) |>
  mutate(
    portfolio_size = assign_portfolio(pick(everything()), "size", 0.5),
    portfolio_bm   = assign_portfolio(pick(everything()), "bm", c(0.3, 0.7))
  ) |> ungroup() |>
  select(permno, sorting_date, hml_port_size = portfolio_size, hml_port_bm = portfolio_bm)

# 3. RMW Construction (Universe: Valid Size & OP)
portfolios_rmw <- sorting_variables |>
  filter(!is.na(size) & !is.na(op)) |> # Independent Filter
  group_by(sorting_date) |>
  mutate(
    portfolio_size = assign_portfolio(pick(everything()), "size", 0.5),
    portfolio_op   = assign_portfolio(pick(everything()), "op", c(0.3, 0.7))
  ) |> ungroup() |>
  select(permno, sorting_date, rmw_port_size = portfolio_size, rmw_port_op = portfolio_op)

# 4. CMA Construction (Universe: Valid Size & Inv) - NO BM CHECK HERE!
portfolios_cma <- sorting_variables |>
  filter(!is.na(size) & !is.na(inv)) |> # Independent Filter
  group_by(sorting_date) |>
  mutate(
    portfolio_size = assign_portfolio(pick(everything()), "size", 0.5),
    portfolio_inv  = assign_portfolio(pick(everything()), "inv", c(0.3, 0.7))
  ) |> ungroup() |>
  select(permno, sorting_date, cma_port_size = portfolio_size, cma_port_inv = portfolio_inv)

# 5. Merge and Calculate Returns
factors_replicated <- crsp_monthly_linked |>
  mutate(sorting_date = if_else(month(date) <= 6, ymd(str_c(year(date)-1, "0701")), ymd(str_c(year(date), "0701")))) |>
  # Join Independent Portfolios
  inner_join(portfolios_hml, by = c("permno", "sorting_date")) |>
  left_join(portfolios_rmw, by = c("permno", "sorting_date")) |> # Left join to allow missing OP
  left_join(portfolios_cma, by = c("permno", "sorting_date")) |> # Left join to allow missing Inv
  group_by(date) |>
  summarize(
    # HML: Uses hml_port_...
    hml_rep = mean(c(weighted.mean(ret_excess[hml_port_size==1 & hml_port_bm==3], mktcap_lag[hml_port_size==1 & hml_port_bm==3], na.rm=TRUE),
                     weighted.mean(ret_excess[hml_port_size==2 & hml_port_bm==3], mktcap_lag[hml_port_size==2 & hml_port_bm==3], na.rm=TRUE))) -
      mean(c(weighted.mean(ret_excess[hml_port_size==1 & hml_port_bm==1], mktcap_lag[hml_port_size==1 & hml_port_bm==1], na.rm=TRUE),
             weighted.mean(ret_excess[hml_port_size==2 & hml_port_bm==1], mktcap_lag[hml_port_size==2 & hml_port_bm==1], na.rm=TRUE))),
    
    # RMW: Uses rmw_port_...
    rmw_rep = mean(c(weighted.mean(ret_excess[rmw_port_size==1 & rmw_port_op==3], mktcap_lag[rmw_port_size==1 & rmw_port_op==3], na.rm=TRUE),
                     weighted.mean(ret_excess[rmw_port_size==2 & rmw_port_op==3], mktcap_lag[rmw_port_size==2 & rmw_port_op==3], na.rm=TRUE))) -
      mean(c(weighted.mean(ret_excess[rmw_port_size==1 & rmw_port_op==1], mktcap_lag[rmw_port_size==1 & rmw_port_op==1], na.rm=TRUE),
             weighted.mean(ret_excess[rmw_port_size==2 & rmw_port_op==1], mktcap_lag[rmw_port_size==2 & rmw_port_op==1], na.rm=TRUE))),
    
    # CMA: Uses cma_port_...
    cma_rep = mean(c(weighted.mean(ret_excess[cma_port_size==1 & cma_port_inv==1], mktcap_lag[cma_port_size==1 & cma_port_inv==1], na.rm=TRUE),
                     weighted.mean(ret_excess[cma_port_size==2 & cma_port_inv==1], mktcap_lag[cma_port_size==2 & cma_port_inv==1], na.rm=TRUE))) -
      mean(c(weighted.mean(ret_excess[cma_port_size==1 & cma_port_inv==3], mktcap_lag[cma_port_size==1 & cma_port_inv==3], na.rm=TRUE),
             weighted.mean(ret_excess[cma_port_size==2 & cma_port_inv==3], mktcap_lag[cma_port_size==2 & cma_port_inv==3], na.rm=TRUE))),
    
    # SMB: Average of the 3 independent sorts
    smb_rep = (
      # SMB from HML Sort
      (mean(c(weighted.mean(ret_excess[hml_port_size==1 & hml_port_bm==1], mktcap_lag[hml_port_size==1 & hml_port_bm==1], na.rm=TRUE),
              weighted.mean(ret_excess[hml_port_size==1 & hml_port_bm==2], mktcap_lag[hml_port_size==1 & hml_port_bm==2], na.rm=TRUE),
              weighted.mean(ret_excess[hml_port_size==1 & hml_port_bm==3], mktcap_lag[hml_port_size==1 & hml_port_bm==3], na.rm=TRUE))) -
         mean(c(weighted.mean(ret_excess[hml_port_size==2 & hml_port_bm==1], mktcap_lag[hml_port_size==2 & hml_port_bm==1], na.rm=TRUE),
                weighted.mean(ret_excess[hml_port_size==2 & hml_port_bm==2], mktcap_lag[hml_port_size==2 & hml_port_bm==2], na.rm=TRUE),
                weighted.mean(ret_excess[hml_port_size==2 & hml_port_bm==3], mktcap_lag[hml_port_size==2 & hml_port_bm==3], na.rm=TRUE)))) +
        # SMB from RMW Sort
        (mean(c(weighted.mean(ret_excess[rmw_port_size==1 & rmw_port_op==1], mktcap_lag[rmw_port_size==1 & rmw_port_op==1], na.rm=TRUE),
                weighted.mean(ret_excess[rmw_port_size==1 & rmw_port_op==2], mktcap_lag[rmw_port_size==1 & rmw_port_op==2], na.rm=TRUE),
                weighted.mean(ret_excess[rmw_port_size==1 & rmw_port_op==3], mktcap_lag[rmw_port_size==1 & rmw_port_op==3], na.rm=TRUE))) -
           mean(c(weighted.mean(ret_excess[rmw_port_size==2 & rmw_port_op==1], mktcap_lag[rmw_port_size==2 & rmw_port_op==1], na.rm=TRUE),
                  weighted.mean(ret_excess[rmw_port_size==2 & rmw_port_op==2], mktcap_lag[rmw_port_size==2 & rmw_port_op==2], na.rm=TRUE),
                  weighted.mean(ret_excess[rmw_port_size==2 & rmw_port_op==3], mktcap_lag[rmw_port_size==2 & rmw_port_op==3], na.rm=TRUE)))) +
        # SMB from CMA Sort
        (mean(c(weighted.mean(ret_excess[cma_port_size==1 & cma_port_inv==1], mktcap_lag[cma_port_size==1 & cma_port_inv==1], na.rm=TRUE),
                weighted.mean(ret_excess[cma_port_size==1 & cma_port_inv==2], mktcap_lag[cma_port_size==1 & cma_port_inv==2], na.rm=TRUE),
                weighted.mean(ret_excess[cma_port_size==1 & cma_port_inv==3], mktcap_lag[cma_port_size==1 & cma_port_inv==3], na.rm=TRUE))) -
           mean(c(weighted.mean(ret_excess[cma_port_size==2 & cma_port_inv==1], mktcap_lag[cma_port_size==2 & cma_port_inv==1], na.rm=TRUE),
                  weighted.mean(ret_excess[cma_port_size==2 & cma_port_inv==2], mktcap_lag[cma_port_size==2 & cma_port_inv==2], na.rm=TRUE),
                  weighted.mean(ret_excess[cma_port_size==2 & cma_port_inv==3], mktcap_lag[cma_port_size==2 & cma_port_inv==3], na.rm=TRUE))))
    ) / 3
  )

# ==============================================================================
# PHASE 3.5: MARKET FACTOR REPLICATION (Without FRED RF Dependency)
# ==============================================================================
message("Replicating Market Factor...")
mkt_replicated <- crsp_monthly_linked |>
  group_by(date) |>
  summarize(
    mkt_excess_replicated = weighted.mean(ret_excess, mktcap_lag, na.rm = TRUE),
    .groups = "drop"
  )

factors_replicated_full <- factors_replicated |>
  left_join(mkt_replicated, join_by(date))

dbWriteTable(tidy_finance, "factors_replicated_full", factors_replicated_full, overwrite = TRUE)

# ==============================================================================
# PHASE 3.6: MOMENTUM FACTOR REPLICATION
# ==============================================================================
message("Replicating Momentum Factor...")

crsp_mom_data <- crsp_monthly_linked |>
  select(permno, date, ret, mktcap_lag, exchange) |>
  mutate(date = ymd(date))

crsp_mom_signal <- crsp_mom_data |>
  arrange(permno, date) |>
  group_by(permno) |>
  mutate(
    gross_ret = 1 + ret,
    roll_ret_11m = zoo::rollapplyr(gross_ret, width = 11, FUN = prod, fill = NA) - 1,
    mom_signal = lag(roll_ret_11m, 2)
  ) |>
  ungroup() |>
  filter(!is.na(mom_signal) & !is.na(mktcap_lag)) |>
  select(permno, date, exchange, mom_signal, mktcap_lag, ret_excess = ret)

assign_mom_portfolio <- function(data) {
  nyse_subset <- data |> filter(exchange == "NYSE")
  size_bp <- quantile(nyse_subset$mktcap_lag, probs = 0.5, na.rm = TRUE)
  mom_bp <- quantile(nyse_subset$mom_signal, probs = c(0.3, 0.7), na.rm = TRUE)
  
  data |>
    mutate(
      portfolio_size = if_else(mktcap_lag <= size_bp, "Small", "Big"),
      portfolio_mom = case_when(
        mom_signal <= mom_bp[1] ~ "Low",
        mom_signal > mom_bp[2] ~ "High",
        TRUE ~ "Neutral"
      )
    )
}

mom_portfolios <- crsp_mom_signal |>
  group_by(date) |>
  group_split() |>
  map_dfr(assign_mom_portfolio)

mom_replicated <- mom_portfolios |>
  group_by(date, portfolio_size, portfolio_mom) |>
  summarize(ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop") |>
  group_by(date) |>
  summarize(
    mom_replicated = mean(ret[portfolio_mom == "High"]) - mean(ret[portfolio_mom == "Low"])
  )

dbWriteTable(tidy_finance, "mom_replicated", mom_replicated, overwrite = TRUE)

# ==============================================================================
# PHASE 4: VALIDATION & TABLES
# ==============================================================================
message("Generating Tables...")

factors_ff5_monthly <- tbl(tidy_finance, "factors_ff5_monthly") |> collect() |> mutate(date = ymd(date))
factors_mom_monthly <- tbl(tidy_finance, "factors_mom_monthly") |> collect() |> mutate(date = ymd(date))

test_data <- factors_ff5_monthly |>
  inner_join(factors_replicated_full, join_by(date)) |>
  left_join(factors_mom_monthly, join_by(date)) |>
  left_join(mom_replicated, join_by(date))

regs <- list(
  "Mkt-RF" = lm(mkt_excess ~ mkt_excess_replicated, data = test_data),
  "SMB"    = lm(smb ~ smb_rep, data = test_data),
  "HML"    = lm(hml ~ hml_rep, data = test_data),
  "RMW"    = lm(rmw ~ rmw_rep, data = test_data),
  "CMA"    = lm(cma ~ cma_rep, data = test_data),
  "MOM"    = lm(mom ~ mom_replicated, data = test_data)
)

msummary(
  regs,
  fmt = 4,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r.squared"),
  title = "Factor Replication Validity"
)

# ==============================================================================
# PHASE 5: VISUALIZATION
# ==============================================================================
message("Generating Plots...")

plot_data <- test_data |>
  select(date, 
         mkt_excess_Off = mkt_excess, mkt_excess_Rep = mkt_excess_replicated,
         smb_Off = smb,                smb_Rep = smb_rep,
         hml_Off = hml,                hml_Rep = hml_rep,
         rmw_Off = rmw,                rmw_Rep = rmw_rep,
         cma_Off = cma,                cma_Rep = cma_rep,
         mom_Off = mom,                mom_Rep = mom_replicated) |>
  pivot_longer(cols = -date, names_to = "key", values_to = "ret") |>
  mutate(
    factor = str_remove(key, "(_Off|_Rep)$"), 
    source = if_else(str_detect(key, "_Off"), "Official", "Replicated"),
    factor = case_when(
      factor == "mkt_excess" ~ "Mkt-RF",
      factor == "smb" ~ "SMB",
      factor == "hml" ~ "HML",
      factor == "rmw" ~ "RMW",
      factor == "cma" ~ "CMA",
      factor == "mom" ~ "MOM",
      TRUE ~ factor
    )
  ) |>
  arrange(factor, source, date) |>
  group_by(factor, source) |>
  mutate(cum_ret = cumprod(1 + ret) - 1) |>
  ungroup()

p <- ggplot(plot_data, aes(x = date, y = cum_ret, color = source, linetype = source)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~factor, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("Official" = "black", "Replicated" = "#E03C31")) + 
  scale_linetype_manual(values = c("Official" = "solid", "Replicated" = "dashed")) +
  labs(
    title = "Factor Replication Quality: Cumulative Returns",
    subtitle = "Comparing Replicated Factors (Red) against Official Fama-French Benchmark (Black)",
    x = NULL, y = "Cumulative Return", color = "Source", linetype = "Source"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 11), plot.title = element_text(face = "bold", size = 14))

print(p)

# ==============================================================================
# PHASE 6: VISUALIZATION (Cumulative Returns Post-2000)
# ==============================================================================
message("Generating Post-2000 Plots...")

# Prepare Plot Data (Filtered for Post-2000)
plot_data_2000 <- test_data |>
  filter(date >= ymd("2000-01-01")) |> # Filter start date
  select(date, 
         mkt_excess_Off = mkt_excess, mkt_excess_Rep = mkt_excess_replicated,
         smb_Off = smb,               smb_Rep = smb_rep,
         hml_Off = hml,               hml_Rep = hml_rep,
         rmw_Off = rmw,               rmw_Rep = rmw_rep,
         cma_Off = cma,               cma_Rep = cma_rep,
         mom_Off = mom,               mom_Rep = mom_replicated) |>
  pivot_longer(cols = -date, names_to = "key", values_to = "ret") |>
  mutate(
    factor = str_remove(key, "(_Off|_Rep)$"), 
    source = if_else(str_detect(key, "_Off"), "Official", "Replicated"),
    factor = case_when(
      factor == "mkt_excess" ~ "Mkt-RF",
      factor == "smb" ~ "SMB",
      factor == "hml" ~ "HML",
      factor == "rmw" ~ "RMW",
      factor == "cma" ~ "CMA",
      factor == "mom" ~ "MOM",
      TRUE ~ factor
    )
  ) |>
  arrange(factor, source, date) |>
  group_by(factor, source) |>
  # Calculation starts fresh from the filtered date (Rebased to 0)
  mutate(cum_ret = cumprod(1 + ret) - 1) |>
  ungroup()

# Generate Plot
p_2000 <- ggplot(plot_data_2000, aes(x = date, y = cum_ret, color = source, linetype = source)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~factor, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("Official" = "black", "Replicated" = "#E03C31")) + 
  scale_linetype_manual(values = c("Official" = "solid", "Replicated" = "dashed")) +
  labs(
    title = "Factor Replication Quality: Cumulative Returns (Since 2000)",
    subtitle = "Rebased to 0% at Jan 2000 | Comparing Replicated (Red) vs Official (Black)",
    x = NULL,
    y = "Cumulative Return",
    color = "Source",
    linetype = "Source"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_2000)


message("Script Complete.")