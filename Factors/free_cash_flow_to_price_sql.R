# ==============================================================================
# FACTOR BUILDER: Free Cash Flow to Price (fcf_me) — DB-only
# Lakonishok et al. (1994) / JKP fcf_me
# Formula: (Operating CF - CAPX) / ME(Dec t-1)
# Construction IDENTICAL to original script; only data source is SQLite.
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(DBI)
library(RSQLite)

as_date_any <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))
  suppressWarnings(lubridate::ymd(x, quiet = TRUE))
}

build_factor <- function(DB_PATH = NULL) {
  
  if (is.null(DB_PATH)) {
    DB_PATH <- file.path(normalizePath(getwd(), winslash = "/"),
                         "data", "tidy_finance_r.sqlite")
  }
  if (!file.exists(DB_PATH)) {
    stop("Missing DB: ", DB_PATH, "\nRun Scripts/00_build_sqlite_db.R")
  }
  
  con <- dbConnect(SQLite(), DB_PATH, extended_types = TRUE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # ---- Load inputs from SQLite ----
  crsp_monthly <- tbl(con, "crsp_monthly") |>
    select(any_of(c(
      "permno","gvkey","date",
      "ret","ret_excess",
      "mktcap","mktcap_lag",
      "exchange","exchcd","shrcd"
    ))) |>
    collect() |>
    mutate(date = as_date_any(date)) |>
    filter(!is.na(date))
  
  comp_fcf_raw <- tbl(con, "comp_funda_core") |>
    select(any_of(c("gvkey","datadate","oancf","capx","ib","dp"))) |>
    collect() |>
    mutate(datadate = as_date_any(datadate)) |>
    filter(!is.na(datadate))
  
  # Use ret_excess if available (matches original); else fallback to ret
  if ("ret_excess" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret_excess)
  } else if ("ret" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret)
  } else {
    stop("crsp_monthly must contain ret_excess or ret.")
  }
  
  # ==============================================================================
  # 2) Construct Signal (FCF / Price) -- IDENTICAL logic
  # ==============================================================================
  fcf_signal <- comp_fcf_raw |>
    arrange(gvkey, datadate) |>
    group_by(gvkey) |>
    mutate(
      cash_flow_gross = dplyr::coalesce(oancf, ib + dp),
      fcf = cash_flow_gross - dplyr::coalesce(capx, 0)
    ) |>
    ungroup() |>
    mutate(sorting_date = as.Date(paste0(year(datadate) + 1, "-07-01"))) |>
    filter(!is.na(fcf) & is.finite(fcf)) |>
    select(gvkey, sorting_date, fcf)
  
  # Denominator: Dec t-1 Market Cap -- IDENTICAL logic
  dec_market_cap <- crsp_monthly |>
    filter(month(date) == 12) |>
    select(permno, gvkey, mktcap_dec = mktcap, date) |>
    mutate(sorting_date = as.Date(paste0(year(date) + 1, "-07-01"))) |>
    select(-date)
  
  # Combine to create fcf_me -- IDENTICAL logic
  fcf_data <- fcf_signal |>
    inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
    mutate(fcf_me = fcf / mktcap_dec) |>
    filter(!is.na(fcf_me) & is.finite(fcf_me)) |>
    select(permno, sorting_date, fcf_me)
  
  # ==============================================================================
  # 3) Merge with June Market Data for Sorting -- IDENTICAL logic
  # FIX (same as at_gr1): June is month-end, so force sorting_date = Jul-01
  # ==============================================================================
  sorting_data <- crsp_monthly |>
    filter(month(date) == 6) |>
    mutate(sorting_date = as.Date(paste0(year(date), "-07-01"))) |>
    select(permno, exchange, sorting_date, mktcap_june = mktcap) |>
    inner_join(fcf_data, by = c("permno", "sorting_date"))
  
  stopifnot("permno" %in% names(sorting_data))
  stopifnot("sorting_date" %in% names(sorting_data))
  
  # ==============================================================================
  # 4) NYSE breakpoints + quintiles -- IDENTICAL logic
  # ==============================================================================
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
    purrr::map_dfr(assign_fcf_portfolio) |>
    select(permno, sorting_date, portfolio_fcf = portfolio)
  
  # ==============================================================================
  # 5) Factor Returns -- IDENTICAL logic (Q5 - Q1)
  # ==============================================================================
  fcf_factor <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
    group_by(date, portfolio_fcf) |>
    summarize(ret = weighted.mean(ret_use, mktcap_lag, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = portfolio_fcf, values_from = ret, names_prefix = "Q") |>
    mutate(fcf_me_replicated = Q5 - Q1) |>
    select(date, fcf_me_replicated)
  
  factor_returns <- fcf_factor |>
    transmute(date, factor = "fcf_me", ret = fcf_me_replicated)
  
  # ==============================================================================
  # Holdings: Long Q5, Short Q1, VW within each leg
  # ==============================================================================
  joined <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date"))
  
  factor_holdings <- joined |>
    filter(portfolio_fcf %in% c(1, 5)) |>
    group_by(date, portfolio_fcf) |>
    mutate(
      leg_w = mktcap_lag / sum(mktcap_lag, na.rm = TRUE),
      w_factor_stock = if_else(portfolio_fcf == 5,  leg_w, -leg_w)
    ) |>
    ungroup() |>
    transmute(date, factor = "fcf_me", permno, w_factor_stock)
  
  # ==============================================================================
  # 6) Write standardized outputs (append-safe)
  # ==============================================================================
  if (!dbExistsTable(con, "factor_returns")) {
    dbWriteTable(con, "factor_returns", factor_returns, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_returns WHERE factor = 'fcf_me'")
    dbWriteTable(con, "factor_returns", factor_returns, append = TRUE)
  }
  
  if (!dbExistsTable(con, "factor_holdings")) {
    dbWriteTable(con, "factor_holdings", factor_holdings, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_holdings WHERE factor = 'fcf_me'")
    dbWriteTable(con, "factor_holdings", factor_holdings, append = TRUE)
  }
  
  dbWriteTable(con, "factor_fcf_price", fcf_factor, overwrite = TRUE)
  
  list(
    returns  = factor_returns,
    holdings = factor_holdings
  )
}