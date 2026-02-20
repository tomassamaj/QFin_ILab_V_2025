# ==============================================================================
# FACTOR BUILDER: Earnings-to-Price (ni_me) — DB-only
# Basu (1983) | Signal: NI / ME(Dec t-1) | Long Q5 - Short Q1
# NOTE: Construction identical to original script; only the data source is SQLite.
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
  
  # -------------------------
  # 1) Load inputs from SQLite
  # -------------------------
  if (!dbExistsTable(con, "crsp_monthly")) stop("Missing table: crsp_monthly")
  if (!dbExistsTable(con, "comp_funda_core")) stop("Missing table: comp_funda_core")
  
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
  
  # Hard requirements
  req_crsp <- c("permno","date","mktcap","mktcap_lag","exchange","gvkey")
  missing_crsp <- setdiff(req_crsp, names(crsp_monthly))
  if (length(missing_crsp) > 0) {
    stop(
      "crsp_monthly in DB is missing required columns: ",
      paste(missing_crsp, collapse = ", "),
      "\nDB_PATH used: ", DB_PATH,
      "\nColumns found: ", paste(names(crsp_monthly), collapse = ", ")
    )
  }
  
  # Use ret_excess if present, else ret (no strategy change; just availability)
  if ("ret_excess" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret_excess)
  } else if ("ret" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret)
  } else {
    stop("crsp_monthly must contain ret_excess or ret.")
  }
  
  comp_ni_raw <- tbl(con, "comp_funda_core") |>
    select(any_of(c("gvkey","datadate","ni"))) |>
    collect() |>
    mutate(datadate = as_date_any(datadate)) |>
    filter(!is.na(datadate))
  
  req_comp <- c("gvkey","datadate","ni")
  missing_comp <- setdiff(req_comp, names(comp_ni_raw))
  if (length(missing_comp) > 0) {
    stop(
      "comp_funda_core in DB is missing required columns: ",
      paste(missing_comp, collapse = ", "),
      "\nColumns found: ", paste(names(comp_ni_raw), collapse = ", ")
    )
  }
  
  # ==============================================================================
  # 2) Construct Signal (Earnings / Price) -- IDENTICAL logic
  # ==============================================================================
  earnings_signal <- comp_ni_raw |>
    select(gvkey, datadate, ni) |>
    mutate(sorting_date = as.Date(paste0(year(datadate) + 1, "-07-01"))) |>
    drop_na(ni)
  
  dec_market_cap <- crsp_monthly |>
    filter(month(date) == 12) |>
    select(permno, gvkey, mktcap_dec = mktcap, date) |>
    mutate(sorting_date = as.Date(paste0(year(date) + 1, "-07-01"))) |>
    select(-date)
  
  ep_signal <- earnings_signal |>
    inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
    mutate(ni_me = ni / mktcap_dec) |>
    filter(!is.na(ni_me) & is.finite(ni_me)) |>
    select(permno, sorting_date, ni_me)
  
  # ==============================================================================
  # 3) Merge with June Market Data for Sorting -- IDENTICAL logic
  # FIX (same as your at_gr1): CRSP is month-end, Comp signal is keyed to Jul-01.
  # So we force sorting_date = Jul-01 of the same year as the June observation.
  # ==============================================================================
  sorting_data <- crsp_monthly |>
    filter(month(date) == 6) |>
    mutate(sorting_date = as.Date(paste0(year(date), "-07-01"))) |>
    select(permno, exchange, sorting_date, mktcap_june = mktcap) |>
    inner_join(ep_signal, by = c("permno", "sorting_date"))
  
  # ==============================================================================
  # 4) Portfolio Construction (NYSE Breakpoints) -- IDENTICAL logic
  # ==============================================================================
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
    purrr::map_dfr(assign_ep_portfolio) |>
    select(permno, sorting_date, portfolio_ep = portfolio)
  
  # ==============================================================================
  # 5) Factor Returns -- IDENTICAL logic (Q5 - Q1)
  # ==============================================================================
  ep_factor <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
    group_by(date, portfolio_ep) |>
    summarize(ret = weighted.mean(ret_use, mktcap_lag, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = portfolio_ep, values_from = ret, names_prefix = "Q") |>
    mutate(ni_me_replicated = Q5 - Q1) |>
    select(date, ni_me_replicated)
  
  factor_returns <- ep_factor |>
    transmute(date, factor = "ni_me", ret = ni_me_replicated)
  
  # ==============================================================================
  # Holdings: Long Q5, Short Q1, VW within each leg (consistent with factor return)
  # ==============================================================================
  joined <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date"))
  
  factor_holdings <- joined |>
    filter(portfolio_ep %in% c(1, 5)) |>
    group_by(date, portfolio_ep) |>
    mutate(
      leg_w = mktcap_lag / sum(mktcap_lag, na.rm = TRUE),
      w_factor_stock = if_else(portfolio_ep == 5,  leg_w, -leg_w)
    ) |>
    ungroup() |>
    transmute(date, factor = "ni_me", permno, w_factor_stock)
  
  # ==============================================================================
  # Write standardized outputs (append-safe)
  # ==============================================================================
  if (!dbExistsTable(con, "factor_returns")) {
    dbWriteTable(con, "factor_returns", factor_returns, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_returns WHERE factor = 'ni_me'")
    dbWriteTable(con, "factor_returns", factor_returns, append = TRUE)
  }
  
  if (!dbExistsTable(con, "factor_holdings")) {
    dbWriteTable(con, "factor_holdings", factor_holdings, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_holdings WHERE factor = 'ni_me'")
    dbWriteTable(con, "factor_holdings", factor_holdings, append = TRUE)
  }
  
  dbWriteTable(con, "factor_earnings_price", ep_factor, overwrite = TRUE)
  
  list(
    returns  = factor_returns,
    holdings = factor_holdings
  )
}