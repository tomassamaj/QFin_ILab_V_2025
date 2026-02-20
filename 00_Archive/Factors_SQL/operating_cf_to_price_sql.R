# ==============================================================================
# FACTOR BUILDER: Cash Flow to Price (ocf_me) — SQLite-first
# Benchmark: JKP ocf_me
# Formula: OCF / ME(Dec t-1)
#   OCF = coalesce(oancf, ib + dp)   (same as your script)
# Direction: Long High (Q5) - Short Low (Q1)
# Sort: NYSE breakpoints (quintiles), applied to all exchanges
# Holding period: Jul t ... Jun t+1
# Weighting: Value-weighted using mktcap_lag (same)
# Outputs:
#   - factor_returns: (date, factor="ocf_me", ret)
#   - factor_holdings: net single-stock weights each month (gross=1)
#   - factor_operating_cashflow_price: per-factor table (optional)
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

build_factor <- function(DB_PATH = NULL, allow_wrds_fetch = TRUE) {
  
  if (is.null(DB_PATH)) {
    DB_PATH <- file.path(normalizePath(getwd(), winslash = "/"),
                         "data", "tidy_finance_r.sqlite")
  }
  if (!file.exists(DB_PATH)) {
    stop("Missing DB: ", DB_PATH, "\nRun Scripts/00_build_sqlite_db.R")
  }
  
  con <- dbConnect(SQLite(), DB_PATH, extended_types = TRUE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # ---- CRSP monthly ----
  crsp_monthly <- tbl(con, "crsp_monthly") |>
    collect() |>
    mutate(date = as_date_any(date)) |>
    filter(!is.na(date))
  
  # Use ret_excess if present; else ret
  if ("ret_excess" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret_excess)
  } else if ("ret" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret)
  } else {
    stop("crsp_monthly must contain ret_excess or ret.")
  }
  
  # ---- Compustat OCF inputs from SQLite if available ----
  need_cols <- c("gvkey","datadate","oancf","ib","dp")
  comp_cf_raw <- NULL
  
  if (dbExistsTable(con, "comp_funda_core")) {
    have_cols <- dbListFields(con, "comp_funda_core")
    if (all(need_cols %in% have_cols)) {
      comp_cf_raw <- tbl(con, "comp_funda_core") |>
        select(all_of(need_cols)) |>
        collect() |>
        mutate(datadate = as_date_any(datadate)) |>
        filter(!is.na(datadate))
    }
  }
  
  # Optional WRDS fetch if SQLite doesn't have required columns
  if (is.null(comp_cf_raw) && allow_wrds_fetch) {
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
      stop("Missing package RPostgres. Install it or set allow_wrds_fetch = FALSE.")
    }
    
    u <- Sys.getenv("WRDS_USER")
    p <- Sys.getenv("WRDS_PASSWORD")
    if (!nzchar(u) || !nzchar(p)) {
      stop("Missing WRDS_USER / WRDS_PASSWORD env vars. Set them or set allow_wrds_fetch = FALSE.")
    }
    
    message("SQLite missing oancf/ib/dp. Fetching from WRDS...")
    
    wrds <- dbConnect(
      RPostgres::Postgres(),
      host = "wrds-pgdata.wharton.upenn.edu",
      dbname = "wrds",
      port = 9737,
      sslmode = "require",
      user = u,
      password = p
    )
    on.exit(try(dbDisconnect(wrds), silent = TRUE), add = TRUE)
    
    comp_cf_raw <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "funda")) |>
      filter(
        indfmt == "INDL", datafmt == "STD", consol == "C", curcd == "USD",
        datadate >= "1960-01-01"
      ) |>
      select(gvkey, datadate, oancf, ib, dp) |>
      collect() |>
      mutate(datadate = as_date_any(datadate)) |>
      filter(!is.na(datadate))
    
    # Cache (does not change construction; just avoids repeated WRDS hits)
    dbWriteTable(con, "comp_ocf_core", comp_cf_raw, overwrite = TRUE)
  }
  
  if (is.null(comp_cf_raw)) {
    stop(
      "Could not source Compustat OCF inputs.\n",
      "Either add columns to comp_funda_core (", paste(need_cols, collapse = ", "), ")\n",
      "or set allow_wrds_fetch=TRUE and provide WRDS_USER/WRDS_PASSWORD env vars."
    )
  }
  
  # ==============================================================================
  # 2) Construct signal (same logic)
  # ==============================================================================
  ocf_signal <- comp_cf_raw |>
    arrange(gvkey, datadate) |>
    group_by(gvkey) |>
    mutate(ocf = coalesce(oancf, ib + dp)) |>
    ungroup() |>
    mutate(sorting_date = as.Date(paste0(year(datadate) + 1, "-07-01"))) |>
    filter(!is.na(ocf), is.finite(ocf)) |>
    select(gvkey, sorting_date, ocf)
  
  # Dec t-1 market cap denominator (same)
  dec_market_cap <- crsp_monthly |>
    filter(month(date) == 12) |>
    select(permno, gvkey, mktcap_dec = mktcap, date) |>
    mutate(sorting_date = as.Date(paste0(year(date) + 1, "-07-01"))) |>
    select(-date)
  
  ocf_data <- ocf_signal |>
    inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
    mutate(ocf_me = ocf / mktcap_dec) |>
    filter(!is.na(ocf_me), is.finite(ocf_me)) |>
    select(permno, gvkey, sorting_date, ocf_me)
  
  # ==============================================================================
  # 3) Merge with June market data (same)
  # ==============================================================================
  sorting_data <- crsp_monthly |>
    filter(month(date) == 6) |>
    mutate(sorting_date = date %m+% months(1)) |>
    select(permno, gvkey, exchange, sorting_date, mktcap_june = mktcap) |>
    inner_join(ocf_data, by = c("permno", "gvkey", "sorting_date")) |>
    drop_na()
  
  # ==============================================================================
  # 4) NYSE breakpoints (same)
  # ==============================================================================
  assign_ocf_portfolio <- function(data) {
    nyse_breaks <- data |>
      filter(exchange == "NYSE") |>
      pull(ocf_me) |>
      quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
    
    data |>
      mutate(portfolio = findInterval(ocf_me, nyse_breaks, all.inside = TRUE))
  }
  
  assignments <- sorting_data |>
    group_by(sorting_date) |>
    group_split() |>
    purrr::map_dfr(assign_ocf_portfolio) |>
    ungroup() |>
    transmute(permno, sorting_date, portfolio = portfolio)
  
  # ==============================================================================
  # 5) Join monthly returns to assignments (Jul–Jun rule) (same)
  # ==============================================================================
  crsp_with_sortdate <- crsp_monthly |>
    mutate(
      sorting_date = if_else(
        month(date) <= 6,
        as.Date(paste0(year(date) - 1, "-07-01")),
        as.Date(paste0(year(date), "-07-01"))
      )
    )
  
  joined <- crsp_with_sortdate |>
    inner_join(assignments, by = c("permno", "sorting_date"))
  
  # ==============================================================================
  # 6) Factor returns: Q5 - Q1 (VW) (same)
  # ==============================================================================
  port_ret <- joined |>
    group_by(date, portfolio) |>
    summarise(r = weighted.mean(ret_use, mktcap_lag, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = portfolio, values_from = r, names_prefix = "Q")
  
  ocf_factor_returns <- port_ret |>
    mutate(ret = Q5 - Q1) |>
    transmute(date, factor = "ocf_me", ret)
  
  # ==============================================================================
  # 7) Factor holdings (net single-stock weights each month)
  #    Long leg = Q5, Short leg = Q1, VW within each leg, gross normalized to 1
  # ==============================================================================
  ocf_factor_holdings <- joined |>
    filter(portfolio %in% c(1, 5)) |>
    group_by(date, portfolio) |>
    mutate(
      leg_w = mktcap_lag / sum(mktcap_lag, na.rm = TRUE),
      sign  = if_else(portfolio == 5, 1, -1),   # long Q5, short Q1
      w_factor_stock = sign * leg_w
    ) |>
    ungroup() |>
    group_by(date) |>
    mutate(w_factor_stock = w_factor_stock / sum(abs(w_factor_stock), na.rm = TRUE)) |>
    ungroup() |>
    transmute(date, factor = "ocf_me", permno, w_factor_stock)
  
  # ==============================================================================
  # 8) Write standardized outputs (append-safe)
  # ==============================================================================
  if (!dbExistsTable(con, "factor_returns")) {
    dbWriteTable(con, "factor_returns", ocf_factor_returns, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_returns WHERE factor = 'ocf_me'")
    dbWriteTable(con, "factor_returns", ocf_factor_returns, append = TRUE)
  }
  
  if (!dbExistsTable(con, "factor_holdings")) {
    dbWriteTable(con, "factor_holdings", ocf_factor_holdings, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_holdings WHERE factor = 'ocf_me'")
    dbWriteTable(con, "factor_holdings", ocf_factor_holdings, append = TRUE)
  }
  
  # Optional per-factor table (like your old pattern)
  dbWriteTable(con, "factor_operating_cashflow_price", ocf_factor_returns, overwrite = TRUE)
  
  message("Done: ocf_me written to factor_returns + factor_holdings.")
  
  list(
    returns  = ocf_factor_returns,
    holdings = ocf_factor_holdings
  )
}