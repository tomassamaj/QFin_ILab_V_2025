# ==============================================================================
# FACTOR BUILDER: Operating Accruals (Sloan 1996) — SQLite-first
# Factor name (benchmark): oaccruals_at
# Formula (your exact construction):
#   (ΔCA - ΔCash) - (ΔCL - ΔSTD - ΔTP) - Dep, scaled by AT(t-1)
# Strategy: Long Q1 (low accruals) - Short Q5 (high accruals)
# Portfolios: NYSE breakpoints quintiles
# Return: VW, Q1 - Q5
# Writes: factor_returns, factor_holdings, factor_operating_accruals
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
  
  # ---- Load CRSP monthly from SQLite ----
  crsp_monthly <- tbl(con, "crsp_monthly") |>
    collect() |>
    mutate(date = as_date_any(date)) |>
    filter(!is.na(date))
  
  # Use ret_excess if present (matches your old scripts); otherwise fallback to ret
  if ("ret_excess" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret_excess)
  } else if ("ret" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret)
  } else {
    stop("crsp_monthly must contain ret_excess or ret.")
  }
  
  # ---- Load Compustat accrual components from SQLite if available ----
  need_cols <- c("gvkey","datadate","at","act","che","lct","dlc","txp","dp")
  
  comp_acc_raw <- NULL
  
  if (dbExistsTable(con, "comp_funda_core")) {
    have_cols <- dbListFields(con, "comp_funda_core")
    if (all(need_cols %in% have_cols)) {
      comp_acc_raw <- tbl(con, "comp_funda_core") |>
        select(all_of(need_cols)) |>
        collect() |>
        mutate(datadate = as_date_any(datadate)) |>
        filter(!is.na(datadate))
    }
  }
  
  # Optional WRDS fetch if SQLite doesn't have the columns
  if (is.null(comp_acc_raw) && allow_wrds_fetch) {
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
      stop("Missing package RPostgres. Install it or set allow_wrds_fetch = FALSE.")
    }
    
    u <- Sys.getenv("WRDS_USER")
    p <- Sys.getenv("WRDS_PASSWORD")
    if (!nzchar(u) || !nzchar(p)) {
      stop("Missing WRDS_USER / WRDS_PASSWORD env vars. Set them or set allow_wrds_fetch = FALSE.")
    }
    
    message("SQLite missing accrual inputs. Fetching Compustat components from WRDS...")
    
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
    
    comp_acc_raw <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "funda")) |>
      filter(
        indfmt == "INDL", datafmt == "STD", consol == "C", curcd == "USD",
        datadate >= "1960-01-01"
      ) |>
      select(gvkey, datadate, at, act, che, lct, dlc, txp, dp) |>
      collect() |>
      mutate(datadate = as_date_any(datadate)) |>
      filter(!is.na(datadate))
    
    # Cache it so next runs can be DB-only (doesn't change construction)
    dbWriteTable(con, "comp_accruals_core", comp_acc_raw, overwrite = TRUE)
  }
  
  if (is.null(comp_acc_raw)) {
    stop(
      "Could not source Compustat accrual inputs.\n",
      "Either add columns to comp_funda_core (", paste(need_cols, collapse = ", "), ")\n",
      "or set allow_wrds_fetch=TRUE and provide WRDS_USER/WRDS_PASSWORD env vars."
    )
  }
  
  # ==============================================================================
  # 2) Construct Signal (your exact construction)
  # ==============================================================================
  accruals_calc <- comp_acc_raw |>
    arrange(gvkey, datadate) |>
    group_by(gvkey) |>
    mutate(
      d_act = act - lag(act),
      d_che = che - lag(che),
      d_lct = lct - lag(lct),
      d_dlc = dlc - lag(dlc),
      d_txp = txp - lag(txp),
      at_lag = lag(at)
    ) |>
    ungroup() |>
    mutate(
      accruals_raw = (d_act - d_che) - (d_lct - d_dlc - d_txp) - dp,
      oaccruals_at = accruals_raw / at_lag
    ) |>
    filter(!is.na(oaccruals_at), is.finite(oaccruals_at), at_lag > 0) |>
    mutate(sorting_date = as.Date(paste0(year(datadate) + 1, "-07-01"))) |>
    select(gvkey, sorting_date, oaccruals_at)
  
  # ==============================================================================
  # 3) Merge with June market data for sorting (same)
  # FIX: CRSP June is month-end (06-30), so June+1 month != 07-01.
  # Force July 1 so it matches Compustat lag convention exactly.
  # ==============================================================================
  sorting_data <- crsp_monthly |>
    filter(month(date) == 6) |>
    mutate(sorting_date = as.Date(paste0(year(date), "-07-01"))) |>
    select(permno, exchange, gvkey, sorting_date, mktcap_june = mktcap) |>
    inner_join(accruals_calc, by = c("gvkey", "sorting_date")) |>
    drop_na()
  
  stopifnot("permno" %in% names(sorting_data))
  
  # ==============================================================================
  # 4) NYSE breakpoints (same)
  # ==============================================================================
  assign_acc_portfolio <- function(data) {
    nyse_breaks <- data |>
      filter(exchange == "NYSE") |>
      pull(oaccruals_at) |>
      quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
    
    data |>
      mutate(portfolio = findInterval(oaccruals_at, nyse_breaks, all.inside = TRUE))
  }
  
  portfolios_formed <- sorting_data |>
    group_by(sorting_date) |>
    group_split() |>
    purrr::map_dfr(assign_acc_portfolio) |>
    select(permno, sorting_date, portfolio_acc = portfolio)
  
  # ==============================================================================
  # 5) Factor returns: VW Q1 - Q5 (same direction)
  # ==============================================================================
  acc_factor_monthly <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
    group_by(date, portfolio_acc) |>
    summarize(ret = weighted.mean(ret_use, mktcap_lag, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = portfolio_acc, values_from = ret, names_prefix = "Q") |>
    mutate(oaccruals_at_replicated = Q1 - Q5) |>
    select(date, oaccruals_at_replicated)
  
  factor_returns <- acc_factor_monthly |>
    transmute(date, factor = "oaccruals_at", ret = oaccruals_at_replicated)
  
  # ==============================================================================
  # Holdings: Long Q1, Short Q5, VW within each leg, gross normalized to 1
  # ==============================================================================
  crsp_with_sortdate <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    ))
  
  joined <- crsp_with_sortdate |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date"))
  
  factor_holdings <- joined |>
    filter(portfolio_acc %in% c(1, 5)) |>
    group_by(date, portfolio_acc) |>
    mutate(
      leg_w = mktcap_lag / sum(mktcap_lag, na.rm = TRUE),
      sign  = if_else(portfolio_acc == 1, 1, -1),  # long Q1, short Q5
      w_factor_stock = sign * leg_w
    ) |>
    ungroup() |>
    group_by(date) |>
    mutate(w_factor_stock = w_factor_stock / sum(abs(w_factor_stock), na.rm = TRUE)) |>
    ungroup() |>
    transmute(date, factor = "oaccruals_at", permno, w_factor_stock)
  
  # ==============================================================================
  # Write standardized outputs (append-safe)
  # ==============================================================================
  if (!dbExistsTable(con, "factor_returns")) {
    dbWriteTable(con, "factor_returns", factor_returns, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_returns WHERE factor = 'oaccruals_at'")
    dbWriteTable(con, "factor_returns", factor_returns, append = TRUE)
  }
  
  if (!dbExistsTable(con, "factor_holdings")) {
    dbWriteTable(con, "factor_holdings", factor_holdings, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_holdings WHERE factor = 'oaccruals_at'")
    dbWriteTable(con, "factor_holdings", factor_holdings, append = TRUE)
  }
  
  dbWriteTable(con, "factor_operating_accruals", acc_factor_monthly, overwrite = TRUE)
  
  message("Done: oaccruals_at written to factor_returns + factor_holdings.")
  
  list(
    returns  = factor_returns,
    holdings = factor_holdings
  )
}