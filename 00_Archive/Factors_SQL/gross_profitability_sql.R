# ==============================================================================
# FACTOR BUILDER: Gross Profitability (Novy-Marx 2013) — SQLite-first
# Factor name (benchmark): gp_at
# Signal: (REVT - COGS) / AT   (fallback: SALE if REVT missing)
# Portfolios: NYSE breakpoints quintiles
# Return: VW, Q5 - Q1
# Writes: factor_returns, factor_holdings, factor_gross_profitability
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
  
  # Use ret_excess if present (matches old scripts); otherwise fallback to ret
  if ("ret_excess" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret_excess)
  } else if ("ret" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret)
  } else {
    stop("crsp_monthly must contain ret_excess or ret.")
  }
  
  # ---- Load Compustat items from SQLite (preferred) ----
  comp_cols <- intersect(
    c("gvkey", "datadate", "revt", "sale", "cogs", "at"),
    dbListFields(con, "comp_funda_core")
  )
  
  comp_prof_raw <- NULL
  if (all(c("gvkey","datadate","at") %in% comp_cols) && ("cogs" %in% comp_cols) && (("revt" %in% comp_cols) || ("sale" %in% comp_cols))) {
    
    comp_prof_raw <- tbl(con, "comp_funda_core") |>
      select(any_of(c("gvkey","datadate","revt","sale","cogs","at"))) |>
      collect() |>
      mutate(datadate = as_date_any(datadate)) |>
      filter(!is.na(datadate))
    
  } else if (allow_wrds_fetch) {
    
    # If the DB doesn’t have revt/cogs, fetch minimal columns from WRDS (same as his old script)
    # NOTE: This doesn’t change construction; it only supplies missing fields.
    if (!requireNamespace("RPostgres", quietly = TRUE)) {
      stop("Missing package RPostgres. Install it or set allow_wrds_fetch = FALSE.")
    }
    message("SQLite missing revt/cogs (or sale/cogs). Fetching from WRDS...")
    
    u <- Sys.getenv("WRDS_USER")
    p <- Sys.getenv("WRDS_PASSWORD")
    if (!nzchar(u) || !nzchar(p)) {
      stop("Missing WRDS_USER / WRDS_PASSWORD env vars. Set them (do NOT hardcode) or set allow_wrds_fetch = FALSE.")
    }
    
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
    
    comp_prof_raw <- dplyr::tbl(wrds, dbplyr::in_schema("comp", "funda")) |>
      filter(
        indfmt == "INDL", datafmt == "STD", consol == "C", curcd == "USD",
        datadate >= "1960-01-01"
      ) |>
      select(gvkey, datadate, revt, sale, cogs, at) |>
      collect() |>
      mutate(datadate = as_date_any(datadate)) |>
      filter(!is.na(datadate))
    
    # Optional: cache into SQLite so next run is DB-only
    if (!dbExistsTable(con, "comp_profitability_core")) {
      dbWriteTable(con, "comp_profitability_core", comp_prof_raw, overwrite = TRUE)
    } else {
      dbWriteTable(con, "comp_profitability_core", comp_prof_raw, overwrite = TRUE)
    }
    
  } else {
    stop("SQLite does not contain needed fields (cogs + (revt or sale) + at). ",
         "Either add them to comp_funda_core or set allow_wrds_fetch = TRUE and provide WRDS env vars.")
  }
  
  # ==============================================================================
  # 3) Construct the Signal (IDENTICAL)
  # ==============================================================================
  gp_signal <- comp_prof_raw |>
    arrange(gvkey, datadate) |>
    group_by(gvkey) |>
    mutate(
      # Use revt when present; fallback to sale only if revt missing
      rev_use = dplyr::coalesce(revt, sale),
      gp = rev_use - cogs,
      gp_at = gp / at
    ) |>
    ungroup() |>
    filter(at > 0, !is.na(gp_at), is.finite(gp_at)) |>
    select(gvkey, datadate, gp_at)
  
  # ==============================================================================
  # 4) Timeline Alignment (IDENTICAL)
  # ==============================================================================
  gp_signal_lagged <- gp_signal |>
    mutate(sorting_date = as.Date(paste0(year(datadate) + 1, "-07-01"))) |>
    select(gvkey, sorting_date, gp_at)
  
  # ==============================================================================
  # 5) Create sorting universe: June market cap + GP signal (IDENTICAL)
  # FIX: June in CRSP is month-end (06-30), so June+1 month ≠ 07-01.
  # Force sorting_date = July 1 to match Compustat lag convention.
  # ==============================================================================
  sorting_data <- crsp_monthly |>
    filter(month(date) == 6) |>
    mutate(sorting_date = as.Date(paste0(year(date), "-07-01"))) |>
    select(permno, gvkey, exchange, sorting_date, mktcap_june = mktcap) |>
    inner_join(gp_signal_lagged, by = c("gvkey", "sorting_date")) |>
    drop_na()
  
  stopifnot("permno" %in% names(sorting_data))
  stopifnot("sorting_date" %in% names(sorting_data))
  
  # ==============================================================================
  # 6) NYSE breakpoints (IDENTICAL)
  # ==============================================================================
  assign_nm_portfolio <- function(data) {
    nyse_breaks <- data |>
      filter(exchange == "NYSE") |>
      pull(gp_at) |>
      quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
    
    data |>
      mutate(portfolio = findInterval(gp_at, nyse_breaks, all.inside = TRUE))
  }
  
  portfolios_formed <- sorting_data |>
    group_by(sorting_date) |>
    group_split() |>
    purrr::map_dfr(assign_nm_portfolio) |>
    select(permno, sorting_date, portfolio_gp = portfolio)
  
  # ==============================================================================
  # 7) Factor Returns (IDENTICAL): VW, Q5 - Q1
  # ==============================================================================
  gp_factor_monthly <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date")) |>
    group_by(date, portfolio_gp) |>
    summarize(ret = weighted.mean(ret_use, mktcap_lag, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = portfolio_gp, values_from = ret, names_prefix = "Q") |>
    mutate(pmu_replicated = Q5 - Q1) |>
    select(date, pmu_replicated)
  
  factor_returns <- gp_factor_monthly |>
    transmute(date, factor = "gp_at", ret = pmu_replicated)
  
  # ==============================================================================
  # Holdings (for momentum): Long Q5, Short Q1, VW within each leg
  # Normalized to gross=1 (sum abs weights = 1) — does not affect factor returns
  # ==============================================================================
  crsp_with_sortdate <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    ))
  
  joined <- crsp_with_sortdate |>
    inner_join(portfolios_formed, by = c("permno", "sorting_date"))
  
  factor_holdings <- joined |>
    filter(portfolio_gp %in% c(1, 5)) |>
    group_by(date, portfolio_gp) |>
    mutate(
      leg_w = mktcap_lag / sum(mktcap_lag, na.rm = TRUE),
      sign  = if_else(portfolio_gp == 5, 1, -1),
      w_factor_stock = sign * leg_w
    ) |>
    ungroup() |>
    group_by(date) |>
    mutate(w_factor_stock = w_factor_stock / sum(abs(w_factor_stock), na.rm = TRUE)) |>
    ungroup() |>
    transmute(date, factor = "gp_at", permno, w_factor_stock)
  
  # ==============================================================================
  # Write standardized outputs (append-safe)
  # ==============================================================================
  if (!dbExistsTable(con, "factor_returns")) {
    dbWriteTable(con, "factor_returns", factor_returns, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_returns WHERE factor = 'gp_at'")
    dbWriteTable(con, "factor_returns", factor_returns, append = TRUE)
  }
  
  if (!dbExistsTable(con, "factor_holdings")) {
    dbWriteTable(con, "factor_holdings", factor_holdings, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_holdings WHERE factor = 'gp_at'")
    dbWriteTable(con, "factor_holdings", factor_holdings, append = TRUE)
  }
  
  # Per-factor table like his original script
  dbWriteTable(con, "factor_gross_profitability", gp_factor_monthly, overwrite = TRUE)
  
  message("Done: gp_at written to factor_returns + factor_holdings.")
  
  list(
    returns  = factor_returns,
    holdings = factor_holdings
  )
}