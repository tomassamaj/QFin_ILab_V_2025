# ==============================================================================
# FACTOR BUILDER: Asset Growth (Cooper, Gulen, Schill 2008) — DB-only
# Factor: at_gr1  |  Strategy: Long Q1 (Low Growth) - Short Q5 (High Growth)
# NOTE: Construction identical to the original script; only the data source is SQLite.
# ==============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(DBI)
library(RSQLite)

# Robust date parser for SQLite (avoids ymd() wiping everything to NA)
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
  
  # --- Load CRSP monthly (must already be in SQLite) ---
  crsp_monthly <- tbl(con, "crsp_monthly") %>%
    select(any_of(c(
      "permno","gvkey","date",
      "ret","ret_excess",
      "mktcap","mktcap_lag",
      "exchange","exchcd","shrcd"
    ))) %>%
    collect() %>%
    mutate(date = as_date_any(date)) %>%
    filter(!is.na(date))
  
  # Use ret_excess if present, else fall back to ret (DB may not have ret_excess)
  if ("ret_excess" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly %>% mutate(ret_use = ret_excess)
  } else if ("ret" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly %>% mutate(ret_use = ret)
  } else {
    stop("crsp_monthly must contain ret_excess or ret.")
  }
  
  # --- Load Compustat AT from SQLite ---
  comp_at_raw <- tbl(con, "comp_funda_core") %>%
    select(gvkey, datadate, at) %>%
    collect() %>%
    mutate(datadate = as_date_any(datadate)) %>%
    filter(!is.na(datadate))
  
  # ==============================================================================
  # 2) Construct Signal (Asset Growth)  -- IDENTICAL logic
  # ==============================================================================
  at_annual <- comp_at_raw %>%
    mutate(year = year(datadate)) %>%
    group_by(gvkey, year) %>%
    filter(datadate == max(datadate, na.rm = TRUE)) %>%
    ungroup() %>%
    select(gvkey, year, datadate, at)
  
  at_lagged <- at_annual %>%
    mutate(year = year + 1) %>%
    select(gvkey, year, at_lag = at)
  
  at_growth_signal <- at_annual %>%
    inner_join(at_lagged, by = c("gvkey", "year")) %>%
    mutate(at_gr1 = (at - at_lag) / at_lag) %>%
    filter(!is.na(at_gr1), is.finite(at_gr1), at_lag > 0) %>%
    mutate(sorting_date = as.Date(paste0(year + 1, "-07-01"))) %>%
    select(gvkey, sorting_date, at_gr1)
  
  # ==============================================================================
  # 3) Merge with June Market Data for Sorting  -- IDENTICAL logic
  # FIX: CRSP dates are month-end (e.g., 06-30), so June+1 month != 07-01.
  # We force July 1 for sorting_date (no strategy change).
  # ==============================================================================
  sorting_data <- crsp_monthly %>%
    filter(month(date) == 6) %>%
    mutate(sorting_date = as.Date(paste0(year(date), "-07-01"))) %>%
    select(permno, exchange, gvkey, sorting_date, mktcap_june = mktcap) %>%
    inner_join(at_growth_signal, by = c("gvkey", "sorting_date"))
  
  # Defensive checks (helps you catch “permno dropped” instantly)
  stopifnot("permno" %in% names(sorting_data))
  stopifnot("sorting_date" %in% names(sorting_data))
  
  # ==============================================================================
  # 4) Portfolio Construction (NYSE Breakpoints)  -- IDENTICAL logic
  # ==============================================================================
  assign_at_portfolio <- function(data) {
    nyse_breaks <- data %>%
      filter(exchange == "NYSE") %>%
      pull(at_gr1) %>%
      quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
    
    data %>%
      mutate(portfolio = findInterval(at_gr1, nyse_breaks, all.inside = TRUE))
  }
  
  portfolios_formed <- sorting_data %>%
    group_by(sorting_date) %>%
    group_split() %>%
    purrr::map_dfr(assign_at_portfolio) %>%
    select(permno, sorting_date, portfolio_at = portfolio)
  
  # ==============================================================================
  # 5) Factor Returns (Low Growth Minus High Growth)  -- IDENTICAL logic
  # ==============================================================================
  at_factor <- crsp_monthly %>%
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) %>%
    inner_join(portfolios_formed, by = c("permno", "sorting_date")) %>%
    group_by(date, portfolio_at) %>%
    summarize(ret = weighted.mean(ret_use, mktcap_lag, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = portfolio_at, values_from = ret, names_prefix = "Q") %>%
    mutate(at_gr1_replicated = Q1 - Q5) %>%
    select(date, at_gr1_replicated)
  
  factor_returns <- at_factor %>%
    transmute(date, factor = "at_gr1", ret = at_gr1_replicated)
  
  # ==============================================================================
  # Holdings consistent with Q1 - Q5 (VW within legs)
  # ==============================================================================
  holdings_joined <- crsp_monthly %>%
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) %>%
    inner_join(portfolios_formed, by = c("permno", "sorting_date"))
  
  factor_holdings <- holdings_joined %>%
    filter(portfolio_at %in% c(1, 5)) %>%
    group_by(date, portfolio_at) %>%
    mutate(
      leg_w = mktcap_lag / sum(mktcap_lag, na.rm = TRUE),
      w_factor_stock = if_else(portfolio_at == 1,  leg_w, -leg_w)
    ) %>%
    ungroup() %>%
    transmute(date, factor = "at_gr1", permno, w_factor_stock)
  
  # ==============================================================================
  # Write standardized outputs
  # ==============================================================================
  if (!dbExistsTable(con, "factor_returns")) {
    dbWriteTable(con, "factor_returns", factor_returns, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_returns WHERE factor = 'at_gr1'")
    dbWriteTable(con, "factor_returns", factor_returns, append = TRUE)
  }
  
  if (!dbExistsTable(con, "factor_holdings")) {
    dbWriteTable(con, "factor_holdings", factor_holdings, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_holdings WHERE factor = 'at_gr1'")
    dbWriteTable(con, "factor_holdings", factor_holdings, append = TRUE)
  }
  
  dbWriteTable(con, "factor_asset_growth", at_factor, overwrite = TRUE)
  
  list(
    returns  = factor_returns,
    holdings = factor_holdings
  )
}