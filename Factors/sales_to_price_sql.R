# ==============================================================================
# FACTOR BUILDER: Sales-to-Price (sale_me) — DB-only
# Barbee, Mukherji, Raines (1996) | Signal: SALE / ME(Dec t-1) | Long Q5 - Short Q1
# Construction identical; SQLite-only data source + July-1 alignment fix.
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
  if (!file.exists(DB_PATH)) stop("Missing DB: ", DB_PATH)
  
  con <- dbConnect(SQLite(), DB_PATH, extended_types = TRUE)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # ---- Load CRSP monthly ----
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
  
  # Use ret_excess if present; else fallback to ret (no construction change, just availability)
  if ("ret_excess" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret_excess)
  } else if ("ret" %in% names(crsp_monthly)) {
    crsp_monthly <- crsp_monthly |> mutate(ret_use = ret)
  } else {
    stop("crsp_monthly must contain ret_excess or ret.")
  }
  
  # ---- Load Compustat SALE ----
  comp_sales <- tbl(con, "comp_funda_core") |>
    select(any_of(c("gvkey","datadate","sale"))) |>
    collect() |>
    mutate(datadate = as_date_any(datadate)) |>
    filter(!is.na(datadate))
  
  if (!("sale" %in% names(comp_sales))) {
    stop("comp_funda_core missing column: sale (SALE).")
  }
  
  # ==============================================================================
  # 2) Construct Signal: SALE / ME(Dec t-1)  -- IDENTICAL logic
  # ==============================================================================
  sales_signal <- comp_sales |>
    select(gvkey, datadate, sale) |>
    mutate(sorting_date = as.Date(paste0(year(datadate) + 1, "-07-01"))) |>
    filter(!is.na(sale), is.finite(sale), sale > 0)
  
  dec_market_cap <- crsp_monthly |>
    filter(month(date) == 12) |>
    select(permno, gvkey, mktcap_dec = mktcap, date) |>
    mutate(sorting_date = as.Date(paste0(year(date) + 1, "-07-01"))) |>
    select(-date)
  
  sp_signal <- sales_signal |>
    inner_join(dec_market_cap, by = c("gvkey", "sorting_date")) |>
    mutate(sale_me = sale / mktcap_dec) |>
    filter(!is.na(sale_me), is.finite(sale_me)) |>
    select(permno, gvkey, sorting_date, sale_me)
  
  # ==============================================================================
  # 3) Merge with June Market Data for Sorting
  # FIX: June is month-end; June+1 month != July 1. Force July 1.
  # ==============================================================================
  sorting_data <- crsp_monthly |>
    filter(month(date) == 6) |>
    mutate(sorting_date = as.Date(paste0(year(date), "-07-01"))) |>
    select(permno, gvkey, exchange, sorting_date, mktcap_june = mktcap) |>
    inner_join(sp_signal, by = c("permno","gvkey","sorting_date"))
  
  stopifnot("permno" %in% names(sorting_data))
  
  # ==============================================================================
  # 4) NYSE breakpoints quintiles  -- IDENTICAL logic
  # ==============================================================================
  assign_sp_portfolio <- function(data) {
    nyse_breaks <- data |>
      filter(exchange == "NYSE") |>
      pull(sale_me) |>
      quantile(probs = seq(0, 1, 0.2), na.rm = TRUE, type = 7)
    
    data |>
      mutate(portfolio = findInterval(sale_me, nyse_breaks, all.inside = TRUE))
  }
  
  portfolios_formed <- sorting_data |>
    group_by(sorting_date) |>
    group_split() |>
    purrr::map_dfr(assign_sp_portfolio) |>
    select(permno, sorting_date, portfolio_sp = portfolio)
  
  # ==============================================================================
  # 5) Factor returns: VW Q5 - Q1  -- IDENTICAL logic
  # ==============================================================================
  sp_factor <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno","sorting_date")) |>
    group_by(date, portfolio_sp) |>
    summarize(ret = weighted.mean(ret_use, mktcap_lag, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = portfolio_sp, values_from = ret, names_prefix = "Q") |>
    mutate(sale_me_replicated = Q5 - Q1) |>
    select(date, sale_me_replicated)
  
  factor_returns <- sp_factor |>
    transmute(date, factor = "sale_me", ret = sale_me_replicated)
  
  # ==============================================================================
  # Holdings: Long Q5, Short Q1, VW within each leg (gross not normalized)
  # ==============================================================================
  joined <- crsp_monthly |>
    mutate(sorting_date = case_when(
      month(date) <= 6 ~ as.Date(paste0(year(date) - 1, "-07-01")),
      month(date) >= 7 ~ as.Date(paste0(year(date), "-07-01"))
    )) |>
    inner_join(portfolios_formed, by = c("permno","sorting_date"))
  
  factor_holdings <- joined |>
    filter(portfolio_sp %in% c(1, 5)) |>
    group_by(date, portfolio_sp) |>
    mutate(
      leg_w = mktcap_lag / sum(mktcap_lag, na.rm = TRUE),
      w_factor_stock = if_else(portfolio_sp == 5, leg_w, -leg_w)
    ) |>
    ungroup() |>
    transmute(date, factor = "sale_me", permno, w_factor_stock)
  
  # ==============================================================================
  # Write standardized outputs (append-safe)
  # ==============================================================================
  if (!dbExistsTable(con, "factor_returns")) {
    dbWriteTable(con, "factor_returns", factor_returns, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_returns WHERE factor = 'sale_me'")
    dbWriteTable(con, "factor_returns", factor_returns, append = TRUE)
  }
  
  if (!dbExistsTable(con, "factor_holdings")) {
    dbWriteTable(con, "factor_holdings", factor_holdings, overwrite = TRUE)
  } else {
    dbExecute(con, "DELETE FROM factor_holdings WHERE factor = 'sale_me'")
    dbWriteTable(con, "factor_holdings", factor_holdings, append = TRUE)
  }
  
  # Optional per-factor table (like your older scripts)
  dbWriteTable(con, "factor_sales_price", sp_factor, overwrite = TRUE)
  
  list(returns = factor_returns, holdings = factor_holdings)
}