library(DBI)
library(RSQLite)
library(RPostgres)
library(dplyr)
library(lubridate)

# --- simple project root (works for everyone) ---
ROOT <- dirname(getActiveDocumentContext()$path)

DATA_DIR <- file.path(ROOT, "data")
dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
DB_PATH <- file.path(DATA_DIR, "tidy_finance_r.sqlite")

message("DB_PATH: ", DB_PATH)

# --- WRDS creds ---
u <- Sys.getenv("WRDS_USER")
p <- Sys.getenv("WRDS_PASSWORD")
if (!nzchar(u) || !nzchar(p)) stop("Missing WRDS_USER / WRDS_PASSWORD in env.")

wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = u,
  password = p
)

# -------------------------
# 1) CRSP monthly: msf
# -------------------------
msf <- tbl(wrds, I("crsp.msf")) %>%
  select(permno, date, ret, prc, shrout) %>%
  filter(date >= "1960-01-01") %>%
  collect() %>%
  mutate(date = ymd(date))

# Names/exchange codes (for NYSE breakpoints etc.)
msenames <- tbl(wrds, I("crsp.msenames")) %>%
  select(permno, namedt, nameendt, exchcd, shrcd) %>%
  collect() %>%
  mutate(namedt = ymd(namedt), nameendt = ymd(nameendt))

# CRSP-Compustat linktable (gvkey <-> permno)
linktbl <- tbl(wrds, I("crsp.ccmxpf_linktable")) %>%
  select(gvkey, lpermno, linkdt, linkenddt, linktype, linkprim) %>%
  collect() %>%
  mutate(linkdt = ymd(linkdt),
         linkenddt = ymd(linkenddt))

# -------------------------
# 2) Compustat funda (core columns used by your factors)
# -------------------------
comp_funda_core <- tbl(wrds, I("comp.funda")) %>%
  filter(
    indfmt == "INDL", datafmt == "STD", consol == "C", curcd == "USD",
    datadate >= "1960-01-01"
  ) %>%
  select(gvkey, datadate, at, ni, oancf, capx, ib, dp, sale) %>%   # <- ADD sale
  collect() %>%
  mutate(datadate = ymd(datadate))
dbDisconnect(wrds)

# -------------------------
# Transform CRSP into the shape your scripts expect
# -------------------------
crsp_monthly <- msf %>%
  mutate(prc = abs(prc),
         mktcap = (prc * shrout) / 1000) %>%  # shrout is in thousands => mktcap in $ millions (consistent)
  arrange(permno, date) %>%
  group_by(permno) %>%
  mutate(mktcap_lag = lag(mktcap)) %>%
  ungroup()

# attach exchange/shrcd as-of date
crsp_monthly <- crsp_monthly %>%
  left_join(msenames, by = "permno") %>%
  filter(date >= namedt, date <= nameendt) %>%
  mutate(exchange = case_when(
    exchcd == 1 ~ "NYSE",
    exchcd == 2 ~ "AMEX",
    exchcd == 3 ~ "NASDAQ",
    TRUE ~ "OTHER"
  )) %>%
  select(permno, date, ret, prc, shrout, mktcap, mktcap_lag, exchcd, shrcd, exchange)

# link gvkey (date-valid)
linktbl2 <- linktbl %>%
  filter(linktype %in% c("LC", "LU"), linkprim %in% c("P", "C")) %>%
  mutate(linkenddt = coalesce(linkenddt, ymd("9999-12-31"))) %>%
  rename(permno = lpermno)

crsp_monthly <- crsp_monthly %>%
  left_join(linktbl2, by = "permno") %>%
  filter(is.na(linkdt) | (date >= linkdt & date <= linkenddt)) %>%
  select(permno, gvkey, date, ret, prc, shrout, mktcap, mktcap_lag, exchcd, shrcd, exchange)

# (Optional) excess returns if you have RF elsewhere; for now keep raw ret.
# Your scripts use ret_excess; either add RF table later or compute excess in factor scripts.

# -------------------------
# Write SQLite
# -------------------------
con <- dbConnect(SQLite(), DB_PATH)
on.exit(dbDisconnect(con), add = TRUE)

dbWriteTable(con, "crsp_monthly", crsp_monthly, overwrite = TRUE)
dbWriteTable(con, "comp_funda_core", comp_funda_core, overwrite = TRUE)
dbWriteTable(con, "crsp_ccm_linktable", linktbl2, overwrite = TRUE)

message("DB tables: ", paste(dbListTables(con), collapse = ", "))
