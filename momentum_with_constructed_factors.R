library(tidyverse)
library(rstudioapi)

# -----------------------------
# 1) Locate project root + DB
# -----------------------------
get_project_root <- function() {
  # Works in RStudio; falls back to getwd() if not
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) {
      d <- dirname(p)
      return(sub("/data/?$", "", d))
    }
  }
  normalizePath(getwd(), winslash = "/")
}

project_root <- get_project_root()
DATA_DIR <- file.path(project_root, "data")
DB_PATH  <- file.path(DATA_DIR, "tidy_finance_r.sqlite")

if (!file.exists(DB_PATH)) {
  stop("Missing DB at: ", DB_PATH, "\nRun Scripts/00_build_sqlite_db.R first.")
}

# -----------------------------
# 2) Factor registry
#    IMPORTANT: factor_id is the BENCHMARK ID the script writes (gp_at, at_gr1, etc.)
# -----------------------------
factors_path <- file.path(project_root, "Factors")

execution_indicator <- tribble(
  ~factor_id,        ~execution, ~file,
  "at_gr1",          TRUE,       file.path(factors_path, "asset_growth_sql.R"),
  "ni_me",           TRUE,      file.path(factors_path, "earnings_to_price_sql.R"),
  "fcf_me",          TRUE,      file.path(factors_path, "free_cash_flow_to_price_sql.R"),
  "gp_at",           TRUE,      file.path(factors_path, "gross_profitability_sql.R"),
  "oaccruals_at",    TRUE,      file.path(factors_path, "operating_accruals_sql.R"),
  "sales_me",        TRUE,      file.path(factors_path, "sales_to_price_sql.R")
)

# -----------------------------
# 3) WRDS creds (DON’T hardcode; better ißn ~/.Renviron)
# -----------------------------
# Sys.setenv(WRDS_USER = "your_user")
# Sys.setenv(WRDS_PASSWORD = "your_password")

# -----------------------------
# 4) Run one factor script
# -----------------------------
run_one_factor <- function(factor_id, file, DB_PATH) {
  if (!file.exists(file)) stop("Missing factor script: ", file)
  
  env <- new.env(parent = globalenv())
  source(file, local = env)
  
  if (!exists("build_factor", envir = env)) {
    stop("File ", file, " must define a function named build_factor()")
  }
  
  # Call build_factor from that script, passing DB_PATH
  out <- env$build_factor(DB_PATH = DB_PATH)
  
  # Basic sanity checks (no strategy changes, just validation)
  if (!all(c("returns", "holdings") %in% names(out))) {
    stop("build_factor() in ", file, " must return list(returns=..., holdings=...)")
  }
  
  # Do NOT overwrite factor names (scripts already set benchmark IDs)
  # We add a column to track which script we ran
  out$returns  <- out$returns  %>% mutate(factor_script = factor_id)
  out$holdings <- out$holdings %>% mutate(factor_script = factor_id)
  
  out
}

# -----------------------------
# 5) Run selected factors
# -----------------------------
selected <- execution_indicator %>% filter(execution)

factor_outputs <- selected %>%
  mutate(out = pmap(list(factor_id, file), ~run_one_factor(..1, ..2, DB_PATH)))

all_returns <- factor_outputs %>%
  transmute(returns = map(out, "returns")) %>%
  unnest(returns)

all_holdings <- factor_outputs %>%
  transmute(holdings = map(out, "holdings")) %>%
  unnest(holdings)

# Optional: quick peek
print(all_returns %>% group_by(factor) %>% summarise(n = n(), min_date = min(date), max_date = max(date)))
print(all_holdings %>% group_by(factor) %>% summarise(n = n(), min_date = min(date), max_date = max(date)))


# -----------------------------
exposure <- all_holdings %>%
  group_by(date, factor) %>%
  summarise(
    net   = sum(w_factor_stock, na.rm = TRUE),
    gross = sum(abs(w_factor_stock), na.rm = TRUE),
    long  = sum(pmax(w_factor_stock, 0), na.rm = TRUE),
    short = sum(pmin(w_factor_stock, 0), na.rm = TRUE),
    n_stocks = n(),
    .groups = "drop"
  )

exposure %>% arrange(factor, date) %>% print(n = 30)
exposure %>% group_by(factor) %>%
  summarise(
    avg_net = mean(net), sd_net = sd(net),
    avg_gross = mean(gross), min_gross=min(gross), max_gross=max(gross),
    .groups="drop"
  )

library(dplyr)
library(tidyr)
library(ggplot2)

stopifnot(all(c("date","factor","permno","w_factor_stock") %in% names(all_holdings)))
stopifnot(all(c("date","factor","ret") %in% names(all_returns)))

# -----------------------------
# Latest common month (safer)
# -----------------------------
latest_date <- min(max(all_returns$date, na.rm = TRUE),
                   max(all_holdings$date, na.rm = TRUE))

# -----------------------------
# Gross-only scaling (NO net forcing)
# Each factor-month: sum(abs(w)) = 1
# -----------------------------
scale_factor_holdings_gross_only <- function(df, target_gross = 1) {
  df %>%
    group_by(date, factor) %>%
    mutate(
      w_factor_stock = w_factor_stock * (target_gross / sum(abs(w_factor_stock), na.rm = TRUE))
    ) %>%
    ungroup()
}

all_holdings_std <- scale_factor_holdings_gross_only(all_holdings, target_gross = 1)

K <- n_distinct(all_holdings_std$factor)

# -----------------------------
# Net positions you would HOLD (equal factor capital 1/K)
# -----------------------------
net_positions_latest_equal <- all_holdings_std %>%
  filter(date == latest_date) %>%
  mutate(w = w_factor_stock / K) %>%
  group_by(permno) %>%
  summarise(net_w = sum(w, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(abs(net_w)))

# (Optional) rescale ONLY gross to 1 at the final combined portfolio
# (does NOT force net=0)
net_positions_latest_equal <- net_positions_latest_equal %>%
  mutate(net_w = net_w / sum(abs(net_w), na.rm = TRUE)) %>%
  arrange(desc(abs(net_w)))

# -----------------------------
# 50/50 buckets (TRUE halves)
# -----------------------------
summary_50_50 <- net_positions_latest_equal %>%
  mutate(bucket2 = if_else(ntile(net_w, 2) == 2, "Long", "Short")) %>%
  group_by(bucket2) %>%
  summarise(
    n_stocks = n(),
    net_exposure = sum(net_w),
    gross_exposure = sum(abs(net_w)),
    avg_abs_weight = mean(abs(net_w)),
    .groups = "drop"
  )

print(summary_50_50)

# -----------------------------
# Terciles (1/3, 1/3, 1/3)
# -----------------------------
summary_terciles <- net_positions_latest_equal %>%
  mutate(
    tercile = ntile(net_w, 3),
    bucket3 = case_when(
      tercile == 1 ~ "Short",
      tercile == 2 ~ "Neutral",
      tercile == 3 ~ "Long"
    )
  ) %>%
  group_by(bucket3) %>%
  summarise(
    n_stocks = n(),
    net_exposure = sum(net_w),
    gross_exposure = sum(abs(net_w)),
    avg_abs_weight = mean(abs(net_w)),
    .groups = "drop"
  )

print(summary_terciles)

# -----------------------------
# Plots
# -----------------------------
p_hist <- ggplot(net_positions_latest_equal, aes(x = net_w)) +
  geom_histogram(bins = 60) +
  labs(
    title = "Distribution of Net Stock Weights (Equal-weight Factor Combo)",
    subtitle = paste("Latest month:", as.character(latest_date)),
    x = "Net weight", y = "Count"
  )

p_rank <- net_positions_latest_equal %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(x = rank, y = abs(net_w))) +
  geom_line() +
  labs(
    title = "Concentration of Net Weights",
    subtitle = paste("Latest month:", as.character(latest_date)),
    x = "Rank by |net weight|", y = "|Net weight|"
  )

print(p_hist)
print(p_rank)

# Optional: top positions
topN <- 30
print(head(net_positions_latest_equal, topN))


library(dplyr)
library(ggplot2)

# -----------------------------
# INPUT: net_positions_latest_equal
# must have: permno, net_w
# -----------------------------
stopifnot(all(c("permno","net_w") %in% names(net_positions_latest_equal)))

np <- net_positions_latest_equal %>% filter(is.finite(net_w))

# -----------------------------
# Build three "portfolios"
# -----------------------------

# 1) Long-only sleeve (normalize positives to sum to +1)
long_leg <- np %>%
  filter(net_w > 0) %>%
  mutate(w = net_w / sum(net_w)) %>%
  transmute(permno, portfolio = "Long (normalized)", w)

# 2) Short-only sleeve (normalize negatives to sum to -1)
short_leg <- np %>%
  filter(net_w < 0) %>%
  mutate(w = -abs(net_w) / sum(abs(net_w))) %>%   # sums to -1
  transmute(permno, portfolio = "Short (normalized)", w)

# 3) L-S / net portfolio (your combined portfolio).
# For plotting concentration, it's helpful to gross-normalize (no net-centering, no neutrality forcing).
ls_net <- np %>%
  mutate(w = net_w / sum(abs(net_w))) %>%         # gross = 1; preserves net drift if any
  transmute(permno, portfolio = "L-S (gross=1)", w)

# Combine
port_all <- bind_rows(long_leg, short_leg, ls_net)

# -----------------------------
# (A) Concentration plot: rank vs |weight|
# -----------------------------
plot_rank <- port_all %>%
  group_by(portfolio) %>%
  arrange(desc(abs(w)), .by_group = TRUE) %>%
  mutate(rank = row_number(),
         abs_w = abs(w)) %>%
  ungroup()

p_rank <- ggplot(plot_rank, aes(x = rank, y = abs_w, linetype = portfolio)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Concentration of Weights: Long vs Short vs L–S",
    subtitle = "Long/Short legs normalized separately; L–S gross-normalized (no net forcing)",
    x = "Rank (by |weight|)", y = "|weight|"
  ) +
  theme_minimal()

print(p_rank)

# -----------------------------
# (B) Optional: cumulative concentration (how fast weight accumulates)
# -----------------------------
p_cum <- ggplot(plot_cum, aes(x = rank, y = cum_abs, color = portfolio, linetype = portfolio)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "Long (normalized)"  = "green3",
    "Short (normalized)" = "red3",
    "L-S (gross=1)"      = "blue3"
  )) +
  labs(
    title = "Cumulative Concentration (Cumulative |weight| by rank)",
    x = "Rank by |weight|", y = "Cumulative |weight|"
  ) +
  theme_minimal()

print(p_cum)

# -----------------------------
# Quick summaries (optional)
# -----------------------------
summary_tbl <- port_all %>%
  group_by(portfolio) %>%
  summarise(
    n_stocks = n(),
    net_exposure = sum(w),
    gross_exposure = sum(abs(w)),
    top10_gross = sum(head(sort(abs(w), decreasing = TRUE), 10)),
    .groups = "drop"
  )

print(summary_tbl)

# ============================================================
# COPY/PASTE: Time-series returns for (i) Long-only, (ii) Short-only,
# and (iii) 50/50 Long–Short portfolio built from your net_positions
#
# Requires in memory:
#   DB_PATH (path to SQLite)
#   net_positions  (date, permno, net_w)
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)

# -----------------------------
# 1) Load CRSP monthly returns from SQLite (ret only)
# -----------------------------
con <- dbConnect(SQLite(), DB_PATH)

crsp_ret <- tbl(con, "crsp_monthly") |>
  select(permno, date, ret) |>
  collect()

dbDisconnect(con)

crsp_ret <- crsp_ret |>
  mutate(date = as.Date(date)) |>
  filter(!is.na(date)) |>
  mutate(ret_use = ret)

# -----------------------------
# 2) Merge your net weights with realized returns
# -----------------------------
x <- net_positions |>
  inner_join(crsp_ret |> select(date, permno, ret_use), by = c("date","permno")) |>
  filter(!is.na(ret_use), is.finite(ret_use), !is.na(net_w), is.finite(net_w))

# -----------------------------
# 3) Build Long-only, Short-only, and 50/50 L–S returns each month
#    - Long-only: normalize positive weights to sum to 1
#    - Short-only: normalize negative weights to sum to 1 (as positive notionals),
#                  then SHORT book return = - sum(w_short * ret)
#    - 50/50 L–S: 0.5*LongOnly + 0.5*ShortOnly
# -----------------------------
portfolio_rets <- x |>
  group_by(date) |>
  summarise(
    long_raw  = sum(pmax(net_w, 0), na.rm = TRUE),
    short_raw = sum(-pmin(net_w, 0), na.rm = TRUE),
    
    ret_long = ifelse(
      long_raw > 0,
      sum((pmax(net_w, 0) / long_raw) * ret_use, na.rm = TRUE),
      NA_real_
    ),
    
    ret_short = ifelse(
      short_raw > 0,
      -sum(((-pmin(net_w, 0)) / short_raw) * ret_use, na.rm = TRUE),
      NA_real_
    ),
    
    ret_LS_50_50 = 0.5 * ret_long + 0.5 * ret_short,
    
    # optional: raw net-weight return (your original net_w portfolio)
    ret_LS_raw = sum(net_w * ret_use, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  arrange(date)

# -----------------------------
# 4) Plot monthly + cumulative
# -----------------------------
plot_df <- portfolio_rets |>
  select(date, ret_long, ret_short, ret_LS_50_50) |>
  pivot_longer(-date, names_to = "portfolio", values_to = "ret") |>
  mutate(
    portfolio = recode(portfolio,
                       "ret_long"      = "Long (normalized)",
                       "ret_short"     = "Short (normalized)",
                       "ret_LS_50_50"  = "L-S (50/50 gross=1)"
    )
  ) |>
  group_by(portfolio) |>
  arrange(date) |>
  mutate(cum = cumprod(1 + replace_na(ret, 0)) - 1) |>
  ungroup()

p_monthly <- ggplot(plot_df, aes(x = date, y = ret, color = portfolio)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c(
    "Long (normalized)"   = "green3",
    "Short (normalized)"  = "red3",
    "L-S (50/50 gross=1)" = "blue3"
  )) +
  labs(
    title = "Monthly Returns: Long vs Short vs 50/50 Long–Short",
    x = NULL, y = "Return"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_cum <- ggplot(plot_df, aes(x = date, y = cum, color = portfolio)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(
    "Long (normalized)"   = "green3",
    "Short (normalized)"  = "red3",
    "L-S (50/50 gross=1)" = "blue3"
  )) +
  labs(
    title = "Cumulative Returns: Long vs Short vs 50/50 Long–Short",
    x = NULL, y = "Cumulative return"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_monthly)
print(p_cum)

tail(portfolio_rets, 12)