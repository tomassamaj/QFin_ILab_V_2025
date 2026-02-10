# ==============================================================================
# 1. ARNOTT (2023) FACTOR MOMENTUM - SINGLE STRATEGY (1963 START)
# ==============================================================================

# --- LIBRARIES ---
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  data.table,
  arrow,
  lubridate,
  PerformanceAnalytics,
  ggplot2,
  scales,
  gridExtra,
  grid
)

# --- CONFIGURATION ---
START_DATE <- as.Date("1963-01-01")
DATA_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs"
VOL_TARGET <- 0.10

# --- DATA LOADING ---
cat("\n[1/5] Loading Data...\n")
pfs_file <- file.path(DATA_DIR, "pfs_daily.parquet")
if (!file.exists(pfs_file)) {
  stop("Data not found!")
}

dt_raw <- read_parquet(pfs_file) %>%
  filter(excntry == "USA") %>%
  select(date, characteristic, pf, ret = ret_vw_cap) %>%
  collect() %>%
  setDT()

# --- FACTOR CONSTRUCTION ---
cat("[2/5] Constructing Factors & Applying Sign Corrections...\n")
dt_factors <- dt_raw[,
  .(
    ret_long = ret[pf == max(pf)],
    ret_short = ret[pf == min(pf)]
  ),
  by = .(date, characteristic)
]

dt_factors[, factor_ret := ret_long - ret_short]

# Sign Corrections (Hardcoded list)
factors_to_flip <- c(
  "betabab_1260d",
  "market_equity",
  "beta_60m",
  "ivol_ff3_21d",
  "age",
  "prc",
  "ret_1_0",
  "sale_gr1",
  "at_gr1",
  "at_be",
  "ret_60_12",
  "chcsho_12m",
  "netis_at",
  "o_score",
  "capx_gr1",
  "oaccruals_at",
  "dolvol_126d",
  "inv_gr1",
  "cowc_gr1a",
  "capex_abn",
  "dbnetis_at",
  "noa_at"
)

dt_factors[characteristic %in% factors_to_flip, factor_ret := factor_ret * -1]

# --- SIGNAL GENERATION ---
cat("[3/5] Generating Monthly Signals...\n")
dt_factors[, month := floor_date(date, "month")]

# 1-Month Momentum Signal
dt_monthly_sig <- dt_factors[,
  .(
    mom_signal = sum(log(1 + factor_ret))
  ),
  by = .(month, characteristic)
]

# Calculate Weights (Top 50% L/S)
dt_weights <- dt_monthly_sig[,
  .(
    characteristic,
    # Compare signal against cross-section of THAT MONTH
    weight_ls = fcase(
      mom_signal >= quantile(mom_signal, 0.50, na.rm = TRUE) ,  1 ,
      mom_signal < quantile(mom_signal, 0.50, na.rm = TRUE)  , -1 ,
      default = 0
    )
  ),
  by = .(month)
]

# Normalize to Dollar Neutral
dt_weights[, weight_ls := weight_ls / sum(abs(weight_ls)), by = month]

# Lag Weights (Signal T -> Trade T+1)
dt_weights[, trade_month := month + months(1)]

# --- BACKTEST ---
cat("[4/5] Running Daily Backtest (Start: 1963)...\n")
dt_bt <- merge(
  dt_factors,
  dt_weights[, .(trade_month, characteristic, weight_ls)],
  by.x = c("month", "characteristic"),
  by.y = c("trade_month", "characteristic"),
  all.x = TRUE
)

# 1. Remove NAs
dt_bt <- dt_bt[!is.na(weight_ls)]

# 2. FILTER START DATE (Crucial Step)
dt_bt <- dt_bt[date >= START_DATE]

# 3. Execution Lag (Day 1 Cash Rule)
dt_bt[, is_trade_day := date == min(date), by = month]
dt_bt[is_trade_day == TRUE, weight_ls := 0]

# 4. Aggregate
strategy_ts <- dt_bt[,
  .(
    ret_daily = sum(weight_ls * factor_ret, na.rm = TRUE)
  ),
  by = date
][order(date)]

# --- REPORTING (FIXED) ---
cat("[5/5] Generating Plots...\n")
xts_ret <- xts(strategy_ts$ret_daily, order.by = strategy_ts$date)
colnames(xts_ret) <- "FactorMomentum"

# Plot Data Setup
df_plot <- data.frame(
  Date = index(xts_ret),
  Return = as.numeric(coredata(xts_ret))
)
df_plot$Cumulative <- cumprod(1 + df_plot$Return)

# MANUAL DRAWDOWN CALCULATION (Bypasses library error)
# Peak = Max value seen so far
df_plot$Peak <- cummax(df_plot$Cumulative)
# Drawdown = (Current / Peak) - 1
df_plot$Drawdown <- (df_plot$Cumulative / df_plot$Peak) - 1

# Charts
p1 <- ggplot(df_plot, aes(x = Date, y = Cumulative)) +
  geom_line(color = "#2c3e50") +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Factor Momentum (1963-Present)", y = "Log Wealth")

p2 <- ggplot(df_plot, aes(x = Date, y = Drawdown)) +
  geom_area(fill = "red", alpha = 0.5) +
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  labs(title = "Drawdowns", y = "%")

grid.arrange(p1, p2, ncol = 1)

# Stats
cat("\n--- PERFORMANCE (1963 START) ---\n")
print(table.AnnualizedReturns(xts_ret))
print(maxDrawdown(xts_ret))


# ==============================================================================
# 2. STRATEGY GRID SEARCH (FIXED GROUPING + 1963 FILTER)
# ==============================================================================

# Define Grid
param_grid <- expand.grid(
  position_type = c("long_short", "long_only"),
  top_pct = c(0.25, 0.33, 0.50),
  stringsAsFactors = FALSE
)

results_list <- list()
metrics_df <- data.frame()
START_DATE <- as.Date("1963-01-01")

# --- FUNCTION DEFINITION ---
run_strategy_fixed <- function(position_type, top_pct) {
  # Percentile Logic
  long_thresh <- 1 - top_pct
  short_thresh <- top_pct

  # 1. CALCULATE WEIGHTS (The Fix: by = .(month) ONLY)
  if (position_type == "long_short") {
    dt_w <- dt_monthly_sig[,
      .(
        characteristic,
        weight = fcase(
          mom_signal >= quantile(mom_signal, long_thresh, na.rm = TRUE)  ,  1 ,
          mom_signal <= quantile(mom_signal, short_thresh, na.rm = TRUE) , -1 ,
          default = 0
        )
      ),
      by = .(month)
    ] # <--- FIXED: No 'characteristic' here

    # Normalize (Sum Abs = 2 -> Scale to 1)
    dt_w[, weight := weight / sum(abs(weight)), by = month]
  } else {
    # Long Only
    dt_w <- dt_monthly_sig[,
      .(
        characteristic,
        weight = fifelse(
          mom_signal >= quantile(mom_signal, long_thresh, na.rm = TRUE),
          1,
          0
        )
      ),
      by = .(month)
    ] # <--- FIXED

    # Normalize (Sum = 1)
    dt_w[, weight := weight / sum(weight, na.rm = TRUE), by = month]
  }

  # 2. LAG & MERGE
  dt_w[, trade_month := month + months(1)]

  dt_btest <- merge(
    dt_factors,
    dt_w[, .(trade_month, characteristic, weight)],
    by.x = c("month", "characteristic"),
    by.y = c("trade_month", "characteristic"),
    all.y = TRUE # Inner join is safer for speed
  )

  # 3. FILTER DATE & CLEAN
  dt_btest <- dt_btest[!is.na(weight) & weight != 0]
  dt_btest <- dt_btest[date >= START_DATE] # <--- 1963 FILTER

  # 4. EXECUTION LAG (Day 1 = 0)
  dt_btest[, is_trade_day := date == min(date), by = month]
  # Keep the rows, but set the weight (and thus return) to 0
  dt_btest[is_trade_day == TRUE, weight := 0]

  # 5. AGGREGATE
  ts <- dt_btest[, .(ret = sum(weight * factor_ret, na.rm = TRUE)), by = date]
  setorder(ts, date)

  # 6. RETURN OBJECT
  if (nrow(ts) > 0) {
    x <- xts(ts$ret, order.by = ts$date)
    return(list(
      name = paste0(position_type, "_", top_pct * 100, "%"),
      xts = x,
      ann_ret = Return.annualized(x),
      sharpe = SharpeRatio.annualized(x),
      dd = maxDrawdown(x)
    ))
  } else {
    return(NULL)
  }
}

# --- EXECUTION LOOP ---
cat("\n[GRID] Running 1963-Present Variations...\n")

for (i in 1:nrow(param_grid)) {
  res <- run_strategy_fixed(param_grid$position_type[i], param_grid$top_pct[i])
  results_list[[i]] <- res

  # Add to summary table
  metrics_df <- rbind(
    metrics_df,
    data.frame(
      Strategy = res$name,
      Ann_Ret = sprintf("%.2f%%", res$ann_ret * 100),
      Sharpe = sprintf("%.2f", res$sharpe),
      Max_DD = sprintf("%.2f%%", res$dd * 100)
    )
  )
  cat(".")
}

# --- VISUALIZATION ---
cat("\n\n=== RESULTS (1963-Present) ===\n")
print(metrics_df)

# Combine for Plot
all_rets <- lapply(results_list, function(item) {
  df <- data.frame(
    Date = index(item$xts),
    Cumulative = cumprod(1 + coredata(item$xts))
  )
  df$Strategy <- item$name
  return(df)
}) %>%
  bind_rows()

ggplot(all_rets, aes(x = Date, y = Cumulative, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Factor Momentum: Grid Comparison (1963+)", y = "Log Wealth")


# ==============================================================================
# 3. SINGLE STOCK PORTFOLIO CONSTRUCTION (NETTING)
# ==============================================================================
# PURPOSE: Convert Factor-Level Weights -> Single Stock Weights
# STRATEGY: Long-Only Top 50% (Based on your grid results)
# ==============================================================================

# --- CONFIGURATION ---
TARGET_STRATEGY_PCT <- 0.25 # Top 50%
START_DATE <- as.Date("1963-01-01") # Ensure this matches your backtest start!
POSITION_TYPE <- "long_only" # "long_only" or "long_short"
WEIGHTS_FILE <- file.path(DATA_DIR, "/usa_factor_weights.parquet") # Adjust path if needed!

# --- 1. RECOVER FACTOR WEIGHTS (FROM BEST STRATEGY) ---
cat("\n[1/4] Re-generating Factor Weights for Target Strategy...\n")

# Re-run the logic for the specific target strategy to get the factor weights
long_thresh <- 1 - TARGET_STRATEGY_PCT

if (POSITION_TYPE == "long_only") {
  dt_target_weights <- dt_monthly_sig[,
    .(
      characteristic,
      factor_weight = fifelse(
        mom_signal >= quantile(mom_signal, long_thresh, na.rm = TRUE),
        1,
        0
      )
    ),
    by = .(month)
  ]

  # Normalize Factor Weights (Sum = 1)
  dt_target_weights[,
    factor_weight := factor_weight / sum(factor_weight, na.rm = TRUE),
    by = month
  ]
}

# Shift to Trade Month (Signal Jan -> Trade Feb)
dt_target_weights[, trade_month := month + months(1)]
dt_target_weights <- dt_target_weights[factor_weight != 0] # Optimization: Drop zero-weight factors

# --- 2. LOAD SINGLE STOCK CONSTITUENTS ---
cat("[2/4] Loading Single Stock Weights (This may take a moment)...\n")
# We need to map: Trade Month -> Factor -> Stock -> Weight
# Note: Ensure 'usa_factor_weights.parquet' exists.
# If you only have daily pfs, we might need to infer weights, but usually weights are monthly.

if (!file.exists(WEIGHTS_FILE)) {
  stop(paste("CRITICAL: Weights file not found at", WEIGHTS_FILE))
}

dt_constituents <- read_parquet(WEIGHTS_FILE) %>%
  select(id, eom, characteristic, stock_weight = weight, leg) %>% # Rename for clarity
  collect() %>%
  setDT()

# Ensure dates are Date objects
dt_constituents[, month := floor_date(eom, "month")]
# Shift Constituent Data:
# Weights are usually "end of month" for trading "next month".
# So 'eom' Jan 31 is the portfolio for Feb.
dt_constituents[, trade_month := floor_date(eom, "month") + months(1)]

# --- 3. EXPLODE & NETTING ---
cat("[3/4] Exploding Factors to Stocks & Netting Positions...\n")

# Merge: Factor Strategy Weights (dt_target_weights) + Stock Constituents (dt_constituents)
# Join on: trade_month, characteristic
dt_portfolio <- merge(
  dt_constituents,
  dt_target_weights[, .(trade_month, characteristic, factor_weight)],
  by = c("trade_month", "characteristic"),
  all.y = TRUE # Keep the Strategy Factors
)

# Filter: We only care about the factors currently in our strategy
dt_portfolio <- dt_portfolio[!is.na(stock_weight)]

dt_portfolio <- dt_portfolio[trade_month >= START_DATE] # Ensure we only keep relevant dates
# APPLY SIGN CORRECTIONS TO CONSTITUENTS
# If a factor was flipped (e.g. Price), we must flip the underlying stock weights too?
# LOGIC CHECK:
# If Factor Return = Long - Short, and we flipped it to Short - Long (-1),
# Then "Buying the Factor" means Buying the Short Leg and Selling the Long Leg.
# So yes, we multiply stock_weight by the flip sign.

# 1. Identify Flip Factors
factors_to_flip <- c(
  "betabab_1260d",
  "market_equity",
  "beta_60m",
  "ivol_ff3_21d",
  "age",
  "prc",
  "ret_1_0",
  "sale_gr1",
  "at_gr1",
  "at_be",
  "ret_60_12",
  "chcsho_12m",
  "netis_at",
  "o_score",
  "capx_gr1",
  "oaccruals_at",
  "dolvol_126d",
  "inv_gr1",
  "cowc_gr1a",
  "capex_abn",
  "dbnetis_at",
  "noa_at"
)

# 2. Apply Flip to Stock Weights
dt_portfolio[
  characteristic %in% factors_to_flip,
  stock_weight := stock_weight * -1
]

# CALCULATE FINAL NET WEIGHT
# Net Weight = Sum ( Factor_Weight_in_Strategy * Stock_Weight_in_Factor )
dt_portfolio[, net_weight := factor_weight * stock_weight]

# AGGREGATE BY STOCK ID
final_stock_portfolio <- dt_portfolio[,
  .(total_weight = sum(net_weight, na.rm = TRUE)),
  by = .(trade_month, id)
]

# Clean up: Remove tiny residuals (floating point errors) and zeros
final_stock_portfolio <- final_stock_portfolio[abs(total_weight) > 1e-6]

# --- 4. ANALYSIS OF FINAL PORTFOLIO ---
cat("[4/4] Analyzing Single Stock Portfolio Characteristics...\n")

# 1. Number of Stocks per Month
stats_n_stocks <- final_stock_portfolio[, .(n_stocks = .N), by = trade_month]

# 2. Gross Exposure (Leverage)
stats_gross_exp <- final_stock_portfolio[,
  .(gross_exp = sum(abs(total_weight))),
  by = trade_month
]

# 3. Top 10 Holdings (Latest Month)
latest_date <- max(final_stock_portfolio$trade_month)
top_holdings <- final_stock_portfolio[trade_month == latest_date][order(
  -total_weight
)] %>%
  head(10)

# --- OUTPUT ---
print(head(top_holdings))
print(summary(stats_n_stocks$n_stocks))

# Plot Number of Stocks over time
ggplot(stats_n_stocks, aes(x = trade_month, y = n_stocks)) +
  geom_area(fill = "#2980b9", alpha = 0.6) +
  labs(
    title = "Number of Stocks in Factor Momentum Portfolio",
    y = "Count",
    x = ""
  ) +
  theme_minimal()

# OPTIONAL: SAVE TO PARQUET FOR EXECUTION
write_parquet(
  final_stock_portfolio,
  file.path(DATA_DIR, "final_strategy_single_stocks.parquet")
)
cat("\nDone! 'final_stock_portfolio' contains the trade list.\n")


# ==============================================================================
# 4. ENRICH PORTFOLIO WITH WRDS METADATA (TICKERS & NAMES) - LOCAL JOIN FIX
# ==============================================================================

# --- LIBRARIES ---
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, data.table, DBI, RPostgres, arrow)

# --- 1. CONNECT TO WRDS ---
cat("\n[1/4] Connecting to WRDS...\n")

wrds <- tryCatch(
  {
    dbConnect(
      Postgres(),
      host = "wrds-pgdata.wharton.upenn.edu",
      port = 9737,
      dbname = "wrds",
      user = Sys.getenv("WRDS_USER"),
      password = Sys.getenv("WRDS_PASSWORD"),
      sslmode = "require"
    )
  },
  error = function(e) {
    stop(
      "Failed to connect to WRDS. Check your internet connection and credentials."
    )
  }
)

# --- 2. FETCH NAMES (CRSP.MSENAMES) ---
cat("[2/4] Querying CRSP Names & Tickers (Download & Local Filter)...\n")

# STRATEGY CHANGE: Instead of uploading a temp table (which failed),
# we download the relevant columns from msenames and filter locally.
# msenames is relatively small, so this is safe.

# Extract unique PERMNOs from your portfolio
unique_permnos <- unique(final_stock_portfolio$id)

# Query: Get all names.
# Optimization: If the table is too big, we can filter by date at least.
names_query <- tbl(
  wrds,
  sql(
    "SELECT permno, namedt, nameendt, ticker, comnam, ncusip FROM crsp.msenames"
  )
)

# Download to R
df_names <- names_query %>% collect() %>% setDT()

# Disconnect immediately
dbDisconnect(wrds)

# --- 3. FILTER LOCALLY ---
cat("[3/4] Filtering and Merging Metadata...\n")

# Filter for only the PERMNOs in our portfolio
df_names <- df_names[permno %in% unique_permnos]

# Ensure dates are proper Date objects
df_names[, namedt := as.Date(namedt)]
df_names[, nameendt := as.Date(nameendt)]
final_stock_portfolio[, trade_month := as.Date(trade_month)]

# Perform Non-Equi Join (Range Join)
# Logic: Stock ID matches PERMNO, and Trade Month falls between Name Start/End dates
dt_enriched <- df_names[
  final_stock_portfolio,
  on = .(permno = id, namedt <= trade_month, nameendt >= trade_month),
  .(
    trade_month = i.trade_month,
    permno = i.id,
    weight = i.total_weight, # from portfolio
    ticker = x.ticker,
    company_name = x.comnam,
    ncusip = x.ncusip
  ),
  nomatch = NULL
] # Drop rows if no name match

# Reorder columns
setcolorder(
  dt_enriched,
  c("trade_month", "permno", "ticker", "company_name", "weight")
)

# Sort
setorder(dt_enriched, trade_month, -weight)

# Preview
print(head(dt_enriched, 10))

# --- 4. SAVE FINAL FILE ---
cat("[4/4] Saving Final Enriched Portfolio...\n")

OUTPUT_FILE <- file.path(
  DATA_DIR,
  "final_strategy_single_stocks_enriched.parquet"
)
write_parquet(dt_enriched, OUTPUT_FILE)

cat(sprintf("\nSUCCESS! Saved to: %s\n", OUTPUT_FILE))


# ==============================================================================
# 6. MONTHLY BACKTEST (WITH OUTLIER CLEANING)
# ==============================================================================

# --- LIBRARIES ---
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  data.table,
  arrow,
  lubridate,
  ggplot2,
  PerformanceAnalytics,
  scales,
  gridExtra
)

# --- CONFIGURATION ---
DATA_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs"
PORTFOLIO_FILE <- file.path(
  DATA_DIR,
  "final_strategy_single_stocks_enriched.parquet"
)
RETURNS_FILE <- file.path(DATA_DIR, "Global_Stock_Returns_Monthly.parquet")

# --- 1. LOAD DATA ---
cat("\n[1/5] Loading Data...\n")
dt_port <- read_parquet(PORTFOLIO_FILE)
setDT(dt_port)
dt_port[, abs_weight := abs(weight)]
dt_port[, join_month := floor_date(trade_month, "month")]

# Check if portfolio is empty
if (nrow(dt_port) == 0) {
  stop("Portfolio file is empty!")
}

# Load Returns
unique_ids <- unique(dt_port$permno)
dt_rets <- read_parquet(RETURNS_FILE) %>%
  filter(excntry == "USA") %>%
  filter(id %in% unique_ids) %>%
  select(id, eom, ret = ret_exc) %>%
  collect() %>%
  setDT()

dt_rets[, join_month := floor_date(eom, "month")]

# --- 2. DIAGNOSTIC: FIND THE "DATA BOMBS" ---
cat("[2/5] Inspecting Data Quality...\n")
# Look for returns > 1000% (10.0)
bombs <- dt_rets[ret > 10.0]
if (nrow(bombs) > 0) {
  cat(sprintf(
    "WARNING: Found %d 'Data Bombs' (Returns > 1000%%)!\n",
    nrow(bombs)
  ))
  print(head(bombs[order(-ret)], 5))
}

# --- 3. CLEANING (CRITICAL FIX) ---
cat("[3/5] Cleaning Extreme Outliers...\n")
# We clip monthly returns at +300% (3.0) and -95% (-0.95).
# This is standard practice to prevent penny stock errors from ruining equal-weighted backtests.
initial_count <- nrow(dt_rets)
dt_rets <- dt_rets[ret < 10.0 & ret > -10.0]
removed <- initial_count - nrow(dt_rets)
cat(sprintf("Removed %d extreme return observations.\n", removed))

# --- 4. ROBUST BACKTEST FUNCTION ---
run_monthly_backtest <- function(threshold_bps) {
  threshold <- threshold_bps / 10000

  # A. Filter Portfolio Weights
  dt_trim <- dt_port[abs_weight >= threshold]
  if (nrow(dt_trim) == 0) {
    return(NULL)
  }

  # B. Merge (Inner Join to ensure we only have tradable assets)
  # We assume we rebalance monthly.
  dt_bt <- merge(
    dt_trim[, .(permno, join_month, weight)],
    dt_rets[, .(id, join_month, ret)],
    by.x = c("permno", "join_month"),
    by.y = c("id", "join_month"),
    all.x = FALSE, # INNER JOIN: Drop stocks with missing returns
    all.y = FALSE
  )

  # C. Daily Re-Normalization (The "100% Invested" Logic)
  # 1. Sum the weights of the stocks we actually found returns for
  ts_monthly <- dt_bt[,
    .(
      raw_weighted_ret = sum(weight * ret, na.rm = TRUE),
      total_found_weight = sum(weight, na.rm = TRUE)
    ),
    by = join_month
  ]

  # 2. Filter out months with bad data coverage
  # If we found less than 50% of our target portfolio, skip the month (or treat as cash)
  # Here we re-scale: Return = Raw_Ret / Total_Weight
  ts_monthly <- ts_monthly[total_found_weight > 0.5]

  ts_monthly[, ret := raw_weighted_ret / total_found_weight]

  setorder(ts_monthly, join_month)
  return(ts_monthly)
}

# --- 5. RUN SIMULATIONS ---
cat("[4/5] Running Simulations...\n")

ts_full <- run_monthly_backtest(0)
ts_trim_1bp <- run_monthly_backtest(1)
ts_trim_5bps <- run_monthly_backtest(5)

# Combine
if (!is.null(ts_full)) {
  ts_full[, Strategy := "Full Portfolio"]
}
if (!is.null(ts_trim_1bp)) {
  ts_trim_1bp[, Strategy := "Trim < 1bp"]
}
if (!is.null(ts_trim_5bps)) {
  ts_trim_5bps[, Strategy := "Trim < 5bps"]
}

results_list <- list(ts_full, ts_trim_1bp, ts_trim_5bps)
results_list <- results_list[!sapply(results_list, is.null)]
df_compare <- rbindlist(results_list)

# --- 6. REPORTING ---
cat("[5/5] Generating Report...\n")

if (nrow(df_compare) > 0) {
  # Cumulative Plot
  df_compare[, Cumulative := cumprod(1 + ret), by = Strategy]

  p1 <- ggplot(
    df_compare,
    aes(x = join_month, y = Cumulative, color = Strategy)
  ) +
    geom_line(linewidth = 1) +
    scale_y_log10(labels = comma) +
    labs(
      title = "Factor Momentum: Monthly Single Stock Replication",
      subtitle = "Cleaned Data (Returns Clipped at +300% / -95%)",
      y = "Cumulative Wealth (Log)",
      x = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p1)

  # Stats Table
  stats_table <- df_compare %>%
    group_by(Strategy) %>%
    summarise(
      Ann_Return = Return.annualized(xts(ret, order.by = join_month)),
      Vol = StdDev.annualized(xts(ret, order.by = join_month)),
      Sharpe = SharpeRatio.annualized(xts(ret, order.by = join_month)),
      Max_DD = maxDrawdown(xts(ret, order.by = join_month))
    )
  print(stats_table)
} else {
  cat("No valid data generated.\n")
}

# load cutoff files
nyse <- read_parquet(file.path(DATA_DIR, "nyse_cutoffs.parquet"))
ret_cutoffs <- read_parquet(file.path(DATA_DIR, "return_cutoffs.parquet"))
ret_daily_cutoffs <- read_parquet(file.path(
  DATA_DIR,
  "return_cutoffs_daily.parquet"
))


# do str and head on the cutoffs with lapply
cat("\n--- NYSE Cutoffs ---\n")
print(str(nyse))
print(head(nyse))
cat("\n--- Monthly Return Cutoffs ---\n")
print(str(ret_cutoffs))
print(head(ret_cutoffs))

cat("\n--- Daily Return Cutoffs ---\n")
print(str(ret_daily_cutoffs))
print(head(ret_daily_cutoffs))


# ==============================================================================
# 8. DAILY BACKTEST WITH DYNAMIC JKP CUTOFFS
# ==============================================================================
# PURPOSE: Use official JKP cutoffs to clean daily returns and filter microcaps.
# ==============================================================================

# --- LIBRARIES ---
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  data.table,
  arrow,
  lubridate,
  ggplot2,
  PerformanceAnalytics,
  scales,
  gridExtra
)

# --- CONFIGURATION ---
DATA_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs"
PORTFOLIO_FILE <- file.path(
  DATA_DIR,
  "final_strategy_single_stocks_enriched.parquet"
)
STOCK_RET_FILE <- file.path(DATA_DIR, "USA_daily_rets.parquet")
# New Cutoff Files
DAILY_CUTOFFS <- file.path(DATA_DIR, "return_cutoffs_daily.parquet")
NYSE_CUTOFFS <- file.path(DATA_DIR, "nyse_cutoffs.parquet")

# --- 1. LOAD & PREPARE PORTFOLIO ---
cat("\n[1/6] Loading Portfolio Weights...\n")
dt_port <- read_parquet(PORTFOLIO_FILE)
setDT(dt_port)
dt_port[, abs_weight := abs(weight)]

# --- 2. LOAD CUTOFF FILES ---
cat("[2/6] Loading Cutoff Definitions...\n")
dt_limits_daily <- read_parquet(DAILY_CUTOFFS) %>% setDT()
dt_limits_nyse <- read_parquet(NYSE_CUTOFFS) %>% setDT()

# Prepare Daily Limits for Merge (Year-Month Key)
# The file has 'year' and 'month' columns. We need to match daily returns to this.
# (No processing needed, we will create year/month in returns)

# --- 3. LOAD & CLEAN DAILY RETURNS (THE FIX) ---
cat("[3/6] Loading & Cleaning Daily Returns...\n")
unique_permnos <- unique(dt_port$permno)

dt_rets <- read_parquet(STOCK_RET_FILE) %>%
  select(permno = id, date, ret = ret_exc) %>%
  filter(permno %in% unique_permnos) %>%
  collect() %>%
  setDT()

# Add Time Keys for Merging Limits
dt_rets[, year := year(date)]
dt_rets[, month := month(date)]

# MERGE 1: Attach Dynamic Return Limits (0.1% and 99.9%)
dt_rets <- merge(
  dt_rets,
  dt_limits_daily[, .(
    year,
    month,
    min_ret = ret_exc_0_1,
    max_ret = ret_exc_99_9
  )],
  by = c("year", "month"),
  all.x = TRUE
)

# FILTER 1: Dynamic Outlier Removal
# We keep only returns that are within the historical 0.1% - 99.9% band
# This handles the "1929 Volatility" differently than "2010 Volatility"
initial_N <- nrow(dt_rets)
dt_rets <- dt_rets[ret >= min_ret & ret <= max_ret]
cat(sprintf(
  "   > Removed %d outliers based on dynamic cutoffs.\n",
  initial_N - nrow(dt_rets)
))

# MERGE 2: NYSE Size Filter (Optional but Recommended)
# Arnott et al use NYSE breakpoints. We can filter stocks smaller than NYSE 20th percentile.
# Note: We need Market Cap (me) for this. If daily 'me' is missing, skip or load monthly me.
# Assuming we proceed with just Return Cleaning for now.

# --- 4. ROBUST BACKTEST FUNCTION (DAILY RE-NORMALIZATION) ---
run_cleaned_backtest <- function(threshold_bps) {
  threshold <- threshold_bps / 10000

  # A. Filter Portfolio
  dt_trim <- dt_port[abs_weight >= threshold]
  if (nrow(dt_trim) == 0) {
    return(NULL)
  }

  # B. Normalize Weights (Target Sum = 1.0)
  dt_trim[, weight := weight / sum(weight, na.rm = TRUE), by = trade_month]

  # C. Merge Portfolio with Cleaned Returns
  dt_rets[, trade_month := floor_date(date, "month")] # Align dates

  dt_bt <- merge(
    dt_rets[, .(permno, date, ret)], # Cleaned returns only
    dt_trim[, .(permno, trade_month, weight)],
    by.x = "permno",
    by.y = "permno",
    all.x = FALSE,
    all.y = FALSE,
    allow.cartesian = TRUE # Safe now because we filter by month next
  )

  # Ensure Date matches Trade Month
  dt_bt <- dt_bt[floor_date(date, "month") == trade_month]

  # D. Daily Re-Normalization
  # Calculate return on "Available Capital"
  ts_daily <- dt_bt[,
    .(
      gross_ret = sum(weight * ret, na.rm = TRUE),
      found_weight = sum(weight, na.rm = TRUE)
    ),
    by = date
  ]

  # Filter days with bad coverage (<50% of portfolio found)
  ts_daily <- ts_daily[found_weight > 0.5]

  # Scale to 100% Investment
  ts_daily[, ret := gross_ret / found_weight]
  setorder(ts_daily, date)

  return(ts_daily)
}

# --- 5. RUN SIMULATIONS ---
cat("[4/6] Running Simulations with Cleaned Data...\n")

ts_full <- run_cleaned_backtest(0)
ts_trim_1bp <- run_cleaned_backtest(1)
ts_trim_5bps <- run_cleaned_backtest(5)

# Combine
if (!is.null(ts_full)) {
  ts_full[, Strategy := "Full Portfolio"]
}
if (!is.null(ts_trim_1bp)) {
  ts_trim_1bp[, Strategy := "Trim < 1bp"]
}
if (!is.null(ts_trim_5bps)) {
  ts_trim_5bps[, Strategy := "Trim < 5bps"]
}

results_list <- list(ts_full, ts_trim_1bp, ts_trim_5bps)
results_list <- results_list[!sapply(results_list, is.null)]
df_compare <- rbindlist(results_list)

# --- 6. REPORTING ---
cat("[5/6] Generating Final Report...\n")

if (nrow(df_compare) > 0) {
  # Cumulative Plot
  df_compare[, Cumulative := cumprod(1 + ret), by = Strategy]

  p1 <- ggplot(df_compare, aes(x = date, y = Cumulative, color = Strategy)) +
    geom_line(linewidth = 0.8) +
    scale_y_log10(labels = comma) +
    labs(
      title = "Factor Momentum: Single Stock Replication (Cleaned)",
      subtitle = "Dynamic Outlier Filtering (0.1% - 99.9% Historical Quantiles)",
      y = "Cumulative Wealth (Log)",
      x = ""
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p1)

  # Stats Table
  stats_table <- df_compare %>%
    group_by(Strategy) %>%
    summarise(
      Ann_Return = Return.annualized(xts(ret, order.by = date)),
      Vol = StdDev.annualized(xts(ret, order.by = date)),
      Sharpe = SharpeRatio.annualized(xts(ret, order.by = date)),
      Max_DD = maxDrawdown(xts(ret, order.by = date))
    )
  print(stats_table)
} else {
  cat("No valid data generated.\n")
}


# ==============================================================================
# 9. GOLD STANDARD MONTHLY BACKTEST (CORRECTED)
# ==============================================================================

# --- LIBRARIES ---
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  data.table,
  arrow,
  lubridate,
  ggplot2,
  PerformanceAnalytics,
  scales,
  gridExtra
)

# --- CONFIGURATION ---
DATA_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs"
PORTFOLIO_FILE <- file.path(
  DATA_DIR,
  "final_strategy_single_stocks_enriched.parquet"
)
RETURNS_FILE <- file.path(DATA_DIR, "Global_Stock_Returns_Monthly.parquet")
CUTOFFS_FILE <- file.path(DATA_DIR, "return_cutoffs.parquet")

# --- 1. LOAD PORTFOLIO WEIGHTS ---
cat("\n[1/6] Loading Portfolio Weights...\n")
dt_port <- read_parquet(PORTFOLIO_FILE)
setDT(dt_port)
# We only use the SIGN from the factor strategy (+1 or -1)
# The magnitude will be determined by Market Cap
dt_port[, signal_sign := sign(weight)]
dt_port[, join_month := floor_date(trade_month, "month")]

# --- 2. LOAD & PREPARE MARKET DATA ---
cat("[2/6] Loading Monthly Returns & Calculating Lagged Cap...\n")
unique_ids <- unique(dt_port$permno)

dt_rets <- read_parquet(RETURNS_FILE) %>%
  filter(excntry == "USA") %>%
  # We load 'me' (Market Cap) and 'ret_exc'
  select(id, eom, me, ret = ret_exc) %>%
  collect() %>%
  setDT()

dt_rets[, join_month := floor_date(eom, "month")]

# *** CRITICAL FIX: LAG MARKET CAP ***
setorder(dt_rets, id, join_month)
dt_rets[, me_lag := shift(me, 1, type = "lag"), by = id]

# Filter out rows where we don't have a lagged cap (can't weight them)
dt_rets <- dt_rets[!is.na(me_lag) & me_lag > 0]

# --- 3. DYNAMIC CLEANING (JKP METHOD) ---
cat("[3/6] Applying Dynamic Outlier Filters...\n")
dt_limits <- read_parquet(CUTOFFS_FILE) %>% setDT()
dt_limits[, join_month := floor_date(eom, "month")]

dt_rets <- merge(
  dt_rets,
  dt_limits[, .(join_month, min_ret = ret_exc_0_1, max_ret = ret_exc_99_9)],
  by = "join_month",
  all.x = TRUE
)

# Apply Filter
initial_N <- nrow(dt_rets)
dt_rets <- dt_rets[ret >= min_ret & ret <= max_ret]
cat(sprintf(
  "   > Removed %d outliers using dynamic cutoffs.\n",
  initial_N - nrow(dt_rets)
))

# --- 4. VALUE-WEIGHTED BACKTEST FUNCTION ---
run_vw_backtest <- function(threshold_bps, name) {
  threshold <- threshold_bps / 10000

  # A. Filter Portfolio Signal (Remove weak factor signals)
  dt_trim <- dt_port[abs(weight) >= threshold]
  if (nrow(dt_trim) == 0) {
    return(NULL)
  }

  # B. Merge Signal with Market Data
  dt_bt <- merge(
    dt_trim[, .(permno, join_month, signal_sign)],
    dt_rets[, .(id, join_month, ret, me_lag)], # Use me_lag!
    by.x = c("permno", "join_month"),
    by.y = c("id", "join_month"),
    all.x = FALSE,
    all.y = FALSE
  )

  # C. Calculate Value Weights (Split by Long/Short)
  # Weight = (LaggedCap / TotalLaggedCap of Leg)
  dt_bt[, leg_cap := sum(me_lag, na.rm = TRUE), by = .(join_month, signal_sign)]
  dt_bt[, vw_weight := (me_lag / leg_cap) * signal_sign]

  # D. Aggregate Active Returns (50/50 L/S)
  ts_active <- dt_bt[,
    .(
      ret_active = sum((vw_weight * 0.5) * ret, na.rm = TRUE),
      n_stocks = .N
    ),
    by = join_month
  ]

  ts_active[, Strategy := name]
  return(ts_active)
}

# --- 5. RUN SIMULATION ---
cat("[4/6] Running Active Strategy...\n")
ts_active <- run_vw_backtest(1, "Factor Momentum (Active L/S)")

# --- 6. CREATE SMART BETA (MARKET + ACTIVE) ---
cat("[5/6] Constructing Long-Only Smart Beta Portfolio...\n")

# A. Calculate Benchmark (S&P Total Market)
# We use the full universe of cleaned returns, Cap-Weighted
dt_rets[, total_mkt_cap := sum(me_lag, na.rm = TRUE), by = join_month]
dt_rets[, w_bench := me_lag / total_mkt_cap]
ts_bench <- dt_rets[,
  .(ret_bench = sum(w_bench * ret, na.rm = TRUE)),
  by = join_month
]

# B. Combine
ts_final <- merge(
  ts_bench,
  ts_active[, .(join_month, ret_active)],
  by = "join_month",
  all.x = TRUE
)
ts_final[is.na(ret_active), ret_active := 0]

# C. Calculate Strategies
# 1. Benchmark (Passive)
# 2. Smart Beta (100% Market + 100% Factor Momentum)
ts_final[, `Benchmark` := ret_bench]
ts_final[, `Smart Beta (FM)` := ret_bench + ret_active]

# --- 7. REPORTING ---
cat("[6/6] Final Report...\n")

xts_res <- xts(
  ts_final[, .(Benchmark, `Smart Beta (FM)`)],
  order.by = ts_final$join_month
)

# Stats
print(table.AnnualizedReturns(xts_res))
print(maxDrawdown(xts_res))

# Plot
df_plot <- data.frame(Date = index(xts_res), coredata(xts_res))
df_plot$Cum_Bench <- cumprod(1 + df_plot$Benchmark)
df_plot$Cum_Smart <- cumprod(1 + df_plot$Smart.Beta..FM.)

ggplot(df_plot, aes(x = Date)) +
  geom_line(
    aes(y = Cum_Bench, color = "Benchmark (Market)"),
    linewidth = 0.8,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = Cum_Smart, color = "Smart Beta (Market + FM)"),
    linewidth = 1
  ) +
  scale_y_log10(labels = comma) +
  scale_color_manual(
    values = c(
      "Benchmark (Market)" = "black",
      "Smart Beta (Market + FM)" = "#2980b9"
    )
  ) +
  labs(
    title = "Factor Momentum: Real-World Implementation",
    subtitle = "Market Cap Weighted | Dynamic Outlier Cleaning | 1bp Trim",
    y = "Cumulative Wealth (Log)",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
