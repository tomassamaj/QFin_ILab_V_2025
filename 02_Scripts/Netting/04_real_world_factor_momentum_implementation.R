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
