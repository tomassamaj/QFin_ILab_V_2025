# =============================================================================
#  STANDARD FRICTIONS ANALYSIS
# =============================================================================

# Libraries loading
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  arrow,
  tidyverse,
  data.table,
  lubridate,
  PerformanceAnalytics,
  scales,
  ggplot2
)

# --- 1. CONFIGURATION ---
MASTER_FILE <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs/arnott_master.parquet"
USA_FILE <- "/Users/farkastallos/USA.parquet"

# A. Transaction Cost Parameters
TC_BASE_BPS <- 5
TC_SLOPE_BPS <- 1
TC_ALPHA <- 0.25
TC_MAX_BPS <- 100

# B. Shorting Cost Parameters
SC_BASE_BPS <- 25
SC_SLOPE_BPS <- 10
SC_ALPHA <- 0.5
SC_MAX_BPS <- 500

# C. Tax Parameters
TAX_RATE <- 0.275 # 27.5% Withholding Tax on Dividends

# --- 2. LOAD & PREP DATA ---
dt <- read_parquet(MASTER_FILE) %>% as.data.table()
dt[, eom := as.Date(eom)]

usa_raw_1 <- read_parquet(USA_FILE) %>% as.data.table()
colnames <- colnames(usa_raw_1)
print("Available columns in USA.parquet:")
print(colnames)
# Load 'me' AND 'div1m_me'
usa_data <- read_parquet(
  USA_FILE,
  col_select = c("id", "date", "me", "div1m_me")
) %>%
  as.data.table()

# Map usa_data columns to dt using a rolling join.
dt[, `:=`(
  me = usa_data[dt, on = .(id, date = eom), roll = "nearest", x.me],
  div1m_me = usa_data[dt, on = .(id, date = eom), roll = "nearest", x.div1m_me]
)]

# --- 3. CALCULATE FRICTION RATES ---
dt[,
  ref_mcap := {
    top_500 <- head(sort(me, decreasing = TRUE), 500)
    median(top_500, na.rm = TRUE)
  },
  by = eom
]

# TC Rate
dt[,
  tc_bps := pmin(
    TC_MAX_BPS,
    TC_BASE_BPS + TC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^TC_ALPHA
  )
]
dt[is.na(tc_bps), tc_bps := quantile(tc_bps, 0.90, na.rm = TRUE), by = eom]

# SC Rate
dt[,
  sc_annual_bps := pmin(
    SC_MAX_BPS,
    SC_BASE_BPS + SC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^SC_ALPHA
  )
]
dt[
  is.na(sc_annual_bps),
  sc_annual_bps := quantile(sc_annual_bps, 0.90, na.rm = TRUE),
  by = eom
]

# --- 4. CALCULATE COSTS ---

# --- A. Transaction Costs (Turnover) ---
w_curr <- dt[, .(eom, id, w_t = weight, tc_bps)]
w_prev <- dt[, .(eom = eom %m+% months(1), id, w_prev = weight)]

trades_dt <- merge(w_curr, w_prev, by = c("eom", "id"), all = TRUE)
trades_dt[is.na(w_t), c("w_t", "tc_bps") := .(0, NA)]
trades_dt[is.na(w_prev), w_prev := 0]

avg_costs <- trades_dt[, .(avg_tc = mean(tc_bps, na.rm = TRUE)), by = eom]
trades_dt[avg_costs, on = "eom", avg_tc := i.avg_tc]
trades_dt[is.na(tc_bps), tc_bps := avg_tc]

trades_dt[, tc_cost := abs(w_t - w_prev) * (tc_bps / 10000)]
monthly_tc <- trades_dt[, .(drag_tc = sum(tc_cost, na.rm = TRUE)), by = eom]

# --- B. Shorting Costs (Borrow Fee) ---
dt[, sc_cost := 0]
dt[weight < 0, sc_cost := abs(weight) * (sc_annual_bps / 10000) / 12]
monthly_sc <- dt[, .(drag_sc = sum(sc_cost, na.rm = TRUE)), by = eom]

# --- C. Tax Costs (Dividend Withholding on Shorts) ---
dt[, tax_cost := 0]
dt[weight < 0 & div1m_me > 0, tax_cost := abs(weight) * div1m_me * TAX_RATE]
monthly_tax <- dt[, .(drag_tax = sum(tax_cost, na.rm = TRUE)), by = eom]


# --- 5. AGGREGATE PERFORMANCE ---
perf_ts <- dt[,
  .(
    ret_standard = sum(weight * ret_exc_lead1m, na.rm = TRUE),
    ret_gross_lagged = sum(weight * (ret_exc_lead1m - ret_day1), na.rm = TRUE)
  ),
  by = eom
][order(eom)]

# Merge all friction layers
perf_ts <- merge(perf_ts, monthly_tc, by = "eom", all.x = TRUE)
perf_ts <- merge(perf_ts, monthly_sc, by = "eom", all.x = TRUE)
perf_ts <- merge(perf_ts, monthly_tax, by = "eom", all.x = TRUE)

# Fill NAs
perf_ts[is.na(drag_tc), drag_tc := 0]
perf_ts[is.na(drag_sc), drag_sc := 0]
perf_ts[is.na(drag_tax), drag_tax := 0]

# Calculate Cumulative Nets (Layered)
perf_ts[, `:=`(
  ret_net_1_tc = ret_gross_lagged - drag_tc,
  ret_net_2_sc = ret_gross_lagged - drag_tc - drag_sc,
  ret_net_3_tax = ret_gross_lagged - drag_tc - drag_sc - drag_tax
)]

# --- 6. PLOTTING ---
# Calculate Wealth Indices
perf_ts[, `:=`(
  wealth_standard = cumprod(1 + replace(ret_standard, is.na(ret_standard), 0)),
  wealth_lagged = cumprod(
    1 + replace(ret_gross_lagged, is.na(ret_gross_lagged), 0)
  ),
  wealth_net_tc = cumprod(1 + replace(ret_net_1_tc, is.na(ret_net_1_tc), 0)),
  wealth_net_sc = cumprod(1 + replace(ret_net_2_sc, is.na(ret_net_2_sc), 0)),
  wealth_final = cumprod(1 + replace(ret_net_3_tax, is.na(ret_net_3_tax), 0))
)]

# Print Stats
cat("\n=== FRICTION DECOMPOSITION (Avg Annual Drag) ===\n")
cat("Transaction Costs: ", percent(mean(perf_ts$drag_tc) * 12), "\n")
cat("Shorting Costs:    ", percent(mean(perf_ts$drag_sc) * 12), "\n")
cat("Dividend Tax:      ", percent(mean(perf_ts$drag_tax) * 12), "\n")

# Visualization
ggplot(perf_ts, aes(x = eom)) +

  # Layer 1: Transaction Costs (Orange)
  geom_ribbon(
    aes(
      ymin = wealth_net_tc,
      ymax = wealth_lagged,
      fill = "1. Transaction Costs"
    ),
    alpha = 0.5
  ) +

  # Layer 2: Shorting Costs (Red)
  geom_ribbon(
    aes(ymin = wealth_net_sc, ymax = wealth_net_tc, fill = "2. Shorting Costs"),
    alpha = 0.5
  ) +

  # Layer 3: Tax Costs (Purple)
  geom_ribbon(
    aes(ymin = wealth_final, ymax = wealth_net_sc, fill = "3. Dividend Tax"),
    alpha = 0.5
  ) +

  # Lines
  geom_line(
    aes(y = wealth_standard, linetype = "Theoretical"),
    color = "grey60"
  ) +
  geom_line(aes(y = wealth_lagged, color = "Gross (Lagged)"), size = 1) +
  geom_line(aes(y = wealth_final, color = "Net (Final)"), size = 1) +

  # Styling
  scale_fill_manual(
    name = "Friction Layers",
    values = c(
      "1. Transaction Costs" = "#FDBF6F",
      "2. Shorting Costs" = "#FB9A99",
      "3. Dividend Tax" = "#CAB2D6"
    )
  ) +

  scale_color_manual(
    name = "Performance",
    values = c(
      "Gross (Lagged)" = "#1F78B4",
      "Net (Final)" = "black"
    )
  ) +

  scale_linetype_manual(
    name = "Reference",
    values = c("Theoretical" = "dashed")
  ) +

  labs(
    title = "Strategy Performance: Decomposition of Frictions",
    subtitle = "Cumulative wealth lost to Transaction Costs, Borrow Fees, and Dividend Taxes",
    y = "Wealth Index (Log Scale)",
    x = "Date"
  ) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "right")

# --- 7. PERFORMANCE TABLE ---

# 1. Create an xts object with the 3 specific series
xts_final <- xts(
  perf_ts[, .(ret_standard, ret_gross_lagged, ret_net_3_tax)],
  order.by = perf_ts$eom
)

# 2. Rename columns for the table header
colnames(xts_final) <- c(
  "Original (T-Close)",
  "Lagged (T+1)",
  "Net (All Frictions)"
)

# 3. Print the table (Scale = 12 for monthly data)
cat("\n=== FINAL STRATEGY METRICS ===\n")
print(table.AnnualizedReturns(xts_final, scale = 12))


# =============================================================================
#  DRIFT-BAND REBALANCING ANALYSIS
# =============================================================================

# Thresholds to test (in basis points)
THRESHOLDS_BPS <- c(0, 5, 10, 15, 20, 30, 50)

simulate_drift_band <- function(dt_all, threshold_bps) {
  # dt_all : data.table with columns: eom, id, weight (target), ret_exc_lead1m,
  #          ret_day1, tc_bps, sc_annual_bps, div1m_me
  # Returns : data.table with monthly results: eom, ret_gross, ret_gross_lagged,
  #           turnover, tc_drag, sc_drag, tax_drag

  setkey(dt_all, eom)
  months <- sort(unique(dt_all$eom))

  id_type <- class(dt_all$id)
  holdings <- data.table(id = vector(id_type, 0), weight = numeric())
  results_list <- vector("list", length(months))

  for (i in seq_along(months)) {
    m <- months[i]
    month_data <- dt_all[.(m)] # target weights & other data for month m

    # Merge with previous actual holdings
    month_data <- merge(
      month_data,
      holdings,
      by = "id",
      all.x = TRUE,
      sort = FALSE
    )
    setnames(month_data, c("weight.x", "weight.y"), c("target", "prev_actual"))
    month_data[is.na(prev_actual), prev_actual := 0]

    # Drift-band rule
    thresh <- threshold_bps / 10000
    trade <- abs(month_data$target - month_data$prev_actual) > thresh |
      month_data$prev_actual == 0
    month_data[, actual := ifelse(trade, target, prev_actual)]

    # Portfolio returns (using actual weights)
    port_ret <- sum(month_data$actual * month_data$ret_exc_lead1m, na.rm = TRUE)
    port_ret_lag <- sum(
      month_data$actual * (month_data$ret_exc_lead1m - month_data$ret_day1),
      na.rm = TRUE
    )

    # Turnover (one‑way sum of absolute weight changes)
    turnover <- sum(
      abs(month_data$actual - month_data$prev_actual),
      na.rm = TRUE
    )

    # Transaction costs on traded stocks
    month_data[,
      tc_drag_stock := ifelse(
        trade,
        2 * (tc_bps / 10000) * abs(target - prev_actual),
        0
      )
    ]
    tc_drag <- sum(month_data$tc_drag_stock, na.rm = TRUE)

    # Shorting costs
    month_data[,
      sc_drag_stock := ifelse(
        actual < 0,
        abs(actual) * (sc_annual_bps / 10000) / 12,
        0
      )
    ]
    sc_drag <- sum(month_data$sc_drag_stock, na.rm = TRUE)

    # Tax drag on dividends for short positions
    month_data[,
      tax_drag_stock := ifelse(
        actual < 0 & div1m_me > 0,
        abs(actual) * div1m_me * TAX_RATE,
        0
      )
    ]
    tax_drag <- sum(month_data$tax_drag_stock, na.rm = TRUE)

    # Store month results
    results_list[[i]] <- data.table(
      eom = m,
      ret_gross = port_ret,
      ret_gross_lagged = port_ret_lag,
      turnover = turnover,
      tc_drag = tc_drag,
      sc_drag = sc_drag,
      tax_drag = tax_drag
    )

    # Update holdings for next month (only non‑zero actual weights)
    holdings <- month_data[actual != 0, .(id, weight = actual)]
  }

  rbindlist(results_list)
}

# Run simulations for all thresholds
cat("\n=============================================================\n")
cat("Running drift‑band simulations...\n")
cat("=============================================================\n")

drift_results_list <- list()
for (th in THRESHOLDS_BPS) {
  cat(sprintf("  Threshold = %d bps ... ", th))
  t_start <- proc.time()
  res <- simulate_drift_band(dt, th)
  elapsed <- (proc.time() - t_start)["elapsed"]
  cat(sprintf("done in %.1f sec\n", elapsed))
  drift_results_list[[as.character(th)]] <- res
}

# Combine all threshold results
drift_all <- rbindlist(drift_results_list, idcol = "threshold")
drift_all[, threshold := as.numeric(threshold)]

# Compute annualised performance metrics per threshold
summary_drift <- drift_all[,
  .(
    ann_ret = mean(ret_gross_lagged - tc_drag - sc_drag - tax_drag) * 12,
    ann_vol = sd(ret_gross_lagged - tc_drag - sc_drag - tax_drag) * sqrt(12),
    sharpe = mean(ret_gross_lagged - tc_drag - sc_drag - tax_drag) /
      sd(ret_gross_lagged - tc_drag - sc_drag - tax_drag) *
      sqrt(12),
    avg_turnover = mean(turnover),
    tc_drag_ann = mean(tc_drag) * 12,
    sc_drag_ann = mean(sc_drag) * 12,
    tax_drag_ann = mean(tax_drag) * 12
  ),
  by = threshold
][order(threshold)]

cat("\n=== DRIFT-BAND PERFORMANCE SUMMARY ===\n")
print(summary_drift)

# Export to CSV
fwrite(summary_drift, "drift_band_summary.csv")
cat("\nSaved: drift_band_summary.csv\n")

# --- PLOTS ---

# Convert to plain data.frame for ggplot and ensure unique names / correct types
summary_df <- as.data.frame(summary_drift)
if (any(duplicated(names(summary_df)))) {
  names(summary_df) <- make.names(names(summary_df), unique = TRUE)
}
# Normalize key columns used in plots
# ensure 'threshold' exists and is numeric
if (!"threshold" %in% names(summary_df)) {
  thr <- grep(
    "^threshold",
    names(summary_df),
    ignore.case = TRUE,
    value = TRUE
  )[1]
  summary_df$threshold <- as.numeric(summary_df[[thr]])
} else {
  summary_df$threshold <- as.numeric(summary_df$threshold)
}
# ensure 'sharpe' exists and is numeric
if (!"sharpe" %in% names(summary_df)) {
  sh <- grep("^sharpe", names(summary_df), ignore.case = TRUE, value = TRUE)[1]
  summary_df$sharpe <- as.numeric(summary_df[[sh]])
} else {
  summary_df$sharpe <- as.numeric(summary_df$sharpe)
}

# Ensure no duplicate column names and correct types before plotting
dups <- names(summary_df)[duplicated(names(summary_df))]
if (length(dups) > 0) {
  message(
    "Duplicate column names found: ",
    paste(dups, collapse = ", "),
    " — making names unique"
  )
  names(summary_df) <- make.names(names(summary_df), unique = TRUE)
}
# Recompute dependent objects from the cleaned summary_df
summary_df$threshold <- as.numeric(summary_df$threshold)
summary_df$sharpe <- as.numeric(summary_df$sharpe)
max_point <- summary_df[which.max(summary_df$sharpe), , drop = FALSE]

# 1. Net Sharpe ratio vs threshold
p1 <- ggplot(summary_df, aes(x = threshold, y = sharpe)) +
  geom_line(color = "#1a2e5a", linewidth = 1.2) +
  geom_point(size = 3, color = "#1e50a2") +
  geom_point(data = max_point, color = "#c8a951", size = 5, shape = 18) +
  geom_label(
    aes(label = paste0(threshold, " bps")),
    vjust = -0.8,
    size = 3,
    color = "#1a2e5a",
    label.size = 0.2,
    fill = "white"
  ) +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  labs(
    x = "Drift‑band threshold (bps)",
    y = "Net Sharpe Ratio",
    title = "Drift‑band rebalancing: Net Sharpe vs threshold",
    subtitle = "CS_LO_25, 12M lookback | 1963–2024"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#1a2e5a"))

print(p1)
ggsave("drift_sharpe_vs_threshold.png", p1, width = 7, height = 4, dpi = 150)

# 2. Turnover and TC drag (dual axis)
p2 <- summary_df %>%
  select(threshold, Turnover = avg_turnover, TC = tc_drag_ann) %>%
  pivot_longer(
    cols = c(Turnover, TC),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = ifelse(
      Metric == "Turnover",
      "Avg monthly turnover",
      "TC drag (% p.a.)"
    )
  ) %>%
  ggplot(aes(x = threshold, y = Value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  scale_color_manual(
    values = c(
      "Avg monthly turnover" = "#1e50a2",
      "TC drag (% p.a.)" = "#c8a951"
    )
  ) +
  labs(
    x = "Drift‑band threshold (bps)",
    y = "Value",
    title = "Turnover and transaction cost decline with threshold",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p2)
ggsave("drift_turnover_TC.png", p2, width = 7, height = 4, dpi = 150)

# 3. Cumulative wealth for baseline and a few alternatives
wealth_data <- drift_all[,
  .(
    eom,
    threshold,
    wealth = cumprod(1 + (ret_gross_lagged - tc_drag - sc_drag - tax_drag))
  ),
  by = threshold
]
wealth_data <- wealth_data[threshold %in% c(0, 5, 10, 20)]
wealth_data <- as.data.frame(wealth_data) # convert to plain data.frame

p3 <- ggplot(wealth_data, aes(x = eom, y = wealth, color = factor(threshold))) +
  geom_line() +
  scale_y_log10(labels = scales::dollar) +
  labs(
    x = "Date",
    y = "Wealth (log scale)",
    title = "Cumulative wealth for selected thresholds",
    color = "Threshold (bps)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)
ggsave("drift_wealth_comparison.png", p3, width = 8, height = 5, dpi = 150)

cat("\n=== DRIFT-BAND ANALYSIS COMPLETE ===\n")

# --- Cumulative wealth for custom threshold selection ---

# Define which thresholds to display
selected_thresholds <- c(0, 15, 30, 50)

# 1. Ensure data is sorted by date before calculating cumulative product
setorder(drift_all, threshold, eom)

# 2. Filter first, then compute cumulative wealth
wealth_selected <- drift_all[
  threshold %in% selected_thresholds,
  .(
    eom,
    wealth = cumprod(1 + (ret_gross_lagged - tc_drag - sc_drag - tax_drag))
  ),
  by = threshold
]

# Convert to data.frame for ggplot
wealth_selected <- as.data.frame(wealth_selected)

# 3. Create the plot
p_extra <- ggplot(
  wealth_selected,
  aes(x = eom, y = wealth, color = factor(threshold))
) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::dollar) +
  labs(
    x = "Date",
    y = "Cumulative Wealth (log scale)",
    title = "Drift-Band Rebalancing: Cumulative Wealth",
    subtitle = paste(
      "Selected thresholds:",
      paste(selected_thresholds, collapse = ", "),
      "bps"
    ),
    color = "Threshold (bps)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", color = "#1a2e5a")
  )

print(p_extra)
ggsave(
  "drift_wealth_custom_selection.png",
  p_extra,
  width = 8,
  height = 5,
  dpi = 150
)
cat("\nSaved: drift_wealth_custom_selection.png\n")

# --- 6. VISUALIZATION ---
ggplot(perf_ts, aes(x = eom)) +

  # Layer 1: Transaction Costs (Orange)
  geom_ribbon(
    aes(
      ymin = wealth_net_tc,
      ymax = wealth_lagged,
      fill = "1. Transaction Costs"
    ),
    alpha = 0.5
  ) +

  # Layer 2: Shorting Costs (Red)
  geom_ribbon(
    aes(ymin = wealth_net_sc, ymax = wealth_net_tc, fill = "2. Shorting Costs"),
    alpha = 0.5
  ) +

  # Layer 3: Tax Costs (Purple)
  geom_ribbon(
    aes(ymin = wealth_final, ymax = wealth_net_sc, fill = "3. Dividend Tax"),
    alpha = 0.5
  ) +

  # Lines
  geom_line(
    aes(y = wealth_standard, linetype = "Theoretical"),
    color = "grey60"
  ) +
  geom_line(aes(y = wealth_lagged, color = "Gross (Lagged)"), size = 1) +
  geom_line(aes(y = wealth_final, color = "Net (Final)"), size = 1) +

  # 5 bps Drift Band Line
  geom_line(aes(y = wealth_net_5bps, color = "Net (5bps Drift)"), size = 1.2) +

  # Styling
  scale_fill_manual(
    name = "Friction Layers",
    values = c(
      "1. Transaction Costs" = "#FDBF6F",
      "2. Shorting Costs" = "#FB9A99",
      "3. Dividend Tax" = "#CAB2D6"
    )
  ) +

  scale_color_manual(
    name = "Performance",
    values = c(
      "Gross (Lagged)" = "#1F78B4",
      "Net (Final)" = "black",
      "Net (5bps Drift)" = "#c8a951"
    )
  ) +

  scale_linetype_manual(
    name = "Reference",
    values = c("Theoretical" = "dashed")
  ) +

  labs(
    title = "Strategy Performance: Decomposition of Frictions",
    subtitle = "Cumulative wealth lost to Frictions, and recovery via 5 bps Drift Band Rebalancing",
    y = "Wealth Index (Log Scale)",
    x = "Date"
  ) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "right")


# --- 7. PERFORMANCE TABLE ---

# 1. Create an xts object with the 4 specific series
xts_final <- xts(
  perf_ts[, .(ret_standard, ret_gross_lagged, ret_net_3_tax, ret_net_5bps)],
  order.by = perf_ts$eom
)

# 2. Rename columns for the table header
colnames(xts_final) <- c(
  "Original (T-Close)",
  "Lagged (T+1)",
  "Net (0bps Rebal)",
  "Net (5bps Drift)"
)

# 3. Print the table (Scale = 12 for monthly data)
cat("\n=== FINAL STRATEGY METRICS ===\n")
print(PerformanceAnalytics::table.AnnualizedReturns(xts_final, scale = 12))

# =============================================================================
#  Factor Momentum: Baseline Frictions & Drift-Band Rebalancing Analysis
# =============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  arrow,
  tidyverse,
  data.table,
  lubridate,
  PerformanceAnalytics,
  scales,
  ggplot2
)


# =============================================================================
# 1. LOAD & PREP DATA
# =============================================================================
cat("[1/5] Loading and prepping data...\n")
dt <- read_parquet(MASTER_FILE) %>% as.data.table()
dt[, eom := as.Date(eom)]

usa_data <- read_parquet(
  USA_FILE,
  col_select = c("id", "date", "me", "div1m_me")
) %>%
  as.data.table()

# Rolling join to update 'dt' by reference without row duplication
dt[, `:=`(
  me = usa_data[dt, on = .(id, date = eom), roll = "nearest", x.me],
  div1m_me = usa_data[dt, on = .(id, date = eom), roll = "nearest", x.div1m_me]
)]

# =============================================================================
# 2. CALCULATE BASELINE FRICTION RATES
# =============================================================================
cat("[2/5] Calculating baseline friction rates...\n")

# Top 500 Market Cap Reference
dt[,
  ref_mcap := {
    top_500 <- head(sort(me, decreasing = TRUE), 500)
    median(top_500, na.rm = TRUE)
  },
  by = eom
]

# TC Rate
dt[,
  tc_bps := pmin(
    TC_MAX_BPS,
    TC_BASE_BPS + TC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^TC_ALPHA
  )
]
dt[is.na(tc_bps), tc_bps := quantile(tc_bps, 0.90, na.rm = TRUE), by = eom]

# SC Rate
dt[,
  sc_annual_bps := pmin(
    SC_MAX_BPS,
    SC_BASE_BPS + SC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^SC_ALPHA
  )
]
dt[
  is.na(sc_annual_bps),
  sc_annual_bps := quantile(sc_annual_bps, 0.90, na.rm = TRUE),
  by = eom
]

# =============================================================================
# 3. CALCULATE BASELINE COSTS (0 BPS DRIFT)
# =============================================================================
cat("[3/5] Computing baseline costs and performance...\n")

# A. Transaction Costs (Turnover)
w_curr <- dt[, .(eom, id, w_t = weight, tc_bps)]
w_prev <- dt[, .(eom = eom %m+% months(1), id, w_prev = weight)]

trades_dt <- merge(w_curr, w_prev, by = c("eom", "id"), all = TRUE)
trades_dt[is.na(w_t), c("w_t", "tc_bps") := .(0, NA)]
trades_dt[is.na(w_prev), w_prev := 0]

avg_costs <- trades_dt[, .(avg_tc = mean(tc_bps, na.rm = TRUE)), by = eom]
trades_dt[avg_costs, on = "eom", avg_tc := i.avg_tc]
trades_dt[is.na(tc_bps), tc_bps := avg_tc]

trades_dt[, tc_cost := abs(w_t - w_prev) * (tc_bps / 10000)]
monthly_tc <- trades_dt[, .(drag_tc = sum(tc_cost, na.rm = TRUE)), by = eom]

# B. Shorting Costs & C. Tax Costs
dt[, sc_cost := 0]
dt[weight < 0, sc_cost := abs(weight) * (sc_annual_bps / 10000) / 12]
monthly_sc <- dt[, .(drag_sc = sum(sc_cost, na.rm = TRUE)), by = eom]

dt[, tax_cost := 0]
dt[weight < 0 & div1m_me > 0, tax_cost := abs(weight) * div1m_me * TAX_RATE]
monthly_tax <- dt[, .(drag_tax = sum(tax_cost, na.rm = TRUE)), by = eom]

# Aggregate Baseline Performance
perf_ts <- dt[,
  .(
    ret_standard = sum(weight * ret_exc_lead1m, na.rm = TRUE),
    ret_gross_lagged = sum(weight * (ret_exc_lead1m - ret_day1), na.rm = TRUE)
  ),
  by = eom
][order(eom)]

perf_ts <- merge(perf_ts, monthly_tc, by = "eom", all.x = TRUE)
perf_ts <- merge(perf_ts, monthly_sc, by = "eom", all.x = TRUE)
perf_ts <- merge(perf_ts, monthly_tax, by = "eom", all.x = TRUE)

perf_ts[is.na(drag_tc), drag_tc := 0]
perf_ts[is.na(drag_sc), drag_sc := 0]
perf_ts[is.na(drag_tax), drag_tax := 0]

# Calculate Layered Nets
perf_ts[, `:=`(
  ret_net_1_tc = ret_gross_lagged - drag_tc,
  ret_net_2_sc = ret_gross_lagged - drag_tc - drag_sc,
  ret_net_3_tax = ret_gross_lagged - drag_tc - drag_sc - drag_tax
)]

# =============================================================================
# 4. DRIFT-BAND REBALANCING SIMULATION
# =============================================================================
cat("[4/5] Running drift-band simulations...\n")

THRESHOLDS_BPS <- c(0, 5, 10, 15, 20, 30, 50)

simulate_drift_band <- function(dt_all, threshold_bps) {
  setkey(dt_all, eom)
  months <- sort(unique(dt_all$eom))

  id_type <- class(dt_all$id)
  holdings <- data.table(id = vector(id_type, 0), weight = numeric())
  results_list <- vector("list", length(months))

  for (i in seq_along(months)) {
    m <- months[i]
    month_data <- dt_all[.(m)]

    month_data <- merge(
      month_data,
      holdings,
      by = "id",
      all.x = TRUE,
      sort = FALSE
    )
    setnames(month_data, c("weight.x", "weight.y"), c("target", "prev_actual"))
    month_data[is.na(prev_actual), prev_actual := 0]

    thresh <- threshold_bps / 10000
    trade <- abs(month_data$target - month_data$prev_actual) > thresh |
      month_data$prev_actual == 0
    month_data[, actual := ifelse(trade, target, prev_actual)]

    port_ret <- sum(month_data$actual * month_data$ret_exc_lead1m, na.rm = TRUE)
    port_ret_lag <- sum(
      month_data$actual * (month_data$ret_exc_lead1m - month_data$ret_day1),
      na.rm = TRUE
    )
    turnover <- sum(
      abs(month_data$actual - month_data$prev_actual),
      na.rm = TRUE
    )

    month_data[,
      tc_drag_stock := ifelse(
        trade,
        2 * (tc_bps / 10000) * abs(target - prev_actual),
        0
      )
    ]
    tc_drag <- sum(month_data$tc_drag_stock, na.rm = TRUE)

    month_data[,
      sc_drag_stock := ifelse(
        actual < 0,
        abs(actual) * (sc_annual_bps / 10000) / 12,
        0
      )
    ]
    sc_drag <- sum(month_data$sc_drag_stock, na.rm = TRUE)

    month_data[,
      tax_drag_stock := ifelse(
        actual < 0 & div1m_me > 0,
        abs(actual) * div1m_me * TAX_RATE,
        0
      )
    ]
    tax_drag <- sum(month_data$tax_drag_stock, na.rm = TRUE)

    results_list[[i]] <- data.table(
      eom = m,
      ret_gross = port_ret,
      ret_gross_lagged = port_ret_lag,
      turnover = turnover,
      tc_drag = tc_drag,
      sc_drag = sc_drag,
      tax_drag = tax_drag
    )

    holdings <- month_data[actual != 0, .(id, weight = actual)]
  }
  rbindlist(results_list)
}

drift_results_list <- list()
for (th in THRESHOLDS_BPS) {
  cat(sprintf("      Threshold = %d bps ...\n", th))
  drift_results_list[[as.character(th)]] <- simulate_drift_band(dt, th)
}

drift_all <- rbindlist(drift_results_list, idcol = "threshold")
drift_all[, threshold := as.numeric(threshold)]

# Calculate exact net returns for each threshold inside drift_all
drift_all[, ret_net := ret_gross_lagged - tc_drag - sc_drag - tax_drag]

# =============================================================================
# 5. INTEGRATE RESULTS & CALCULATE WEALTH INDICES
# =============================================================================
cat("[5/5] Integrating results and building wealth indices...\n")

drift_5 <- drift_all[threshold == 5, .(eom, ret_net_5bps = ret_net)]
perf_ts[drift_5, on = "eom", ret_net_5bps := i.ret_net_5bps]

# Calculate all Wealth Indices together
perf_ts[, `:=`(
  wealth_standard = cumprod(1 + replace(ret_standard, is.na(ret_standard), 0)),
  wealth_lagged = cumprod(
    1 + replace(ret_gross_lagged, is.na(ret_gross_lagged), 0)
  ),
  wealth_net_tc = cumprod(1 + replace(ret_net_1_tc, is.na(ret_net_1_tc), 0)),
  wealth_net_sc = cumprod(1 + replace(ret_net_2_sc, is.na(ret_net_2_sc), 0)),
  wealth_final = cumprod(1 + replace(ret_net_3_tax, is.na(ret_net_3_tax), 0)),
  wealth_net_5bps = cumprod(1 + replace(ret_net_5bps, is.na(ret_net_5bps), 0))
)]

# =============================================================================
# 7. FINAL VISUALIZATIONS & REPORTING
# =============================================================================
cat("\n=== FRICTION DECOMPOSITION (Avg Annual Drag) ===\n")
cat("Transaction Costs: ", percent(mean(perf_ts$drag_tc) * 12), "\n")
cat("Shorting Costs:    ", percent(mean(perf_ts$drag_sc) * 12), "\n")
cat("Dividend Tax:      ", percent(mean(perf_ts$drag_tax) * 12), "\n\n")

# --- Plot: Final Friction Waterfall (with 5bps overlay) ---
p_waterfall <- ggplot(perf_ts, aes(x = eom)) +
  geom_ribbon(
    aes(
      ymin = wealth_net_tc,
      ymax = wealth_lagged,
      fill = "1. Transaction Costs"
    ),
    alpha = 0.5
  ) +
  geom_ribbon(
    aes(ymin = wealth_net_sc, ymax = wealth_net_tc, fill = "2. Shorting Costs"),
    alpha = 0.5
  ) +
  geom_ribbon(
    aes(ymin = wealth_final, ymax = wealth_net_sc, fill = "3. Dividend Tax"),
    alpha = 0.5
  ) +
  geom_line(
    aes(y = wealth_standard, linetype = "Theoretical"),
    color = "grey60"
  ) +
  geom_line(aes(y = wealth_lagged, color = "Gross (Lagged)"), size = 1) +
  geom_line(aes(y = wealth_final, color = "Net (0bps Rebal)"), size = 1) +
  geom_line(aes(y = wealth_net_5bps, color = "Net (5bps Drift)"), size = 1.2) +
  scale_fill_manual(
    name = "Friction Layers",
    values = c(
      "1. Transaction Costs" = "#FDBF6F",
      "2. Shorting Costs" = "#FB9A99",
      "3. Dividend Tax" = "#CAB2D6"
    )
  ) +
  scale_color_manual(
    name = "Performance",
    values = c(
      "Gross (Lagged)" = "#1F78B4",
      "Net (0bps Rebal)" = "black",
      "Net (5bps Drift)" = "#c8a951"
    )
  ) +
  scale_linetype_manual(
    name = "Reference",
    values = c("Theoretical" = "dashed")
  ) +
  labs(
    title = "Strategy Performance: Decomposition of Frictions",
    subtitle = "Cumulative wealth lost to Frictions, and 5 bps Drift Band Rebalancing",
    y = "Wealth Index (Log Scale)",
    x = "Date"
  ) +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

print(p_waterfall)
ggsave(
  "friction_waterfall_final.png",
  p_waterfall,
  width = 9,
  height = 5,
  dpi = 150
)

# --- Performance Table ---
xts_final <- xts(
  perf_ts[, .(ret_standard, ret_gross_lagged, ret_net_3_tax, ret_net_5bps)],
  order.by = perf_ts$eom
)
colnames(xts_final) <- c(
  "Original (T-Close)",
  "Lagged (T+1)",
  "Net (0bps Rebal)",
  "Net (5bps Drift)"
)

cat("=== FINAL STRATEGY METRICS ===\n")
print(PerformanceAnalytics::table.AnnualizedReturns(xts_final, scale = 12))


# --- Plot: Drift-Band Net Sharpe vs Threshold ---
summary_drift <- drift_all[,
  .(
    ann_ret = mean(ret_net) * 12,
    ann_vol = sd(ret_net) * sqrt(12),
    sharpe = mean(ret_net) / sd(ret_net) * sqrt(12),
    avg_turnover = mean(turnover),
    tc_drag_ann = mean(tc_drag) * 12
  ),
  by = threshold
][order(threshold)]

max_point <- summary_drift[which.max(sharpe)]

p_sharpe <- ggplot(summary_drift, aes(x = threshold, y = sharpe)) +
  geom_line(color = "#1a2e5a", linewidth = 1.2) +
  geom_point(size = 3, color = "#1e50a2") +
  geom_point(data = max_point, color = "#c8a951", size = 5, shape = 18) +
  geom_label(
    aes(label = paste0(threshold, " bps")),
    vjust = -0.8,
    size = 3,
    color = "#1a2e5a",
    label.size = 0.2,
    fill = "white"
  ) +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  labs(
    x = "Drift‑band threshold (bps)",
    y = "Net Sharpe Ratio",
    title = "Drift‑band rebalancing: Net Sharpe vs threshold",
    subtitle = "Optimal threshold improves Sharpe by retaining gross alpha and slashing TCs"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#1a2e5a"))

print(p_sharpe)
ggsave(
  "drift_sharpe_vs_threshold.png",
  p_sharpe,
  width = 7,
  height = 4,
  dpi = 150
)

# --- Plot: Turnover and TC drag (Dual metrics) ---
p_turnover <- summary_drift %>%
  select(threshold, Turnover = avg_turnover, TC = tc_drag_ann) %>%
  pivot_longer(
    cols = c(Turnover, TC),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = ifelse(
      Metric == "Turnover",
      "Avg monthly turnover",
      "TC drag (% p.a.)"
    )
  ) %>%
  ggplot(aes(x = threshold, y = Value, color = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  scale_color_manual(
    values = c("Avg monthly turnover" = "#1e50a2", "TC drag (% p.a.)" = "#c8a951")
  ) +
  labs(
    x = "Drift‑band threshold (bps)",
    y = "Value",
    title = "Turnover and transaction cost decline with threshold",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", color = "#1a2e5a")
  )

print(p_turnover)
ggsave("drift_turnover_TC.png", p_turnover, width = 7, height = 4, dpi = 150)

cat("\n=== ANALYSIS COMPLETE ===\n")


# =============================================================================
# 7. INDUSTRY EXPOSURE (FF17) STACKED AREA CHART & SUMMARY
# =============================================================================
cat("\n[6/6] Generating Industry Exposure Stacked Area Chart & Summary...\n")

# 1. Pull SIC codes from USA_FILE
usa_inds <- read_parquet(
  USA_FILE,
  col_select = c("id", "date", "sic")
) %>%
  as.data.table()

# 2. Merge SIC into the main dt
dt[, sic := usa_inds[dt, on = .(id, date = eom), roll = "nearest", x.sic]]

# 3. Map SIC to Fama-French 17 Industries
dt[,
  ff17 := case_when(
    sic >= 100 & sic <= 999 ~ "Food",
    sic >= 1000 & sic <= 1299 ~ "Mines",
    sic >= 1300 & sic <= 1399 ~ "Oil",
    sic >= 1400 & sic <= 1499 ~ "Mines",
    sic >= 1500 & sic <= 1799 ~ "Cnstr",
    sic >= 2000 & sic <= 2111 ~ "Food",
    sic >= 2200 & sic <= 2299 ~ "Txtls",
    sic >= 2300 & sic <= 2399 ~ "Clths",
    sic >= 2400 & sic <= 2499 ~ "Cnstr",
    sic >= 2500 & sic <= 2599 ~ "FabPr",
    sic >= 2600 & sic <= 2699 ~ "Paper",
    sic >= 2700 & sic <= 2799 ~ "Paper",
    sic >= 2800 & sic <= 2829 ~ "Chems",
    sic >= 2830 & sic <= 2839 ~ "Drugs",
    sic >= 2840 & sic <= 2899 ~ "Chems",
    sic >= 2900 & sic <= 2999 ~ "Oil",
    sic >= 3000 & sic <= 3099 ~ "Chems",
    sic >= 3100 & sic <= 3199 ~ "Clths",
    sic >= 3200 & sic <= 3299 ~ "Cnstr",
    sic >= 3300 & sic <= 3399 ~ "Steel",
    sic >= 3400 & sic <= 3499 ~ "FabPr",
    sic >= 3500 & sic <= 3599 ~ "Machn",
    sic >= 3600 & sic <= 3699 ~ "Machn",
    sic >= 3700 & sic <= 3719 ~ "Cars",
    sic >= 3720 & sic <= 3799 ~ "Machn",
    sic >= 3800 & sic <= 3899 ~ "Machn",
    sic >= 3900 & sic <= 3999 ~ "Other",
    sic >= 4000 & sic <= 4899 ~ "Trans",
    sic >= 4900 & sic <= 4999 ~ "Utils",
    sic >= 5000 & sic <= 5199 ~ "Other",
    sic >= 5200 & sic <= 5999 ~ "Rtail",
    sic >= 6000 & sic <= 6999 ~ "Finan",
    sic >= 7000 & sic <= 7999 ~ "Other",
    sic >= 8000 & sic <= 8999 ~ "Other",
    sic >= 9000 & sic <= 9999 ~ "Other",
    TRUE ~ "Missing"
  )
]

# 4. Calculate Gross Exposure (abs weight) by Month and Industry
ind_exposure <- dt[,
  .(gross_w = sum(abs(weight), na.rm = TRUE)),
  by = .(eom, ff17)
]

# Calculate month-total gross exposure to find the percentage
ind_exposure[, total_gross_month := sum(gross_w), by = eom]
ind_exposure[, pct_weight := gross_w / total_gross_month]

# 5. Define Color Palette
high_vis_17 <- c(
  "#E6194B",
  "#3CB44B",
  "#FFE119",
  "#4363D8",
  "#F58231",
  "#911EB4",
  "#46F0F0",
  "#F032E6",
  "#BCF60C",
  "#FABEBE",
  "#008080",
  "#E6BEFF",
  "#9A6324",
  "#FFFAC8",
  "#800000",
  "#AAFFC3",
  "#808000"
)

num_industries <- length(unique(ind_exposure$ff17))
final_palette <- colorRampPalette(high_vis_17)(num_industries)

# 6. Plot the 100% Stacked Area Chart
p_ind <- ggplot(ind_exposure, aes(x = eom, y = gross_w, fill = ff17)) +
  geom_area(position = "fill", alpha = 0.9, color = "white", linewidth = 0.1) +
  scale_fill_manual(values = final_palette) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Portfolio Industry Composition (FF17)",
    subtitle = "Gross capital allocation to sectors over time (Scaled to 100%)",
    x = "Date",
    y = "Proportion of Gross Portfolio",
    fill = "Industry"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", color = "#1a2e5a"),
    panel.grid.minor = element_blank()
  )

print(p_ind)
ggsave(
  "industry_composition_stacked_v2.png",
  p_ind,
  width = 10,
  height = 5,
  dpi = 150
)
cat("\nSaved: industry_composition_stacked_v2.png\n")


# 7. SUMMARY ANALYSIS: Print the average weights over time
last_date <- max(ind_exposure$eom)

ind_summary <- ind_exposure[,
  .(
    Avg_Historical_Weight = mean(pct_weight),
    Max_Historical_Weight = max(pct_weight),
    Min_Historical_Weight = min(pct_weight),
    Current_Weight = pct_weight[eom == last_date][1] # Get weight as of the final month
  ),
  by = ff17
][order(-Avg_Historical_Weight)]

# Format as percentages
ind_summary_print <- ind_summary[, .(
  Industry = ff17,
  `Avg Hist. Weight` = percent(Avg_Historical_Weight, accuracy = 0.1),
  `Max Hist. Weight` = percent(Max_Historical_Weight, accuracy = 0.1),
  `Min Hist. Weight` = percent(Min_Historical_Weight, accuracy = 0.1),
  `Current Weight` = percent(Current_Weight, accuracy = 0.1)
)]

cat("\n=== INDUSTRY CONCENTRATION SUMMARY ===\n")
cat(
  "Note: Weights represent proportion of total deployed capital (Long + Short)\n\n"
)
print(ind_summary_print, row.names = FALSE)

# Also warn if any industry breached 20%
breach <- ind_summary[Max_Historical_Weight > 0.20]
if (nrow(breach) > 0) {
  cat(
    "\n⚠️ WARNING: The following industries exceeded 20% concentration at least once:\n"
  )
  for (i in 1:nrow(breach)) {
    cat(sprintf(
      "   - %s (Max hit %s)\n",
      breach$ff17[i],
      percent(breach$Max_Historical_Weight[i], accuracy = 0.1)
    ))
  }
}
cat("======================================\n")


# --- Compute annualised performance metrics per threshold (Gross vs Net) ---
summary_drift <- drift_all[,
  .(
    Gross_Ret_Ann = mean(ret_gross_lagged) * 12,
    Gross_Vol_Ann = sd(ret_gross_lagged) * sqrt(12),
    Gross_Sharpe = mean(ret_gross_lagged) / sd(ret_gross_lagged) * sqrt(12),

    tc_drag_ann = mean(tc_drag) * 12,
    avg_turnover = mean(turnover),

    Net_Ret_Ann = mean(ret_gross_lagged - tc_drag - sc_drag - tax_drag) * 12,
    Net_Vol_Ann = sd(ret_gross_lagged - tc_drag - sc_drag - tax_drag) *
      sqrt(12),
    Net_Sharpe = mean(ret_gross_lagged - tc_drag - sc_drag - tax_drag) /
      sd(ret_gross_lagged - tc_drag - sc_drag - tax_drag) *
      sqrt(12)
  ),
  by = threshold
][order(threshold)]

# --- Print summary table ---
cat("\n=== DRIFT-BAND TRADE-OFF: GROSS VS NET ===\n")
print(summary_drift[, .(
  threshold,
  Gross_Ret_Ann,
  tc_drag_ann,
  Net_Ret_Ann,
  Gross_Sharpe,
  Net_Sharpe
)])

# --- Export to CSV ---
fwrite(summary_drift, "drift_band_summary.csv")
cat("\nSaved: drift_band_summary.csv\n")

# --- PLOTS ---

# Convert to plain data.frame for ggplot
summary_df <- as.data.frame(summary_drift)

# 1. Gross vs Net Sharpe ratio trade-off plot
sharpe_data <- summary_df %>%
  select(threshold, Gross_Sharpe, Net_Sharpe) %>%
  pivot_longer(
    cols = c(Gross_Sharpe, Net_Sharpe),
    names_to = "Metric",
    values_to = "Sharpe"
  )

p_tradeoff <- ggplot(
  sharpe_data,
  aes(x = threshold, y = Sharpe, color = Metric)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = THRESHOLDS_BPS) +
  scale_color_manual(
    values = c("Gross_Sharpe" = "#1F78B4", "Net_Sharpe" = "#c8a951"),
    labels = c("Gross Sharpe (Before Fees)", "Net Sharpe (After Fees)")
  ) +
  labs(
    x = "Drift-band threshold (bps)",
    y = "Annualized Sharpe Ratio",
    title = "The Drift Band Trap: Alpha Decays Faster Than Costs Drop",
    subtitle = "Saving on fees is useless if holding 'stale' momentum kills the Gross Alpha",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", color = "#1a2e5a")
  )

print(p_tradeoff)
ggsave(
  "drift_tradeoff_gross_vs_net.png",
  p_tradeoff,
  width = 8,
  height = 5,
  dpi = 150
)
cat("\nSaved: drift_tradeoff_gross_vs_net.png\n")


# --- Cumulative Wealth (Gross vs Net Trade-off) ---
cat("\nGenerating Gross vs Net Cumulative Wealth Plot...\n")

compare_thresholds <- c(0, 5)

# 1. Ensure data is sorted by date
setorder(drift_all, threshold, eom)

# 2. Compute both Gross and Net Cumulative Wealth
wealth_compare <- drift_all[
  threshold %in% compare_thresholds,
  .(
    eom,
    Gross_Wealth = cumprod(1 + ret_gross_lagged),
    Net_Wealth = cumprod(1 + (ret_gross_lagged - tc_drag - sc_drag - tax_drag))
  ),
  by = threshold
]

# 3. Reshape the data for ggplot
wealth_long <- wealth_compare %>%
  pivot_longer(
    cols = c(Gross_Wealth, Net_Wealth),
    names_to = "Return_Type",
    values_to = "Wealth"
  ) %>%
  mutate(
    Strategy = paste0(
      ifelse(Return_Type == "Gross_Wealth", "Gross", "Net"),
      " (",
      threshold,
      " bps Drift)"
    )
  ) %>%
  as.data.frame()

# 4. Create the plot
p_wealth_compare <- ggplot(
  wealth_long,
  aes(x = eom, y = Wealth, color = Strategy, linetype = Return_Type)
) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = scales::dollar) +
  scale_color_manual(
    values = c(
      "Gross (0 bps Drift)" = "#1F78B4",
      "Net (0 bps Drift)" = "#A6CEE3",
      "Gross (5 bps Drift)" = "#c8a951",
      "Net (5 bps Drift)" = "#E5D8BD"
    )
  ) +
  scale_linetype_manual(
    values = c("Gross_Wealth" = "dashed", "Net_Wealth" = "solid"),
    guide = "none"
  ) +
  labs(
    x = "Date",
    y = "Cumulative Wealth (log scale)",
    title = "The Cost of Stale Signals: Gross vs Net Wealth",
    subtitle = "The 5 bps drift band saves fees (narrower gap), but destroys the gross signal (dashed line collapses).",
    color = "Portfolio Version"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", color = "#1a2e5a")
  )

print(p_wealth_compare)
ggsave(
  "drift_wealth_gross_vs_net.png",
  p_wealth_compare,
  width = 9,
  height = 5,
  dpi = 150
)
cat("\nSaved: drift_wealth_gross_vs_net.png\n")


# --- Cumulative Wealth (Gross vs Net with Shaded Fee Drag) ---
cat("\nGenerating Gross vs Net Cumulative Wealth Plot...\n")

compare_thresholds <- c(0, 5)

# 1. Ensure data is sorted by date
setorder(drift_all, threshold, eom)

# 2. Compute both Gross and Net Cumulative Wealth
wealth_compare <- drift_all[
  threshold %in% compare_thresholds,
  .(
    eom,
    Gross_Wealth = cumprod(1 + ret_gross_lagged),
    Net_Wealth = cumprod(1 + (ret_gross_lagged - tc_drag - sc_drag - tax_drag))
  ),
  by = threshold
]

# Add a descriptive label
wealth_compare[, Strategy := paste0(threshold, " bps Drift")]

# Convert to data.frame for standard ggplot handling
wealth_df <- as.data.frame(wealth_compare)

# 3. Create the shaded plot
p_wealth_shaded <- ggplot(wealth_df, aes(x = eom, group = Strategy)) +

  # Step A: Shade the area between Gross and Net (This represents the Fees Paid)
  geom_ribbon(
    aes(ymin = Net_Wealth, ymax = Gross_Wealth, fill = Strategy),
    alpha = 0.25
  ) +

  # Step B: Plot the Gross Lines (Dashed - Before Fees)
  geom_line(
    aes(y = Gross_Wealth, color = paste0(Strategy, " (Gross)")),
    linetype = "dashed",
    linewidth = 0.8
  ) +

  # Step C: Plot the Net Lines (Solid - After Fees)
  geom_line(
    aes(y = Net_Wealth, color = paste0(Strategy, " (Net)")),
    linetype = "solid",
    linewidth = 1.2
  ) +

  # Scales
  scale_y_log10(labels = scales::dollar) +

  # Colors for the Shaded Areas
  scale_fill_manual(
    name = "Fee Drag (Shaded Area)",
    values = c("0 bps Drift" = "#1F78B4", "5 bps Drift" = "#c8a951")
  ) +

  # Colors for the Lines
  scale_color_manual(
    name = "Wealth Lines",
    values = c(
      "0 bps Drift (Gross)" = "#1F78B4",
      "0 bps Drift (Net)" = "#1F78B4",
      "5 bps Drift (Gross)" = "#c8a951",
      "5 bps Drift (Net)" = "#c8a951"
    )
  ) +

  # Labels and Styling
  labs(
    x = "Date",
    y = "Cumulative Wealth (log scale)",
    title = "The Cost of Stale Signals: Gross vs Net Wealth",
    caption = "Dashed = Gross Returns | Solid = Net Returns | Shaded Area = Cumulative Frictions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", color = "#1a2e5a"),
    panel.grid.minor = element_blank()
  )

print(p_wealth_shaded)
ggsave(
  "drift_wealth_gross_vs_net_shaded.png",
  p_wealth_shaded,
  width = 10,
  height = 5,
  dpi = 150
)
cat("\nSaved: drift_wealth_gross_vs_net_shaded.png\n")


# --- 4. PERFORMANCE SUMMARY TABLE (0 vs 5 bps) ---
cat("\n=== PERFORMANCE METRICS: 0 bps vs 5 bps DRIFT BAND ===\n")

# Extract and format the relevant thresholds from summary_drift
metrics_table <- summary_drift[
  threshold %in% compare_thresholds,
  .(
    Threshold = paste0(threshold, " bps"),
    `Gross Ret p.a.` = percent(Gross_Ret_Ann, accuracy = 0.1),
    `Gross Vol` = percent(Gross_Vol_Ann, accuracy = 0.1),
    `Gross Sharpe` = round(Gross_Sharpe, 2),
    `Net Ret p.a.` = percent(Net_Ret_Ann, accuracy = 0.1),
    `Net Vol` = percent(Net_Vol_Ann, accuracy = 0.1),
    `Net Sharpe` = round(Net_Sharpe, 2)
  )
]

print(metrics_table, row.names = FALSE)
cat("======================================================\n")
