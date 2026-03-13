# ==============================================================================
# PHASE 2: SINGLE STOCK WEIGHTS ANALYSIS
# Investability, Turnover, and Concentration
# ==============================================================================

# --- LIBRARIES ---
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(arrow, tidyverse, data.table, lubridate, scales, gridExtra, PerformanceAnalytics, viridis)


# --- CONFIGURATION ---
FILE_PATH <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs/arnott_stock_weights.parquet"

# --- 1. LOAD DATA ---
cat("Loading Portfolio Weights...\n")
dt <- read_parquet(FILE_PATH) %>% as.data.table()

# Ensure date format
dt[, eom := as.Date(eom)]
setorder(dt, eom, id)

# --- 2. SANITY CHECKS ---
cat("\n--- SANITY CHECKS ---\n")

# Gross Exposure should be 1.0 or 100%
check_gross <- dt[, .(gross_exp = sum(abs(weight))), by = eom]
cat(
  "Avg Gross Exposure (Target 1.0): ",
  round(mean(check_gross$gross_exp), 4),
  "\n"
)

# CNet Exposure should be 0.0 for dollar neutral
check_net <- dt[, .(net_exp = sum(weight)), by = eom]
cat(
  "Avg Net Exposure (Target 0.0):   ",
  round(mean(check_net$net_exp), 4),
  "\n"
)

# Missing IDs
n_ids <- uniqueN(dt$id)
cat("Total Unique Stocks traded since 1963:", n_ids, "\n")


# --- 3. CONCENTRATION ANALYSIS ---
# How much of the portfolio is in the Top 10 bets?

conc_stats <- dt[,
  .(
    top10_share = sum(sort(abs(weight), decreasing = TRUE)[1:10]),
    max_single_weight = max(abs(weight))
  ),
  by = eom
]

p1 <- ggplot(conc_stats, aes(x = eom, y = top10_share)) +
  geom_area(fill = "#2c3e50", alpha = 0.7) +
  geom_hline(
    yintercept = mean(conc_stats$top10_share),
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Portfolio Concentration",
    subtitle = "Sum of Absolute Weights of Top 10 Holdings",
    y = "Weight %",
    x = ""
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()


# --- 4. TURNOVER ANALYSIS ---
# Calculation: Sum(|w_t - w_{t-1}|) / 2

cat("\nCalculating Turnover (This may take a moment)...\n")

# Create t-1 dataset
dt_prev <- copy(dt)
dt_prev[, eom := eom %m+% months(1)] # Shift date forward to match 'next' month
setnames(dt_prev, "weight", "w_prev")

dt_turnover <- merge(dt, dt_prev, by = c("eom", "id"), all = TRUE)

# Fill NAs with 0 (Entry or Exit)
dt_turnover[is.na(weight), weight := 0]
dt_turnover[is.na(w_prev), w_prev := 0]

# Calculate Monthly Turnover 
monthly_tcost <- dt_turnover[,
  .(
    turnover = sum(abs(weight - w_prev)) / 2
  ),
  by = eom
]

avg_turnover <- mean(monthly_tcost$turnover, na.rm = TRUE)
ann_turnover <- avg_turnover * 12

cat("Average Monthly Turnover:", percent(avg_turnover), "\n")
cat("Annualized Turnover:     ", percent(ann_turnover), "\n")

p2 <- ggplot(monthly_tcost, aes(x = eom, y = turnover)) +
  geom_col(fill = "#e74c3c") +
  geom_hline(yintercept = avg_turnover, color = "black", size = 1) +
  labs(
    title = "Monthly Turnover (One-Way)",
    subtitle = paste0("Avg Annualized: ", percent(ann_turnover)),
    y = "Turnover %",
    x = "Date"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()


# --- 5. POSITION COUNTS ---
pos_counts <- dt[,
  .(
    Longs = sum(weight > 0),
    Shorts = sum(weight < 0)
  ),
  by = eom
] %>%
  pivot_longer(
    cols = c("Longs", "Shorts"),
    names_to = "Leg",
    values_to = "Count"
  )

p3 <- ggplot(pos_counts, aes(x = eom, y = Count, fill = Leg)) +
  geom_area() +
  scale_fill_manual(values = c("Longs" = "#8f9ba6ff", "Shorts" = "#0B2B65")) +
  labs(
    title = "Number of Active Positions",
    subtitle = "Total breadth of the strategy",
    y = "Count",
    x = "Date"
  ) +
  theme_minimal()


# --- 6. OUTPUT ---
grid.arrange(p1, p3, ncol = 1)

# Summary Table 
summary_table <- data.frame(
  Metric = c(
    "Annualized Turnover",
    "Avg Top 10 Concentration",
    "Max Single Stock Weight",
    "Avg Positions (Long)",
    "Avg Positions (Short)"
  ),
  Value = c(
    percent(ann_turnover),
    percent(mean(conc_stats$top10_share)),
    percent(max(conc_stats$max_single_weight)),
    round(mean(pos_counts$Count[pos_counts$Leg == "Longs"])),
    round(mean(pos_counts$Count[pos_counts$Leg == "Shorts"]))
  )
)

print(summary_table)


# ==============================================================================
# PHASE 2: LOCAL BACKTEST SCRIPT
# ==============================================================================

# --- 1. CONFIGURATION ---
MASTER_FILE <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code/01_Data/Clean_Daily_Inputs/arnott_master.parquet" # Update this path!

# --- 2. LOAD MASTER FILE ---
dt <- read_parquet(MASTER_FILE) %>% as.data.table()
dt[, eom := as.Date(eom)]

# --- 3. CALCULATE PORTFOLIO RETURNS ---
# A. Standard (Trade at Month End Close)
# B. Lagged (Trade at Next Day Close) 

perf_ts <- dt[,
  .(
    # Standard: Sum(Weight * Monthly_Ret)
    ret_standard = sum(weight * ret_exc_lead1m, na.rm = TRUE),

    # Lagged: Sum(Weight * (Monthly_Ret - Day1_Ret))
    ret_lagged = sum(weight * (ret_exc_lead1m - ret_day1), na.rm = TRUE),

    gross_exp = sum(abs(weight))
  ),
  by = eom
][order(eom)]

# --- 4. TURNOVER CALCULATION ---
# Shift weights to align t and t-1
w_curr <- dt[, .(eom, id, w_t = weight)]
w_prev <- dt[, .(eom = eom %m+% months(1), id, w_prev = weight)]

turnover_dt <- merge(w_curr, w_prev, by = c("eom", "id"), all = TRUE)
turnover_dt[is.na(w_t), w_t := 0]
turnover_dt[is.na(w_prev), w_prev := 0]

t_cost_ts <- turnover_dt[,
  .(
    turnover = sum(abs(w_t - w_prev)) / 2
  ),
  by = eom
]

# Merge into performance time series
perf_ts <- merge(perf_ts, t_cost_ts, by = "eom")

# --- 5. REPORTING ---
xts_ret <- xts(perf_ts[, .(ret_standard, ret_lagged)], order.by = perf_ts$eom)

cat("\n=== STRATEGY PERFORMANCE (1963-Present) ===\n")
table.AnnualizedReturns(xts_ret)
maxDrawdown(xts_ret)

cat("\n=== IMPLEMENTATION REALITY ===\n")
cat("Avg Annualized Turnover: ", percent(mean(perf_ts$turnover) * 12), "\n")
cat(
  "Avg Gross Exposure:      ",
  round(mean(perf_ts$gross_exp), 2),
  "(Target: 1.0)\n"
)

# --- 6. PLOTTING ---
charts.PerformanceSummary(
  xts_ret,
  main = "Arnott Factor Momentum: Standard vs. Lagged",
  colorset = c("black", "red")
)

# ==============================================================================
# DEEP DIVE INTO WEIGHT BEHAVIOR
# ==============================================================================

# --- 1. LOAD & PREP ---
cat("Loading Data...\n")
dt <- read_parquet(FILE_PATH) %>% as.data.table()
dt[, eom := as.Date(eom)]

# --- 2. THE NETTING EFFICIENCY CHECK ---

cat("Calculating Netting Efficiency...\n")

# --- 3. WEIGHT DISTRIBUTION ---
# Filter for non-zero positions
active_pos <- dt[weight != 0]

p1 <- ggplot(active_pos[eom > "2020-01-01"], aes(x = weight)) +
  geom_histogram(bins = 100, fill = "#2980b9", color = "white") +
  theme_minimal() +
  labs(
    title = "Weight Distribution (Post-2020)",
    subtitle = "Are weights Gaussian or Fat-Tailed?",
    x = "Net Weight",
    y = "Count"
  ) +
  xlim(-0.005, 0.005) 

# --- 4.Visualizing Turnover ---
# Pick 4 well-known stocks to see how their weight changes over time.

top_ids <- dt[, .(total_w = sum(abs(weight))), by = id][order(-total_w)][1:4]$id

stock_trace <- dt[id %in% top_ids & eom > "2015-01-01"]

p2 <- ggplot(
  stock_trace,
  aes(x = eom, y = weight, group = id, color = as.factor(id))
) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~id, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(
    title = "Single Stock Weight History (2015-Present)",
    subtitle = "Visualizing the instability that drives 600% turnover",
    y = "Weight",
    x = "Year",
    color = "Stock ID"
  ) +
  theme(legend.position = "none")

# --- 5. THE WHALE DETECTOR ---
# Find the absolute maximum weight ever assigned to a single stock.

max_w <- dt[which.max(abs(weight))]
cat("\n--- EXTREME POSITION CHECK ---\n")
cat(
  "Max Single Position:",
  percent(max_w$weight),
  "on",
  as.character(max_w$eom),
  "Stock ID:",
  max_w$id,
  "\n"
)

# Top 1% of weights
cat("99th Percentile Weight:", percent(quantile(abs(dt$weight), 0.99)), "\n")

# --- 6. HEATMAP OF TOP POSITIONS ---
# Look at the Top 20 stocks by weight for a specific year (e.g., 2023) and see how they enter/exit the "Top 20" list.

subset_yr <- dt[year(eom) == 2023]
# Rank stocks by absolute weight each month
subset_yr[, rank := frank(-abs(weight)), by = eom]
# Filter for Top 20
top_20_matrix <- subset_yr[rank <= 20]

p3 <- ggplot(
  top_20_matrix,
  aes(x = eom, y = reorder(as.factor(id), weight), fill = weight)
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#c0392b",
    mid = "white",
    high = "#27ae60",
    midpoint = 0
  ) +
  theme_minimal() +
  labs(
    title = "Top 20 Positions Heatmap (2023)",
    subtitle = "Green = Long, Red = Short. Choppy blocks = High Turnover.",
    x = "Month",
    y = "Stock ID",
    fill = "Weight"
  ) +
  theme(axis.text.y = element_text(size = 6))

# --- 7. OUTPUT ---
grid.arrange(p1, p3, ncol = 1)
print(p2) 

# ==============================================================================
# DAILY EXCESS MARKET DATA & CUMULATIVE RETURNS (LOG-SCALED)
# ==============================================================================
start_date <- ymd("1963-01-01")
end_date <- ymd("2024-12-31")

# Ensure required packages are loaded
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(frenchdata, lubridate, data.table, ggplot2)

# --- 1. FETCH FAMA-FRENCH DAILY MARKET DATA ---
cat("Downloading Kenneth French Daily Market Data...\n")
ff_raw <- download_french_data("Fama/French 3 Factors [Daily]")

# Extract the daily data subset
ff_daily <- as.data.table(ff_raw$subsets$data[[1]])

if ("...1" %in% names(ff_daily)) {
  setnames(ff_daily, "...1", "date")
}

ff_daily[, eom := ymd(date)]

# Isolate Excess Market Return 
ff_daily[, mkt_exc := `Mkt-RF` / 100]

# Filter FF data to the exact specified timeframe
ff_clean <- ff_daily[eom >= start_date & eom <= end_date, .(eom, mkt_exc)]

# --- 2. MERGE WITH STRATEGY RETURNS ---
setDT(perf_ts)
perf_ts_filtered <- perf_ts[eom >= start_date & eom <= end_date]

# Merge the daily market skeleton with the filtered strategy returns
perf_ts_daily <- merge(ff_clean, perf_ts_filtered, by = "eom", all.x = TRUE)

# Ensure the result stays a data.table
setDT(perf_ts_daily)

# Handle NAs for non-trading days 
perf_ts_daily[is.na(mkt_exc), mkt_exc := 0]
perf_ts_daily[is.na(ret_lagged), ret_lagged := 0]

# --- 3. CALCULATE CUMULATIVE RETURNS (BASE = 1) ---
perf_ts_daily[, cum_ret_lagged := cumprod(1 + ret_lagged)]
perf_ts_daily[, cum_ret_mkt_exc := cumprod(1 + mkt_exc)]

# --- 4. PREPARE DATA ---

plot_dt <- melt(
  perf_ts_daily[, .(eom, cum_ret_lagged, cum_ret_mkt_exc)],
  id.vars = "eom",
  variable.name = "Strategy",
  value.name = "CumRet"
)

# Ensure plot_dt is a data.table after melting
setDT(plot_dt)

# Rename levels for a cleaner legend
plot_dt[,
  Strategy := fcase(
    Strategy == "cum_ret_lagged"  , "Gross (Lagged)"         ,
    Strategy == "cum_ret_mkt_exc" , "Excess Market (Mkt-RF)"
  )
]

# Ensure the factor ordering so the legend matches the visual hierarchy
plot_dt[,
  Strategy := factor(
    Strategy,
    levels = c("Gross (Lagged)", "Excess Market (Mkt-RF)")
  )
]

# --- 5. GENERATE LOG-SCALED PLOT ---
p_cum_ret <- ggplot(plot_dt, aes(x = eom, y = CumRet, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::comma_format(accuracy = 0.1)
  ) +
  scale_color_manual(
    values = c(
      "Gross (Lagged)" = "#1F78B4", 
      "Excess Market (Mkt-RF)" = "#95a5a6" 
    )
  ) +
  labs(
    title = "Cumulative Strategy Excess Returns vs. Daily Excess Market",
    subtitle = paste0("Log-scaled, Base = 1 | ", start_date, " to ", end_date),
    x = "Date",
    y = "Cumulative Return (Log Scale)",
    color = "Portfolio"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor.y = element_blank()
  )

# Display the plot
print(p_cum_ret)


# ==============================================================================
# PERFORMANCE STATISTICS
# ==============================================================================

cat(
  "\n=== ANNUALIZED PERFORMANCE METRICS (",
  as.character(start_date),
  "to",
  as.character(end_date),
  ") ===\n"
)

# 1. Market Metrics (Calculated on Daily frequency)
mkt_ann_ret <- mean(ff_clean$mkt_exc, na.rm = TRUE) * 252
mkt_ann_vol <- sd(ff_clean$mkt_exc, na.rm = TRUE) * sqrt(252)
mkt_sharpe <- mkt_ann_ret / mkt_ann_vol

# 2. Strategy Metrics (Calculated on Monthly frequency)
strat_ann_ret <- mean(perf_ts_filtered$ret_lagged, na.rm = TRUE) * 12
strat_ann_vol <- sd(perf_ts_filtered$ret_lagged, na.rm = TRUE) * sqrt(12)
strat_sharpe <- strat_ann_ret / strat_ann_vol

# 3. Build and Print Summary Table
perf_summary <- data.frame(
  Portfolio = c("Gross (Lagged)", "Excess Market (Mkt-RF)"),
  Ann_Return = scales::percent(c(strat_ann_ret, mkt_ann_ret), accuracy = 0.01),
  Ann_Volatility = scales::percent(
    c(strat_ann_vol, mkt_ann_vol),
    accuracy = 0.01
  ),
  Sharpe_Ratio = round(c(strat_sharpe, mkt_sharpe), 2)
)

print(perf_summary)
