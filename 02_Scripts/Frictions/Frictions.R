# --- LIBRARIES ---
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
MASTER_FILE  <- "C:/Users/h12427911/Downloads/arnott_master.parquet"
USA_FILE     <- "C:/Users/h12427911/Downloads/USA.parquet" 

# A. Transaction Cost Parameters
TC_BASE_BPS  <- 5      
TC_SLOPE_BPS <- 1      
TC_ALPHA     <- 0.25   
TC_MAX_BPS   <- 100    

# B. Shorting Cost Parameters
SC_BASE_BPS  <- 25     
SC_SLOPE_BPS <- 10     
SC_ALPHA     <- 0.5    
SC_MAX_BPS   <- 500    

# C. Tax Parameters
TAX_RATE     <- 0.275  # 27.5% Withholding Tax on Dividends

# --- 2. LOAD & PREP DATA (CORRECTED) ---
dt <- read_parquet(MASTER_FILE) %>% as.data.table()
dt[, eom := as.Date(eom)]

# Load 'me' AND 'div1m_me'
usa_data <- read_parquet(USA_FILE, col_select = c("id", "date", "me", "div1m_me")) %>%
  as.data.table()

# CORRECTED MERGE: Update 'dt' by reference to prevent row duplication
# We map usa_data columns to dt using a rolling join.
# This keeps the original row count of 'dt' intact.
dt[, `:=`(
  me       = usa_data[dt, on = .(id, date = eom), roll = "nearest", x.me],
  div1m_me = usa_data[dt, on = .(id, date = eom), roll = "nearest", x.div1m_me]
)]

# --- 3. CALCULATE FRICTION RATES ---
dt[, ref_mcap := {
  top_500 <- head(sort(me, decreasing = TRUE), 500)
  median(top_500, na.rm = TRUE)
}, by = eom]

# TC Rate
dt[, tc_bps := pmin(TC_MAX_BPS, TC_BASE_BPS + TC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^TC_ALPHA)]
dt[is.na(tc_bps), tc_bps := quantile(tc_bps, 0.90, na.rm = TRUE), by = eom]

# SC Rate
dt[, sc_annual_bps := pmin(SC_MAX_BPS, SC_BASE_BPS + SC_SLOPE_BPS * (ref_mcap / pmax(me, 1))^SC_ALPHA)]
dt[is.na(sc_annual_bps), sc_annual_bps := quantile(sc_annual_bps, 0.90, na.rm = TRUE), by = eom]

# --- 4. CALCULATE COSTS ---

# --- A. Transaction Costs (Turnover) ---
w_curr <- dt[, .(eom, id, w_t = weight, tc_bps)] 
w_prev <- dt[, .(eom = eom %m+% months(1), id, w_prev = weight)]

trades_dt <- merge(w_curr, w_prev, by = c("eom", "id"), all = TRUE)
trades_dt[is.na(w_t), c("w_t", "tc_bps") := .(0, NA)] 
trades_dt[is.na(w_prev), w_prev := 0]

avg_costs <- trades_dt[, .(avg_tc = mean(tc_bps, na.rm=TRUE)), by = eom]
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
perf_ts <- dt[, .(
  ret_standard     = sum(weight * ret_exc_lead1m, na.rm = TRUE),
  ret_gross_lagged = sum(weight * (ret_exc_lead1m - ret_day1), na.rm = TRUE)
), by = eom][order(eom)]

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
  ret_net_1_tc   = ret_gross_lagged - drag_tc,
  ret_net_2_sc   = ret_gross_lagged - drag_tc - drag_sc,
  ret_net_3_tax  = ret_gross_lagged - drag_tc - drag_sc - drag_tax # Final
)]

# --- 6. PLOTTING ---
# Calculate Wealth Indices
perf_ts[, `:=`(
  wealth_standard = cumprod(1 + replace(ret_standard, is.na(ret_standard), 0)),
  wealth_lagged   = cumprod(1 + replace(ret_gross_lagged, is.na(ret_gross_lagged), 0)),
  wealth_net_tc   = cumprod(1 + replace(ret_net_1_tc, is.na(ret_net_1_tc), 0)),
  wealth_net_sc   = cumprod(1 + replace(ret_net_2_sc, is.na(ret_net_2_sc), 0)),
  wealth_final    = cumprod(1 + replace(ret_net_3_tax, is.na(ret_net_3_tax), 0))
)]

# Print Stats
cat("\n=== FRICTION DECOMPOSITION (Avg Annual Drag) ===\n")
cat("Transaction Costs: ", percent(mean(perf_ts$drag_tc) * 12), "\n")
cat("Shorting Costs:    ", percent(mean(perf_ts$drag_sc) * 12), "\n")
cat("Dividend Tax:      ", percent(mean(perf_ts$drag_tax) * 12), "\n")

# Visualization
ggplot(perf_ts, aes(x = eom)) +
  
  # Layer 1: Transaction Costs (Orange)
  geom_ribbon(aes(ymin = wealth_net_tc, ymax = wealth_lagged, fill = "1. Transaction Costs"), alpha = 0.5) +
  
  # Layer 2: Shorting Costs (Red)
  geom_ribbon(aes(ymin = wealth_net_sc, ymax = wealth_net_tc, fill = "2. Shorting Costs"), alpha = 0.5) +
  
  # Layer 3: Tax Costs (Purple)
  geom_ribbon(aes(ymin = wealth_final, ymax = wealth_net_sc, fill = "3. Dividend Tax"), alpha = 0.5) +
  
  # Lines
  geom_line(aes(y = wealth_standard, linetype = "Theoretical"), color = "grey60") +
  geom_line(aes(y = wealth_lagged, color = "Gross (Lagged)"), size = 1) +
  geom_line(aes(y = wealth_final, color = "Net (Final)"), size = 1) +
  
  # Styling
  scale_fill_manual(name = "Friction Layers", 
                    values = c("1. Transaction Costs" = "#FDBF6F",  # Light Orange
                               "2. Shorting Costs" = "#FB9A99",     # Light Red
                               "3. Dividend Tax" = "#CAB2D6")) +    # Light Purple
  
  scale_color_manual(name = "Performance", 
                     values = c("Gross (Lagged)" = "#1F78B4",       # Blue
                                "Net (Final)" = "black")) +
  
  scale_linetype_manual(name = "Reference", values = c("Theoretical" = "dashed")) +
  
  labs(title = "Strategy Performance: Decomposition of Frictions",
       subtitle = "Cumulative wealth lost to Transaction Costs, Borrow Fees, and Dividend Taxes",
       y = "Wealth Index (Log Scale)",
       x = "Date") +
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
colnames(xts_final) <- c("Original (T-Close)", "Lagged (T+1)", "Net (All Frictions)")

# 3. Print the table (Scale = 12 for monthly data)
cat("\n=== FINAL STRATEGY METRICS ===\n")
print(table.AnnualizedReturns(xts_final, scale = 12))
