# ==============================================================================
# ENHANCED TRADEABILITY: MARKET CAP & LIQUIDITY ANALYSIS
# ==============================================================================
# Purpose: Add market cap and trading volume data to assess real-world tradeability
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, arrow, data.table, lubridate, 
  RPostgres, DBI,
  ggplot2, scales
)

# --- CONFIGURATION ---
BASE_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
OUTPUT_DIR <- file.path(BASE_DIR, "03_Outputs/Strategy")
ANALYSIS_DIR <- file.path(BASE_DIR, "03_Outputs/Analysis")

# Input
PORTFOLIO_WITH_TICKERS <- file.path(OUTPUT_DIR, "stock_portfolio_with_tickers.parquet")

# ==============================================================================
# STEP 1: LOAD PORTFOLIO WITH TICKERS
# ==============================================================================

cat("\n=== Loading Portfolio Data ===\n")

portfolio <- read_parquet(PORTFOLIO_WITH_TICKERS)
setDT(portfolio)

# Get unique stock-month combinations
stock_months <- portfolio[, .(
  permno = id,
  year = year(trade_month),
  month = month(trade_month)
)] %>% unique()

cat("Need liquidity data for", nrow(stock_months), "stock-month combinations\n")

# ==============================================================================
# STEP 2: CONNECT TO WRDS AND GET MARKET CAP + VOLUME
# ==============================================================================

cat("\n=== Connecting to WRDS ===\n")

wrds_user <- Sys.getenv("WRDS_USER")
wrds_pass <- Sys.getenv("WRDS_PASSWORD")

wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  dbname = "wrds",
  user = wrds_user,
  password = wrds_pass,
  sslmode = "require"
)

cat("✅ Connected to WRDS\n")

# Strategy: Query in batches to avoid overwhelming WRDS
# We'll get the last 5 years of data (since most recent holdings matter most)

recent_portfolio <- portfolio[trade_month >= (max(trade_month) - years(5))]
recent_permnos <- unique(recent_portfolio$id)

cat("Querying market data for", length(recent_permnos), "stocks (last 5 years)...\n")

# Query CRSP Monthly Stock File for market cap and volume
query <- sprintf("
  SELECT 
    permno,
    date,
    prc,
    abs(prc) as abs_prc,
    vol,
    shrout,
    ret,
    abs(prc) * shrout as mkt_cap_thousands,
    CASE 
      WHEN abs(prc) * shrout IS NOT NULL 
      THEN abs(prc) * shrout / 1000000 
      ELSE NULL 
    END as mkt_cap_millions
  FROM crsp.msf
  WHERE permno IN (%s)
    AND date >= '%s'
    AND date <= '%s'
  ORDER BY permno, date
", 
paste(recent_permnos, collapse = ","),
min(recent_portfolio$trade_month),
max(recent_portfolio$trade_month)
)

market_data <- dbGetQuery(wrds, query)
setDT(market_data)

cat("✅ Retrieved", nrow(market_data), "monthly observations\n")

# Calculate average daily volume (ADV) and dollar volume
# For monthly data, we approximate ADV = monthly_vol / 21
market_data[, adv := vol / 21]
market_data[, dollar_vol_daily := abs_prc * adv]

# Align dates
market_data[, trade_month := floor_date(as.Date(date), "month")]

# ==============================================================================
# STEP 3: MERGE MARKET DATA WITH PORTFOLIO
# ==============================================================================

cat("\n=== Merging Market Data ===\n")

portfolio_with_liquidity <- merge(
  portfolio,
  market_data[, .(permno, trade_month, mkt_cap_millions, adv, dollar_vol_daily, abs_prc)],
  by.x = c("id", "trade_month"),
  by.y = c("permno", "trade_month"),
  all.x = TRUE
)

# ==============================================================================
# STEP 4: CALCULATE TRADEABILITY METRICS
# ==============================================================================

cat("\n=== Calculating Tradeability Metrics ===\n")

# Assume we're trading a $100M portfolio
PORTFOLIO_SIZE_MM <- 100

portfolio_with_liquidity[, position_size_mm := abs(net_weight) * PORTFOLIO_SIZE_MM]

# Days to trade: How many days would it take to build this position 
# if we use 10% of ADV?
ADV_USAGE_PCT <- 0.10

portfolio_with_liquidity[, shares_needed := position_size_mm * 1000000 / abs_prc]
portfolio_with_liquidity[, days_to_trade := shares_needed / (adv * ADV_USAGE_PCT)]

# Flag problematic positions
portfolio_with_liquidity[, is_illiquid := days_to_trade > 5]
portfolio_with_liquidity[, is_large_cap := mkt_cap_millions > 2000]
portfolio_with_liquidity[, is_mid_cap := mkt_cap_millions >= 200 & mkt_cap_millions <= 2000]
portfolio_with_liquidity[, is_small_cap := mkt_cap_millions < 200]

# Market cap categories
portfolio_with_liquidity[, cap_category := case_when(
  mkt_cap_millions >= 10000 ~ "Mega Cap (>$10B)",
  mkt_cap_millions >= 2000 ~ "Large Cap ($2-10B)",
  mkt_cap_millions >= 200 ~ "Mid Cap ($200M-2B)",
  mkt_cap_millions >= 50 ~ "Small Cap ($50-200M)",
  TRUE ~ "Micro Cap (<$50M)"
)]

# ==============================================================================
# STEP 5: SUMMARY STATISTICS
# ==============================================================================

cat("\n=== Tradeability Summary ===\n\n")

# Overall statistics
summary_stats <- portfolio_with_liquidity[!is.na(days_to_trade), .(
  n_positions = .N,
  
  # Position sizes
  avg_position_mm = mean(position_size_mm, na.rm = TRUE),
  median_position_mm = median(position_size_mm, na.rm = TRUE),
  max_position_mm = max(position_size_mm, na.rm = TRUE),
  
  # Market caps
  avg_mkt_cap = mean(mkt_cap_millions, na.rm = TRUE),
  median_mkt_cap = median(mkt_cap_millions, na.rm = TRUE),
  
  # Liquidity
  avg_days_to_trade = mean(days_to_trade, na.rm = TRUE),
  median_days_to_trade = median(days_to_trade, na.rm = TRUE),
  pct_illiquid = mean(is_illiquid, na.rm = TRUE),
  
  # Cap distribution
  pct_large_cap = mean(is_large_cap, na.rm = TRUE),
  pct_mid_cap = mean(is_mid_cap, na.rm = TRUE),
  pct_small_cap = mean(is_small_cap, na.rm = TRUE)
)]

cat("--- Overall Portfolio Characteristics ($100M portfolio) ---\n")
print(summary_stats)

# Monthly breakdown
monthly_liquidity <- portfolio_with_liquidity[!is.na(days_to_trade), .(
  avg_days_to_trade = mean(days_to_trade, na.rm = TRUE),
  median_days_to_trade = median(days_to_trade, na.rm = TRUE),
  pct_illiquid = mean(is_illiquid, na.rm = TRUE),
  avg_mkt_cap = mean(mkt_cap_millions, na.rm = TRUE),
  pct_large_cap = mean(is_large_cap, na.rm = TRUE)
), by = trade_month]

cat("\n--- Recent Months ---\n")
print(tail(monthly_liquidity, 12))

# ==============================================================================
# STEP 6: VISUALIZATIONS
# ==============================================================================

cat("\n=== Generating Liquidity Visualizations ===\n")

pdf(file.path(ANALYSIS_DIR, "Liquidity_Analysis.pdf"), width = 12, height = 8)

# 1. Days to Trade Distribution
p1 <- ggplot(portfolio_with_liquidity[!is.na(days_to_trade) & days_to_trade < 30], 
             aes(x = days_to_trade)) +
  geom_histogram(bins = 50, fill = "#3498DB", color = "white") +
  geom_vline(xintercept = 5, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 5.5, y = Inf, label = "5 days (threshold)", 
           vjust = 2, hjust = 0, color = "red") +
  labs(
    title = "Days to Trade Distribution",
    subtitle = "Using 10% of ADV | $100M portfolio | Positions >30 days excluded",
    x = "Days to Build Position",
    y = "Count"
  ) +
  theme_minimal()

print(p1)

# 2. Market Cap Distribution
p2 <- ggplot(portfolio_with_liquidity[!is.na(mkt_cap_millions)], 
             aes(x = mkt_cap_millions)) +
  geom_histogram(bins = 50, fill = "#27AE60", color = "white") +
  scale_x_log10(labels = dollar_format(suffix = "M")) +
  geom_vline(xintercept = c(50, 200, 2000, 10000), 
             linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Market Cap Distribution",
    subtitle = "Log scale",
    x = "Market Cap",
    y = "Count"
  ) +
  theme_minimal()

print(p2)

# 3. Market Cap Over Time
cap_time <- portfolio_with_liquidity[!is.na(cap_category), .(
  count = .N
), by = .(trade_month, cap_category)]

cap_pct <- cap_time[, .(
  cap_category = cap_category,
  pct = count / sum(count)
), by = trade_month]

p3 <- ggplot(cap_pct, aes(x = trade_month, y = pct, fill = cap_category)) +
  geom_area(alpha = 0.7) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "Market Cap Mix Over Time",
    subtitle = "How is the portfolio distributed across cap sizes?",
    y = "% of Portfolio (by count)",
    x = "",
    fill = "Market Cap"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

# 4. Liquidity Over Time
p4 <- ggplot(monthly_liquidity, aes(x = trade_month)) +
  geom_line(aes(y = median_days_to_trade, color = "Median"), linewidth = 1) +
  geom_line(aes(y = avg_days_to_trade, color = "Average"), linewidth = 1, alpha = 0.7) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.5) +
  scale_color_manual(values = c("Median" = "#2980B9", "Average" = "#E67E22")) +
  labs(
    title = "Portfolio Liquidity Over Time",
    subtitle = "Days to build average position (using 10% ADV)",
    y = "Days to Trade",
    x = "",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p4)

# 5. Scatter: Position Size vs Liquidity
latest_month <- max(portfolio_with_liquidity$trade_month)
latest_data <- portfolio_with_liquidity[trade_month == latest_month & !is.na(days_to_trade)]

p5 <- ggplot(latest_data, 
             aes(x = position_size_mm, y = days_to_trade, 
                 color = cap_category, size = abs(net_weight))) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = dollar_format(suffix = "M")) +
  scale_y_log10() +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = paste("Position Size vs Liquidity -", latest_month),
    subtitle = "Red line = 5 days threshold",
    x = "Position Size (Log Scale)",
    y = "Days to Trade (Log Scale)",
    color = "Market Cap",
    size = "Portfolio %"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p5)

# 6. Illiquid Positions Over Time
p6 <- ggplot(monthly_liquidity, aes(x = trade_month, y = pct_illiquid)) +
  geom_line(color = "#E74C3C", linewidth = 1.2) +
  geom_smooth(se = FALSE, color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "% of Illiquid Positions Over Time",
    subtitle = "Illiquid = >5 days to trade using 10% ADV",
    y = "% of Positions",
    x = ""
  ) +
  theme_minimal()

print(p6)

dev.off()

cat("✅ Liquidity analysis PDF saved\n")

# ==============================================================================
# STEP 7: IDENTIFY PROBLEMATIC POSITIONS
# ==============================================================================

cat("\n=== Identifying Problematic Positions ===\n")

# Latest month analysis
latest_portfolio <- portfolio_with_liquidity[trade_month == latest_month]

# Very illiquid positions (>10 days to trade)
very_illiquid <- latest_portfolio[!is.na(days_to_trade) & days_to_trade > 10][
  order(-days_to_trade)
][1:20, .(
  Ticker = ticker,
  Company = str_trunc(company_name, 30),
  Direction = ifelse(net_weight > 0, "LONG", "SHORT"),
  Weight = percent(abs(net_weight), 0.01),
  Position_Size_MM = round(position_size_mm, 2),
  Mkt_Cap_MM = round(mkt_cap_millions, 0),
  Days_to_Trade = round(days_to_trade, 1),
  ADV_Shares = scales::comma(adv)
)]

cat("\n--- Top 20 Most Illiquid Positions (", as.character(latest_month), ") ---\n")
print(very_illiquid)

# ==============================================================================
# STEP 8: ENHANCED TRADEABILITY SCORE
# ==============================================================================

cat("\n=== Calculating Enhanced Tradeability Score ===\n")

portfolio_with_liquidity[, enhanced_trade_score := 0]

# Market cap points (0-3)
portfolio_with_liquidity[is_large_cap == TRUE, enhanced_trade_score := enhanced_trade_score + 3]
portfolio_with_liquidity[is_mid_cap == TRUE, enhanced_trade_score := enhanced_trade_score + 2]
portfolio_with_liquidity[is_small_cap == TRUE, enhanced_trade_score := enhanced_trade_score + 1]

# Liquidity points (0-3)
portfolio_with_liquidity[days_to_trade <= 1, enhanced_trade_score := enhanced_trade_score + 3]
portfolio_with_liquidity[days_to_trade > 1 & days_to_trade <= 3, enhanced_trade_score := enhanced_trade_score + 2]
portfolio_with_liquidity[days_to_trade > 3 & days_to_trade <= 5, enhanced_trade_score := enhanced_trade_score + 1]

# Exchange points (0-2)
portfolio_with_liquidity[exchange %in% c("NYSE", "NASDAQ"), enhanced_trade_score := enhanced_trade_score + 2]

# Security type points (0-2)
portfolio_with_liquidity[share_type == "Common", enhanced_trade_score := enhanced_trade_score + 2]

# Position size appropriateness (0-2)
portfolio_with_liquidity[abs(net_weight) >= 0.001 & abs(net_weight) <= 0.02, 
                        enhanced_trade_score := enhanced_trade_score + 2]

enhanced_summary <- portfolio_with_liquidity[!is.na(enhanced_trade_score), .(
  avg_score = mean(enhanced_trade_score),
  median_score = median(enhanced_trade_score),
  pct_highly_tradeable = mean(enhanced_trade_score >= 10),
  pct_problematic = mean(enhanced_trade_score <= 5)
), by = trade_month]

cat("\n--- Enhanced Tradeability Scores (Recent) ---\n")
print(tail(enhanced_summary, 12))

# ==============================================================================
# STEP 9: SAVE ENHANCED DATA
# ==============================================================================

write_parquet(
  portfolio_with_liquidity,
  file.path(OUTPUT_DIR, "portfolio_with_full_liquidity_data.parquet")
)

# Export detailed CSV for latest month
write_csv(
  latest_portfolio[!is.na(enhanced_trade_score)][order(-enhanced_trade_score), .(
    Rank = .I,
    Ticker = ticker,
    Company = str_trunc(company_name, 40),
    Direction = ifelse(net_weight > 0, "LONG", "SHORT"),
    Weight_Pct = percent(abs(net_weight), 0.01),
    Position_Size_100M = dollar(position_size_mm * 1000000),
    Mkt_Cap = dollar(mkt_cap_millions * 1000000),
    Days_to_Trade = round(days_to_trade, 1),
    Tradeability_Score = enhanced_trade_score,
    Exchange = exchange,
    Share_Type = share_type
  )],
  file.path(ANALYSIS_DIR, paste0("tradeable_positions_ranked_", latest_month, ".csv"))
)

# Disconnect
dbDisconnect(wrds)

cat("\n=== ENHANCED ANALYSIS COMPLETE ===\n")
cat("\nKey Findings:\n")
cat("  • Average days to trade:", round(mean(portfolio_with_liquidity$days_to_trade, na.rm = TRUE), 1), "days\n")
cat("  • % Illiquid positions:", percent(mean(portfolio_with_liquidity$is_illiquid, na.rm = TRUE)), "\n")
cat("  • % Large cap:", percent(mean(portfolio_with_liquidity$is_large_cap, na.rm = TRUE)), "\n")
cat("  • Average enhanced score:", round(mean(portfolio_with_liquidity$enhanced_trade_score, na.rm = TRUE), 1), "/ 12\n")

cat("\n📁 Files created:\n")
cat("  1. Liquidity_Analysis.pdf - Visual analysis\n")
cat("  2. portfolio_with_full_liquidity_data.parquet - Full dataset\n")
cat("  3. tradeable_positions_ranked_[DATE].csv - Ranked by tradeability\n")

