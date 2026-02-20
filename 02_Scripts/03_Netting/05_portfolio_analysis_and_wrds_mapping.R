# ==============================================================================
# PORTFOLIO TRADEABILITY ANALYSIS & WRDS TICKER MAPPING
# ==============================================================================
# Purpose: 
#   1. Connect to WRDS and map stock IDs to tickers/names
#   2. Analyze portfolio concentration and tradeability
#   3. Visualize monthly portfolio characteristics
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, arrow, data.table, lubridate, 
  RPostgres, DBI,  # WRDS connection
  ggplot2, gridExtra, scales, ggridges, patchwork
)

# --- CONFIGURATION ---
BASE_DIR <- "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
OUTPUT_DIR <- file.path(BASE_DIR, "03_Outputs/Strategy")

# Input files (from previous script)
PORTFOLIO_FILE <- file.path(OUTPUT_DIR, "stock_portfolio_monthly.parquet")
RETURNS_FILE <- file.path(OUTPUT_DIR, "strategy_returns_monthly.parquet")

# Output
ANALYSIS_DIR <- file.path(BASE_DIR, "03_Outputs/Analysis")
dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# STEP 1: CONNECT TO WRDS AND GET TICKER MAPPINGS
# ==============================================================================

cat("\n=== STEP 1: Connecting to WRDS ===\n")

# Get credentials from environment variables
wrds_user <- Sys.getenv("WRDS_USER")
wrds_pass <- Sys.getenv("WRDS_PASSWORD")

if (wrds_user == "" || wrds_pass == "") {
  stop("WRDS credentials not found in environment variables. 
       Please set WRDS_USER and WRDS_PASSWORD.")
}

# Connect to WRDS
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

# ==============================================================================
# STEP 2: LOAD PORTFOLIO DATA AND EXTRACT UNIQUE STOCK IDS
# ==============================================================================

cat("\n=== STEP 2: Loading Portfolio Data ===\n")

portfolio <- read_parquet(PORTFOLIO_FILE)
setDT(portfolio)

cat("Loaded portfolio with", nrow(portfolio), "stock-month observations\n")
cat("Date range:", as.character(min(portfolio$trade_month)), "to", 
    as.character(max(portfolio$trade_month)), "\n")

# Get unique stock IDs
unique_ids <- unique(portfolio$id)
cat("Unique stocks across all months:", length(unique_ids), "\n")

# ==============================================================================
# STEP 3: MAP IDS TO TICKERS/NAMES VIA WRDS
# ==============================================================================

cat("\n=== STEP 3: Mapping Stock IDs to Tickers ===\n")

# The 'id' column in JKP data is typically 'permno' from CRSP
# We'll query CRSP to get ticker symbols and company names

# Build query to get the most recent ticker for each permno
# We'll use the CRSP stocknames table which has the full history

cat("Querying CRSP for ticker mappings...\n")

# Query CRSP MSF (Monthly Stock File) for header info
# Note: This gets the most recent available ticker for each permno
query <- sprintf("
  SELECT DISTINCT
    permno,
    ticker,
    comnam as company_name,
    ncusip,
    shrcd,
    exchcd
  FROM crsp.msenames
  WHERE permno IN (%s)
    AND namedt <= CURRENT_DATE
    AND (nameendt >= CURRENT_DATE OR nameendt IS NULL)
  ORDER BY permno
", paste(unique_ids, collapse = ","))

# Execute query
ticker_map <- dbGetQuery(wrds, query)
setDT(ticker_map)

cat("✅ Retrieved", nrow(ticker_map), "ticker mappings\n")

# For permnos not found (delisted or merged), get last known ticker
missing_permnos <- setdiff(unique_ids, ticker_map$permno)

if (length(missing_permnos) > 0) {
  cat("Looking up", length(missing_permnos), "delisted stocks...\n")
  
  query_delisted <- sprintf("
    SELECT DISTINCT ON (permno)
      permno,
      ticker,
      comnam as company_name,
      ncusip,
      shrcd,
      exchcd
    FROM crsp.msenames
    WHERE permno IN (%s)
    ORDER BY permno, nameendt DESC NULLS FIRST
  ", paste(missing_permnos, collapse = ","))
  
  ticker_map_delisted <- dbGetQuery(wrds, query_delisted)
  setDT(ticker_map_delisted)
  
  # Add marker for delisted
  ticker_map_delisted[, is_delisted := TRUE]
  ticker_map[, is_delisted := FALSE]
  
  # Combine
  ticker_map <- rbind(ticker_map, ticker_map_delisted, fill = TRUE)
  
  cat("✅ Retrieved", nrow(ticker_map_delisted), "delisted ticker mappings\n")
}

# Clean up ticker symbols
ticker_map[, ticker := str_trim(ticker)]
ticker_map[, company_name := str_trim(company_name)]

# Add exchange names
ticker_map[, exchange := case_when(
  exchcd == 1 ~ "NYSE",
  exchcd == 2 ~ "NYSE MKT",
  exchcd == 3 ~ "NASDAQ",
  exchcd == 31 ~ "NASDAQ",
  exchcd == 32 ~ "NASDAQ",
  exchcd == 33 ~ "NASDAQ",
  TRUE ~ "Other"
)]

# Add share type
ticker_map[, share_type := case_when(
  shrcd %in% c(10, 11) ~ "Common",
  shrcd %in% c(12) ~ "Common (limited voting)",
  shrcd %in% c(20, 21, 22) ~ "REIT",
  shrcd %in% c(30, 31, 32) ~ "ADR",
  TRUE ~ "Other"
)]

cat("\n--- Ticker Map Sample ---\n")
print(head(ticker_map[order(-permno)], 10))

# Disconnect from WRDS
dbDisconnect(wrds)
cat("\n✅ Disconnected from WRDS\n")

# ==============================================================================
# STEP 4: MERGE TICKER INFO INTO PORTFOLIO
# ==============================================================================

cat("\n=== STEP 4: Enriching Portfolio with Ticker Data ===\n")

portfolio_enriched <- merge(
  portfolio,
  ticker_map,
  by.x = "id",
  by.y = "permno",
  all.x = TRUE
)

# Calculate some useful metrics
portfolio_enriched[, abs_weight := abs(net_weight)]
portfolio_enriched[, position_value := abs_weight * 1000000] # Assuming $1M portfolio

# Save enriched portfolio
write_parquet(
  portfolio_enriched,
  file.path(OUTPUT_DIR, "stock_portfolio_with_tickers.parquet")
)

cat("✅ Enriched portfolio saved\n")

# ==============================================================================
# STEP 5: PORTFOLIO CONCENTRATION ANALYSIS
# ==============================================================================

cat("\n=== STEP 5: Analyzing Portfolio Concentration ===\n")

# Monthly statistics
monthly_stats <- portfolio_enriched[, .(
  n_stocks = .N,
  n_long = sum(net_weight > 0),
  n_short = sum(net_weight < 0),
  gross_exposure = sum(abs_weight),
  net_exposure = sum(net_weight),
  
  # Top positions
  top1_pct = max(abs_weight),
  top5_pct = sum(head(sort(abs_weight, decreasing = TRUE), 5)),
  top10_pct = sum(head(sort(abs_weight, decreasing = TRUE), 10)),
  
  # HHI (Herfindahl-Hirschman Index)
  hhi = sum(abs_weight^2),
  
  # Exchange distribution
  pct_nyse = sum(abs_weight[exchange == "NYSE"], na.rm = TRUE) / sum(abs_weight),
  pct_nasdaq = sum(abs_weight[exchange == "NASDAQ"], na.rm = TRUE) / sum(abs_weight),
  
  # Share type
  pct_common = sum(abs_weight[share_type == "Common"], na.rm = TRUE) / sum(abs_weight),
  
  # Factor diversification (from original data)
  avg_factors_per_stock = mean(n_factors)
), by = trade_month]

setorder(monthly_stats, trade_month)

cat("\n--- Monthly Portfolio Statistics (Recent) ---\n")
print(tail(monthly_stats, 12))

# ==============================================================================
# STEP 6: TRADEABILITY ANALYSIS
# ==============================================================================

cat("\n=== STEP 6: Tradeability Analysis ===\n")

# For each stock-month, we want to know:
# 1. Market cap (if available)
# 2. Liquidity (volume, spread)
# 3. Position size relative to ADV

# We'll need to go back to WRDS for market cap and volume data
# For now, let's analyze what we have

# Position size distribution
position_analysis <- portfolio_enriched[, .(
  min_position = min(abs_weight),
  q25_position = quantile(abs_weight, 0.25),
  median_position = median(abs_weight),
  q75_position = quantile(abs_weight, 0.75),
  max_position = max(abs_weight),
  
  # Number of "large" positions (>1% of portfolio)
  n_large = sum(abs_weight > 0.01),
  
  # Number of "tiny" positions (<0.1%)
  n_tiny = sum(abs_weight < 0.001)
), by = trade_month]

cat("\n--- Position Size Distribution (Recent) ---\n")
print(tail(position_analysis, 12))

# ==============================================================================
# STEP 7: VISUALIZATION
# ==============================================================================

cat("\n=== STEP 7: Generating Visualizations ===\n")

pdf(file.path(ANALYSIS_DIR, "Portfolio_Tradeability_Analysis.pdf"), 
    width = 14, height = 10)

# --- PAGE 1: Overview Statistics ---

# 1. Number of stocks over time
p1 <- ggplot(monthly_stats, aes(x = trade_month)) +
  geom_line(aes(y = n_stocks, color = "Total"), linewidth = 1) +
  geom_line(aes(y = n_long, color = "Long"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = n_short, color = "Short"), linewidth = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("Total" = "black", "Long" = "#27AE60", "Short" = "#E74C3C")) +
  labs(
    title = "Portfolio Size Over Time",
    subtitle = "Number of stocks held each month",
    y = "Number of Stocks",
    x = "",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 2. Concentration (HHI)
p2 <- ggplot(monthly_stats, aes(x = trade_month, y = hhi)) +
  geom_line(color = "#3498DB", linewidth = 1) +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = min(monthly_stats$trade_month), y = 0.012, 
           label = "HHI = 0.01 (Moderate Concentration)", hjust = 0, size = 3) +
  labs(
    title = "Portfolio Concentration (HHI)",
    subtitle = "Lower = more diversified",
    y = "Herfindahl-Hirschman Index",
    x = ""
  ) +
  theme_minimal()

# 3. Exposure
p3 <- ggplot(monthly_stats, aes(x = trade_month)) +
  geom_line(aes(y = gross_exposure, color = "Gross"), linewidth = 1) +
  geom_line(aes(y = abs(net_exposure), color = "Net (Absolute)"), linewidth = 1) +
  geom_hline(yintercept = 2.0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("Gross" = "#9B59B6", "Net (Absolute)" = "#E67E22")) +
  labs(
    title = "Portfolio Exposure",
    subtitle = "Gross = |Long| + |Short|, Net = Long - Short",
    y = "Exposure (times capital)",
    x = "",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4. Top concentration
p4 <- ggplot(monthly_stats, aes(x = trade_month)) +
  geom_line(aes(y = top1_pct, color = "Top 1"), linewidth = 1) +
  geom_line(aes(y = top5_pct, color = "Top 5"), linewidth = 1) +
  geom_line(aes(y = top10_pct, color = "Top 10"), linewidth = 1) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("Top 1" = "#E74C3C", "Top 5" = "#F39C12", "Top 10" = "#F1C40F")) +
  labs(
    title = "Top Position Concentration",
    subtitle = "% of portfolio in largest positions",
    y = "% of Portfolio",
    x = "",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Arrange in grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

# --- PAGE 2: Position Distribution ---

# Violin plot of position sizes by year
portfolio_enriched[, year := year(trade_month)]

p5 <- ggplot(portfolio_enriched[year >= 2010], 
             aes(x = factor(year), y = abs_weight, fill = factor(year))) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.8, outlier.size = 0.5) +
  scale_y_log10(labels = percent) +
  scale_fill_viridis_d() +
  labs(
    title = "Position Size Distribution by Year",
    subtitle = "How scattered are the weights?",
    y = "Absolute Weight (Log Scale)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)

# Ridge plot of position distribution over time
p6 <- portfolio_enriched[trade_month >= "2015-01-01"] %>%
  ggplot(aes(x = abs_weight, y = factor(year(trade_month)), fill = factor(year(trade_month)))) +
  geom_density_ridges(alpha = 0.7, scale = 2) +
  scale_x_log10(labels = percent) +
  scale_fill_viridis_d() +
  labs(
    title = "Position Weight Distribution Over Time",
    subtitle = "Density of weights for each year",
    x = "Absolute Weight (Log Scale)",
    y = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p6)

# --- PAGE 3: Exchange & Security Type Distribution ---

# Exchange over time
exchange_time <- portfolio_enriched[, .(
  total_weight = sum(abs_weight)
), by = .(trade_month, exchange)]

exchange_pct <- exchange_time[, .(
  exchange = exchange,
  pct = total_weight / sum(total_weight)
), by = trade_month]

p7 <- ggplot(exchange_pct, aes(x = trade_month, y = pct, fill = exchange)) +
  geom_area(alpha = 0.7) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Exchange Distribution Over Time",
    subtitle = "Where are the stocks traded?",
    y = "% of Portfolio Weight",
    x = "",
    fill = "Exchange"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p7)

# Share type distribution
type_time <- portfolio_enriched[, .(
  total_weight = sum(abs_weight)
), by = .(trade_month, share_type)]

type_pct <- type_time[, .(
  share_type = share_type,
  pct = total_weight / sum(total_weight)
), by = trade_month]

p8 <- ggplot(type_pct, aes(x = trade_month, y = pct, fill = share_type)) +
  geom_area(alpha = 0.7) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Security Type Distribution Over Time",
    subtitle = "Common stock vs REITs vs ADRs",
    y = "% of Portfolio Weight",
    x = "",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p8)

# --- PAGE 4: Factor Diversification ---

p9 <- ggplot(portfolio_enriched, aes(x = n_factors)) +
  geom_histogram(binwidth = 1, fill = "#3498DB", color = "white") +
  labs(
    title = "Factor Diversification per Stock",
    subtitle = "How many factors contribute to each stock position?",
    x = "Number of Factors",
    y = "Count (stock-month obs)"
  ) +
  theme_minimal()

print(p9)

# Factor diversification over time
p10 <- ggplot(monthly_stats, aes(x = trade_month, y = avg_factors_per_stock)) +
  geom_line(color = "#8E44AD", linewidth = 1) +
  geom_smooth(se = FALSE, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Average Factor Exposure per Stock",
    subtitle = "How diversified is each stock position?",
    y = "Avg # Factors per Stock",
    x = ""
  ) +
  theme_minimal()

print(p10)

dev.off()

cat("\n✅ PDF report saved to:", file.path(ANALYSIS_DIR, "Portfolio_Tradeability_Analysis.pdf"), "\n")

# ==============================================================================
# STEP 8: DETAILED POSITION ANALYSIS FOR MOST RECENT MONTH
# ==============================================================================

cat("\n=== STEP 8: Analyzing Most Recent Month ===\n")

latest_month <- max(portfolio_enriched$trade_month)
latest_portfolio <- portfolio_enriched[trade_month == latest_month]

cat("\n--- Latest Portfolio (", as.character(latest_month), ") ---\n")
cat("Total stocks:", nrow(latest_portfolio), "\n")
cat("Long positions:", sum(latest_portfolio$net_weight > 0), "\n")
cat("Short positions:", sum(latest_portfolio$net_weight < 0), "\n")
cat("Gross exposure:", round(sum(latest_portfolio$abs_weight), 2), "\n")
cat("Net exposure:", round(sum(latest_portfolio$net_weight), 2), "\n\n")

# Top 20 positions
cat("--- Top 20 Absolute Positions ---\n")
top_positions <- latest_portfolio[order(-abs_weight)][1:20, .(
  Ticker = ticker,
  Company = str_trunc(company_name, 30),
  Direction = ifelse(net_weight > 0, "LONG", "SHORT"),
  Weight = scales::percent(abs_weight, 0.01),
  Factors = n_factors,
  Exchange = exchange
)]
print(top_positions)

# Save detailed trade list for latest month
write_csv(
  latest_portfolio[order(-abs_weight), .(
    permno = id,
    ticker,
    company_name,
    exchange,
    share_type,
    direction = ifelse(net_weight > 0, "LONG", "SHORT"),
    weight = net_weight,
    weight_pct = scales::percent(abs_weight, 0.01),
    n_factors,
    position_value_1M = abs_weight * 1000000
  )],
  file.path(OUTPUT_DIR, paste0("trade_list_detailed_", latest_month, ".csv"))
)

cat("\n✅ Detailed trade list saved\n")

# ==============================================================================
# STEP 9: TRADEABILITY SCORES
# ==============================================================================

cat("\n=== STEP 9: Calculating Tradeability Scores ===\n")

# Define tradeability criteria
portfolio_enriched[, tradeability_score := 0]

# Points for being common stock
portfolio_enriched[share_type == "Common", tradeability_score := tradeability_score + 2]

# Points for major exchange
portfolio_enriched[exchange %in% c("NYSE", "NASDAQ"), tradeability_score := tradeability_score + 2]

# Points for reasonable position size (0.1% to 2%)
portfolio_enriched[abs_weight >= 0.001 & abs_weight <= 0.02, tradeability_score := tradeability_score + 2]

# Points for NOT being delisted
portfolio_enriched[is.na(is_delisted) | is_delisted == FALSE, tradeability_score := tradeability_score + 1]

# Points for lower factor concentration (more diversified source)
portfolio_enriched[n_factors >= 2, tradeability_score := tradeability_score + 1]

# Summary
tradeability_summary <- portfolio_enriched[, .(
  avg_score = mean(tradeability_score),
  pct_high_tradeable = mean(tradeability_score >= 6),
  pct_low_tradeable = mean(tradeability_score <= 3)
), by = trade_month]

cat("\n--- Recent Tradeability Scores ---\n")
print(tail(tradeability_summary, 12))

# Visualize
p_trade <- ggplot(tradeability_summary, aes(x = trade_month, y = avg_score)) +
  geom_line(color = "#27AE60", linewidth = 1) +
  geom_hline(yintercept = 6, linetype = "dashed", color = "blue", alpha = 0.5) +
  annotate("text", x = min(tradeability_summary$trade_month), y = 6.2, 
           label = "High Tradeability Threshold", hjust = 0, size = 3) +
  ylim(0, 8) +
  labs(
    title = "Portfolio Tradeability Score Over Time",
    subtitle = "Higher = easier to trade (max 8 points)",
    y = "Average Tradeability Score",
    x = ""
  ) +
  theme_minimal()

ggsave(
  file.path(ANALYSIS_DIR, "tradeability_score.png"),
  p_trade,
  width = 10,
  height = 6
)

# ==============================================================================
# STEP 10: SUMMARY REPORT
# ==============================================================================

cat("\n=== FINAL SUMMARY ===\n\n")

cat("📊 Portfolio Characteristics (Full History):\n")
cat("  • Average stocks per month:", round(mean(monthly_stats$n_stocks)), "\n")
cat("  • Average HHI (concentration):", round(mean(monthly_stats$hhi), 4), "\n")
cat("  • Average gross exposure:", round(mean(monthly_stats$gross_exposure), 2), "x\n")
cat("  • Average |net| exposure:", round(mean(abs(monthly_stats$net_exposure)), 2), "x\n\n")

cat("📈 Most Recent Month (", as.character(latest_month), "):\n")
cat("  • Total stocks:", nrow(latest_portfolio), "\n")
cat("  • Long:", sum(latest_portfolio$net_weight > 0), "| Short:", sum(latest_portfolio$net_weight < 0), "\n")
cat("  • Top position:", scales::percent(max(latest_portfolio$abs_weight), 0.01), "\n")
cat("  • Avg tradeability score:", round(mean(latest_portfolio$tradeability_score), 1), "/ 8\n\n")

cat("✅ All analysis files saved to:", ANALYSIS_DIR, "\n")
cat("\nFiles created:\n")
cat("  1. Portfolio_Tradeability_Analysis.pdf - Full visual report\n")
cat("  2. tradeability_score.png - Tradeability over time\n")
cat("  3. stock_portfolio_with_tickers.parquet - Enriched portfolio data\n")
cat("  4. trade_list_detailed_[DATE].csv - Ready-to-trade list with tickers\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
