# ==============================================================================
# JKP DATA: DEEP DIVE & TRADING READINESS AUDIT (CORRECTED)
# Purpose: Inspect Raw Stock Data and Calculate Net Positions (Share Counts)
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, skimr, lubridate, scales)

# --- CONFIGURATION ---
setwd("/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code")
CHAR_FILE <- "01_Data/Raw/USA.parquet"

# ------------------------------------------------------------------------------
# 1. LOAD DATA (Latest Snapshot)
# ------------------------------------------------------------------------------
cat("--- 1. Loading Latest Snapshot ---\n")

# Use 'open_dataset' to find the latest date efficiently
ds_char <- open_dataset(CHAR_FILE)
latest_date <- ds_char %>% 
  select(date) %>% 
  summarise(max(date)) %>% 
  collect() %>% 
  pull()

cat("Latest Date:", as.character(latest_date), "\n")

# Load only the latest month
snapshot <- read_parquet(CHAR_FILE) %>%
  filter(date == latest_date)

# ------------------------------------------------------------------------------
# 2. STATISTICAL SUMMARY
# ------------------------------------------------------------------------------
cat("\n--- 2. Universe Summary ---\n")

snapshot %>%
  summarise(
    Total_Stocks = n(),
    Total_Market_Cap_Trillion = sum(me, na.rm=T) / 10^6,
    # FIXED: Using 'ret_12_7' instead of 'r_12_7'
    Stocks_with_Momentum = sum(!is.na(ret_12_7)), 
    Stocks_with_Value = sum(!is.na(be_me)),
    # CHECK: Do we have Price to calculate shares?
    Stocks_with_Price = sum(!is.na(prc))
  ) %>%
  print()

# ------------------------------------------------------------------------------
# 3. TRADING SIMULATION: $10M PORTFOLIO
# ------------------------------------------------------------------------------
cat("\n--- 3. Constructing $10M Net Position Portfolio ---\n")

# Target: Buy Top 50 Momentum Stocks
portfolio <- snapshot %>%
  filter(!is.na(ret_12_7)) %>%      # Filter for valid Momentum
  filter(!is.na(prc) & prc > 5) %>% # Filter for tradable Price (> $5)
  arrange(desc(ret_12_7)) %>%       # Sort Winners
  slice(1:50) %>%                   # Pick Top 50
  select(id, Ticker_ID = id, Momentum = ret_12_7, Price = prc, MarketCap = me) %>%
  mutate(
    Total_Capital = 10000000,      # $10 Million Fund
    Weight = 1 / n(),              # Equal Weight (2% each)
    Target_Value = Total_Capital * Weight,
    
    # THE "NET POSITION" CALCULATION
    # Formula: Cash Allocation / Price per Share = Share Count
    Shares_to_Buy = floor(Target_Value / Price)
  )

cat("Portfolio Strategy: Equal Weight Top 50 Momentum Stocks\n")
cat("Capital Allocated: $", format(sum(portfolio$Target_Value), big.mark=","), "\n")
cat("Total Shares to Buy:", format(sum(portfolio$Shares_to_Buy), big.mark=","), "\n\n")

print(head(portfolio, 10))

# ------------------------------------------------------------------------------
# 4. EXPORT "EXECUTION FILE"
# ------------------------------------------------------------------------------
# This is what you would send to a "Broker"
execution_file <- portfolio %>%
  select(Ticker_ID, Shares_to_Buy, Price, Target_Value)

write.csv(execution_file, "03_Outputs/Reports/Trade_Execution_List.csv", row.names=FALSE)
cat("\n✅ Trade Execution File saved to: 03_Outputs/Reports/Trade_Execution_List.csv\n")