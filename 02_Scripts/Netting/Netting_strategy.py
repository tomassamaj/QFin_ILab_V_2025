
import polars as pl
import numpy as np
import os

# --- CONFIGURATION ---
DATA_DIR    = "data/processed"
TRADES_FILE = f"{DATA_DIR}/intermediate/final_trades.parquet"
STOCKS_FILE = f"{DATA_DIR}/return_data/daily_rets_by_country/USA.parquet"
OUTPUT_FILE = f"{DATA_DIR}/intermediate/strategy_performance.parquet"

print(f"--- 1. LOADING DATA ---")
trades = pl.scan_parquet(TRADES_FILE)
stocks = pl.scan_parquet(STOCKS_FILE).select(["date", "id", "ret"])

print(f"--- 2. EXECUTING STRATEGY ---")

# Join and Calculate Contribution
# FIX: We fill null returns with 0.0 to prevent NaN propagation
portfolio = (
    trades.join(stocks, on=["date", "id"], how="inner")
    .with_columns([
        pl.col("ret").fill_null(0.0),
        pl.col("net_weight").fill_null(0.0)
    ])
    .with_columns([
        (pl.col("net_weight") * pl.col("ret")).alias("contribution")
    ])
)

# Aggregate to Daily Portfolio Return
daily_perf = (
    portfolio.group_by("date")
    .agg([
        pl.sum("contribution").alias("daily_ret"),
        pl.sum("net_weight").abs().alias("gross_leverage"),
        pl.count("id").alias("n_positions")
    ])
    .sort("date")
    .collect()
)

# Check for NaNs in the daily series before compounding
n_nans = daily_perf.filter(pl.col("daily_ret").is_nan()).height
if n_nans > 0:
    print(f"WARNING: Found {n_nans} days with NaN returns. Filling with 0.0.")
    daily_perf = daily_perf.with_columns(pl.col("daily_ret").fill_nan(0.0))

# Calculate Cumulative Return
daily_perf = daily_perf.with_columns([
    (1 + pl.col("daily_ret")).cum_prod().alias("cum_ret")
])

print(f"--- 3. CALCULATING STATISTICS ---")

# Filter out the first day if it starts at 0 or 1 arbitrarily
start_val = daily_perf["cum_ret"][0]
end_val   = daily_perf["cum_ret"][-1]

# Annualize
mean_daily = daily_perf["daily_ret"].mean()
std_daily  = daily_perf["daily_ret"].std()
ann_ret    = mean_daily * 252
ann_vol    = std_daily * (252 ** 0.5)
sharpe     = ann_ret / ann_vol

print("\n" + "="*40)
print(f" STRATEGY PERFORMANCE REPORT")
print(f" Period: {daily_perf['date'][0]} to {daily_perf['date'][-1]}")
print("="*40)
print(f" Total Return:      {end_val:.2f} x")
print(f" Annualized Return: {ann_ret:.2%}")
print(f" Annualized Vol:    {ann_vol:.2%}")
print(f" Sharpe Ratio:      {sharpe:.2f}")
print("="*40 + "\n")

# Save Result
daily_perf.write_parquet(OUTPUT_FILE)
print("✅ Backtest Complete.")
