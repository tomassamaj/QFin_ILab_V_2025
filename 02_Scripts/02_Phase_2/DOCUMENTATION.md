# Phase 2 R Scripts — Documentation

## Project Context

Phase 2 translates the factor-level factor momentum strategy (Phase 1) into a
**single-stock portfolio** using JKP constituent weights from the cluster.
The core idea: instead of holding factor portfolios, hold the underlying stocks
directly, netting long/short exposures across all active factors for each stock.

**Key data assets driving this phase:**
| File | Description |
|---|---|
| `usa_factor_weights.parquet` | 87M rows — stock weights per factor × month-end |
| `pfs_daily.parquet` | Daily VW-cap portfolio sorts per factor (used for factor signals) |
| `USA_daily_rets.parquet` | Daily excess stock returns |
| `arnott_master.parquet` | Pre-joined file: stock weights + `ret_exc_lead1m` + `ret_day1` |
| `[usa]_[all_factors]_[monthly]_[vw_cap].csv` | Official JKP monthly factor benchmark |

---

## Script-by-Script Overview

---

### 01 — `01_Netting_from_JKP_Cluster_weights_2026_02_05.R`

**Role: Core strategy script (main backtest pipeline)**

**What it does:**
1. Loads `usa_factor_weights.parquet`, filters to 39 target factors (post-1960)
2. Applies direction corrections (`factors_to_flip` list, hardcoded) so all factors point "long = good"
3. Reads `pfs_daily.parquet`, constructs daily L-S factor returns (high pf − low pf)
4. Generates monthly momentum signals: 21-day rolling log-return sum, sampled at month-end
5. Ranks factors cross-sectionally → assigns weights (top-25% long-only or median L/S)
6. Merges factor weights × stock constituent weights → nets to stock-level positions
7. Backtests on daily stock returns (`USA_daily_rets.parquet`), holding weights for the full next month
8. Produces event studies (Dot-Com, COVID), crisis diagnostics

**Data in:**
- `usa_factor_weights.parquet`
- `pfs_daily.parquet`
- `USA_daily_rets.parquet` (for stock-level backtest)

**Data out:**
- `final_monthly_strategy_weights_1960.parquet` (netted stock positions)


---

### 02 — `02_flip_and_factor_from_single_stock.R`

**Role: Audit / validation + early daily strategy exploration**

**Part A — Bottom-up replication audit:**
- Reconstructs factor returns from stock weights × monthly returns
- Compares to official JKP benchmark (`[usa]_[all_factors]_[monthly]_[vw_cap].csv`)
- Auto-detects factors with negative correlation → produces the `factors_to_flip` list
- Saves corrected weights to `usa_factor_weights_corrected.parquet`
- Generates PDF report: `Bottom_Up_Replication_Report.pdf`, `Audit_Report_All_Factors.pdf`

**Part B — Daily factor momentum (exploratory):**
- Applies sign corrections, generates monthly signals at factor level
- Backtests with execution lag (first trading day of month = 0 weight)
- Applies 60-day rolling volatility scaling to 10% target vol

**Data in:**
- `usa_factor_weights.parquet`
- `Global_Stock_Returns_Monthly.parquet`
- `[usa]_[all_factors]_[monthly]_[vw_cap].csv` (official benchmark)
- `pfs_daily.parquet`

**Data out:**
- `usa_factor_weights_corrected.parquet`
- `Bottom_Up_Replication_Report.pdf`, `Audit_Report_All_Factors.pdf`


---

### 03 — `03_daily_to_monthly_audit.R`

**Role: Validate daily → monthly frequency aggregation**

**What it does:**
- Loads `pfs_daily.parquet`, compounds daily returns within each month
- Compares to official JKP monthly benchmark
- Identifies and applies sign corrections
- Saves corrected monthly factor returns and a sign correction CSV

**Data in:**
- `pfs_daily.parquet`
- `[usa]_[all_factors]_[monthly]_[vw_cap].csv`

**Data out:**
- `monthly_factors_from_daily.parquet`
- `factor_sign_corrections.csv`
- `Daily_to_Monthly_Audit.pdf`

---

### 04 — `04_real_world_factor_momentum_implementation.R`

**Role: Large script — factor-level grid search + single-stock netting + WRDS enrichment + multiple backtests**

**What it does (multiple sections):**

1. **Factor-level backtest (1963 start):** Single strategy (top-50% L/S), with drawdown chart
2. **Grid search:** 6 combinations of position type (L/S, L-only) × top percentile (25/33/50%); prints Sharpe table
3. **Single-stock netting:** Converts factor weights → stock weights using `usa_factor_weights.parquet`; applies flip corrections; aggregates by stock ID
4. **WRDS ticker enrichment:** Connects to CRSP `msenames`, maps PERMNOs to tickers/company names
5. **Monthly backtest with outlier cleaning:** Clips extreme returns (±1000%), runs 3 variants (no trim, 1bp trim, 5bp trim)
6. **Daily backtest with JKP cutoffs:** Uses dynamic return cutoffs (0.1–99.9 percentiles), normalizes daily
7. **"Gold standard" VW backtest:** Value-weighted L/S overlay on market benchmark → "smart beta" comparison
8. **Loads cutoff reference files** (nyse_cutoffs, return_cutoffs_daily) as a reference check

**Data in:**
- `pfs_daily.parquet`
- `usa_factor_weights.parquet`
- `Global_Stock_Returns_Monthly.parquet`
- `USA_daily_rets.parquet`
- `nyse_cutoffs.parquet`, `return_cutoffs.parquet`, `return_cutoffs_daily.parquet`
- WRDS (CRSP `msenames`)

**Data out:**
- `final_strategy_single_stocks.parquet`
- `final_strategy_single_stocks_enriched.parquet`


---

### 05 — `05_portfolio_analysis_and_wrds_mapping.R`

**Role: Portfolio tradeability and concentration analysis (post-enrichment)**

**What it does:**
- Loads enriched portfolio (`stock_portfolio_monthly.parquet`) produced by script 04
- Connects to WRDS, maps stock IDs to tickers/names, handles delisted stocks
- Computes monthly stats: stock counts, HHI concentration, gross/net exposure, top-N concentration, exchange/share-type breakdowns
- Produces position distribution violins, ridge plots, exchange/share-type area charts
- Calculates a **tradeability score** (0–8 points) per position based on: common stock (+2), major exchange (+2), reasonable position size (+2), not delisted (+1), ≥2 factors (+1)
- Outputs PDF report and a detailed ready-to-trade CSV for the most recent month

**Data in:**
- `stock_portfolio_monthly.parquet` (from script 04 — may not exist)
- `strategy_returns_monthly.parquet` (from script 04 — may not exist)
- WRDS (CRSP `msenames`)

**Data out:**
- `stock_portfolio_with_tickers.parquet`
- `Portfolio_Tradeability_Analysis.pdf`
- `trade_list_detailed_[DATE].csv`
- `tradeability_score.png`


---

### 06 — `06_enhanced_liquidity_analysis.R`

**Role: Quantitative liquidity analysis using real market cap and volume data**

**What it does:**
- Loads `stock_portfolio_with_tickers.parquet` from script 05
- Connects to WRDS CRSP MSF, pulls monthly price/volume/market cap for the last 5 years
- Computes "days to trade" per position: `position_size / (10% × ADV)`
- Classifies positions: micro/small/mid/large/mega cap; liquid vs. illiquid (>5 days)
- Produces enhanced tradeability score (0–12 points) incorporating market cap, liquidity, exchange, security type, position size
- Outputs PDF and ranked CSV

**Data in:**
- `stock_portfolio_with_tickers.parquet`
- WRDS CRSP MSF (market cap, volume)

**Data out:**
- `portfolio_with_full_liquidity_data.parquet`
- `Liquidity_Analysis.pdf`
- `tradeable_positions_ranked_[DATE].csv`


---

### 07 — `07_net_weights_analysis.R`

**Role: Sanity checks and diagnostics on final stock-level weights + local backtest**

**Part A — Weight diagnostics (on `arnott_stock_weights.parquet`):**
- Checks gross exposure = 1.0, net exposure ≈ 0
- Concentration: top-10 holdings as % of portfolio over time
- **Turnover:** Monthly one-way turnover = Σ|w_t − w_{t-1}| / 2; annualizes to ~600%
- Position counts (long vs. short) over time
- Weight distribution histograms, single-stock weight traces, 2023 top-20 heatmap
- Whale detector: max single-stock weight ever

**Part B — Local backtest (on `arnott_master.parquet`):**
- Runs standard vs. lagged returns: `ret_exc_lead1m` vs. `ret_exc_lead1m − ret_day1`
- Calculates turnover alongside performance
- Plots cumulative returns vs. Fama-French excess market return
- Prints annualized performance summary

**Data in:**
- `arnott_stock_weights.parquet` (from cluster pipeline)
- `arnott_master.parquet` (pre-joined: weights + returns)

**Data out:**
- Charts/plots (in-session only)


---

### 08 — `08_Limit_no_of_positions.R`

**Role: Position reduction — filter to extreme weight deciles only**

**What it does:**
- Loads `arnott_master.parquet`
- Computes monthly weight percentiles (2.5th, 5th, 95th, 97.5th)
- **Version A:** Keeps top + bottom 5% of weights (10% of positions total) → rescales to gross=1
- **Version B:** Keeps top + bottom 2.5% of weights (5% of positions total) → rescales to gross=1
- Saves both versions

**Data in:**
- `arnott_master.parquet` (old Windows path `C:/WU/...` — needs updating!)

**Data out:**
- `arnott_master_dec_ls.parquet` (5%/5% filter)
- `arnott_master_top5_ls.parquet` (2.5%/2.5% filter)

---

### 09 — `09_Frictions.R`

**Role: Full real-world cost model — the final performance estimate**

**What it does:**
1. Loads `arnott_master.parquet` (stock weights + `ret_exc_lead1m` + `ret_day1`)
2. Loads `USA.parquet` for market cap (`me`) and dividend yield (`div1m_me`) via rolling join
3. Computes per-stock friction rates:
   - **Transaction costs:** `TC_BASE=5bps + TC_SLOPE=1bps × (ref_mcap/me)^0.25`, capped at 100bps
     - `ref_mcap` = median of top-500 stocks by market cap each month
   - **Shorting costs:** `SC_BASE=25bps + SC_SLOPE=10bps × (ref_mcap/me)^0.5`, capped at 500bps/year
   - **Dividend tax:** 27.5% withholding on short positions' dividends
4. Calculates monthly cost drags:
   - TC drag = Σ |Δweight| × tc_bps per month (on turnover)
   - SC drag = Σ |short_weight| × sc_annual_bps / 12 per month
   - Tax drag = Σ |short_weight| × div1m_me × 0.275 per month
5. Builds layered performance: Gross → net of TC → net of SC → net of Tax
6. Produces waterfall visualization and final performance table

**Data in:**
- `arnott_master.parquet`
- `USA.parquet` (for `me`, `div1m_me`)

**Data out:**
- Plots and `table.AnnualizedReturns` output (in-session only)
- Also contains start of a "Drift-Band Rebalancing Analysis" section (truncated)

---


