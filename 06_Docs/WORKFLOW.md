# Factor Momentum Replication — Full Workflow

Arnott, Kalesnik, Linnainmaa (2023, RFS) — Phase 1 & Phase 2

---

## Project Structure

```
ILab_Code/
├── 01_Data/
│   ├── Raw/
│   └── Processed/
│       ├── USA_daily_rets.parquet        JKP daily factor returns (USA)
│       ├── usa_factor_weights.parquet    Stock-level factor constituents
│       ├── USA_stocks_char.parquet       Monthly stock characteristics
│       └── Phase2/
│           ├── phase2_factor_returns.parquet   Factor-level monthly returns (30 combos)
│           ├── phase2_stock_weights.parquet     Stock-level netted weights
│           ├── phase2_master.parquet            Master: weights + returns + ME + divs
│           ├── phase2_summary.csv              Factor-level performance (748 months)
│           ├── phase2_stock_summary.csv        Stock-level performance (518 months)
│           ├── phase2_frictions_*.parquet      Frictions time-series (per strategy/lb)
│           └── phase2_frictions_summary_*.csv  Frictions summary tables
├── 02_Scripts/
│   ├── 01_Phase_1/
│   │   └── Arnott_Replication_Full_Grid.R      Phase 1: factor-level grid
│   └── 04_Frictions/
│       └── Phase2_Frictions_Tradability.R      Phase 2: stock-level + frictions
├── 03_Outputs/
│   ├── Figures/                                Phase 1 plots
│   ├── Tables/                                 Phase 1 CSV tables
│   └── Frictions/                              Phase 2 frictions PDFs
├── 06_Docs/
│   └── WORKFLOW.md                             This file
└── jkp-data-replication/
    └── code/
        └── arnott_phase2_full_grid.py          Phase 2 Python pipeline
```

---

## Step-by-Step: Run Order

### Phase 1 — Factor-Level Replication (R)

```bash
cd ILab_Code
Rscript 02_Scripts/01_Phase_1/Arnott_Replication_Full_Grid.R
```

**What it does:**
- Loads JKP daily factor returns (`USA_daily_rets.parquet`)
- Computes rolling 21-day factor momentum signals (DAILY rebalancing)
- Runs 5 lookbacks × 6 strategies = 30 combinations
- Saves performance table → `03_Outputs/Tables/Arnott_Grid_Performance.csv`
- Saves heatmaps, wealth curves, comparison plots → `03_Outputs/Figures/`

**Runtime:** ~2 minutes

---

### Phase 2 — Stock-Level Decomposition (Python, run on cluster)

```bash
cd jkp-data-replication
uv run python code/arnott_phase2_full_grid.py
```

**What it does:**
- Loads JKP daily factor returns → computes 1-day-lagged monthly factor returns
- Builds factor momentum signals (MONTHLY calendar, prior 1M/2M/3M/6M/12M)
- Aggregates factor weights × stock constituents → netted stock weights
- Attaches stock characteristics (ME, ret_exc_lead1m, div12m_me)
- Outputs 5 parquet/CSV files including `phase2_master.parquet`

**Runtime:** ~15-30 min on cluster (73M row output)

**Key outputs:**
| File | Description |
|------|-------------|
| `phase2_factor_returns.parquet` | Factor-level monthly returns, 30 combos, lag-adjusted |
| `phase2_master.parquet` | 73M rows: weights + returns + ME + dividends per stock/month/strategy |
| `phase2_summary.csv` | Factor-level SR (748 months, full period) |
| `phase2_stock_summary.csv` | Stock-level SR (518 months, where stock data exists) |

---

### Phase 2 — Frictions & Tradability (R)

```bash
cd ILab_Code
Rscript 02_Scripts/04_Frictions/Phase2_Frictions_Tradability.R
```

**Configure** `FOCUS_STRATEGY` and `FOCUS_LOOKBACKS` at the top of the script:
```r
FOCUS_STRATEGY  <- c("CS_LO_50", "CS_LO_25")   # or "CS_LS_50"
FOCUS_LOOKBACKS <- c("1M", "6M", "12M")
```

**What it does:**
- Applies transaction costs, short-sale borrow costs, dividend withholding tax
- Tests 5 concentration versions (full, min5bps, top10/5/2.5pct)
- Adds 1-day implementation lag correction using factor-level lag-adjusted returns
- Outputs parquet time-series, CSV summaries, and 2 PDFs per strategy×lookback

**Runtime:** ~5-10 minutes per strategy×lookback combo

---

## Performance Comparison: Phase 1 vs Phase 2

### What each phase measures

| | Phase 1 (R) | Phase 2 stock gross | Phase 2 lag-adj | Phase 2 net frictions |
|--|--|--|--|--|
| Signal | Rolling 21-trading-day log-return | Prior calendar month full return | Prior calendar month full return | Prior calendar month full return |
| Rebalancing | Every 21 trading days (drifts from calendar) | Calendar month-end | Calendar month-end | Calendar month-end |
| Implementation lag | 1 day (explicit) | None (uses full-month ret) | 1 day (realistic) | 1 day + TC/SC/tax |
| Universe | 36 JKP factors | 39 JKP factors, ~4,700 stocks/mo | Same | Same |
| Period | 1963–2023 (730mo) | 1985–2024 (518mo) | Same | Same |

### CS_LO_50 (Long-Only at Factor Level, Median Factor Split)

| Metric | Phase 1 (1M daily) | Stock gross (1M) | Lag-adj (1M) | Net+lag (1M) | Stock gross (12M) | Lag-adj (12M) | Net+lag (12M) |
|--|--|--|--|--|--|--|--|
| Ann Return | 5.4% | 4.7% | 3.4% | 2.1% | 5.6% | 3.8% | 2.7% |
| Ann Vol | 5.5% | 6.5% | 5.9% | 5.9% | 6.2% | 5.3% | 5.3% |
| Sharpe | **0.98** | 0.72 | 0.57 | 0.36 | **0.90** | 0.72 | 0.51 |

### Why does performance fall step by step?

```
Phase 1 daily (SR=0.98)
  ↓ Switch to monthly calendar rebalancing
Phase 2 stock gross (SR=0.72–0.90)      [−0.3 to −0.1 SR]
  Cause: Daily rebalancing in Phase 1 has lower vol (5.5% vs 6.5%)
         because it smooths the portfolio continuously.
         Returns are similar (5.4% vs 4.7–5.6%).
  ↓ Apply 1-day implementation lag
Phase 2 lag-adjusted gross (SR=0.57–0.72)  [−0.15 to −0.18 SR]
  Cause: Missing the first trading day of each month costs ~1.5–1.8% pa.
         This is unavoidable in real implementation unless you trade at
         the formation-month closing price.
  ↓ Apply frictions (TC + SC + Dividend Tax)
Phase 2 net (SR=0.36–0.51)              [−0.21 SR]
  Cause: Total drag ~1.1% pa:
    TC = 0.6–0.7% pa  (turnover × small-cap illiquidity)
    SC = 0.2% pa      (borrow costs on short stock legs)
    Div Tax = 0.3% pa (27.5% Austrian rate on short dividends)
```

### CS_LO_25 (Top Quartile of Factors, Long-Only)

| Lookback | Stock gross SR | Lag-adj SR | Net+lag SR | Net return |
|--|--|--|--|--|
| 1M | 0.52 | 0.43 | 0.27 | 2.2% |
| 6M | 0.62 | 0.52 | 0.38 | 3.2% |
| 12M | 0.75 | 0.61 | 0.45 | 3.3% |

---

## Key Methodological Differences: Phase 1 vs Arnott (2023)

| Feature | Arnott (2023) paper | Phase 1 R (our code) | Phase 2 Python (our code) |
|--|--|--|--|
| Signal horizon | Prior calendar month | Rolling 21 trading days | Prior calendar month ✓ |
| Rebalancing | Monthly (calendar) | Daily | Monthly (calendar) ✓ |
| Factor universe | 43 factors (2×3 FF sort) | 36 JKP factors | 39 JKP factors |
| Factor construction | Above/below median EW L-S | Above/below median EW L-S | Above/below median EW L-S ✓ |
| Implementation lag | Not stated (paper = 0) | None | 1 day (realistic) |
| Sample | Jul 1963 – Dec 2021 | 1963–2023 | 1963–2024 |

Phase 1 rebalances every 21 **trading** days (not calendar month-ends), so rebalancing dates slowly drift out of sync with the calendar. The signal is a rolling 21-day log-return sum, not a clean prior-calendar-month return. Both approaches are monthly in spirit, but the calendar alignment difference + slightly different signal window explains the SR gap — not "daily rebalancing".

---

## Friction Model Parameters

```r
# Transaction costs (power-law by market cap)
TC: pmin(100bps, 5 + 1 × (median_top500_ME / stock_ME)^0.25)

# Short-sale borrow costs (annual, power-law)
SC: pmin(500bps/yr, 25 + 10 × (median_top500_ME / stock_ME)^0.50) / 12

# Dividend withholding tax (Austrian, on short positions)
Tax: |weight| × (div12m_me / 12) × 0.275
```

---

## Strategy Labels

| Code | Meaning |
|------|---------|
| CS_LS_50 | Cross-Sec Long-Short, top/bottom 50% (Arnott median canonical) |
| CS_LS_33 | Cross-Sec Long-Short, top/bottom 33% |
| CS_LS_25 | Cross-Sec Long-Short, top/bottom 25% |
| CS_LO_50 | Cross-Sec, Long-Only at FACTOR level, top 50% factors held |
| CS_LO_33 | Cross-Sec, Long-Only at FACTOR level, top 33% factors held |
| CS_LO_25 | Cross-Sec, Long-Only at FACTOR level, top 25% factors held |

Note: LO strategies are long-only at the **factor selection** level. Each JKP factor
is itself long-short at stock level, so LO strategies still have negative stock weights.
