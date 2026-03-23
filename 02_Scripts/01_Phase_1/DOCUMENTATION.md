# Phase 1 Documentation — Factor-Level Replication

**Script**: `Arnott_Replication_Full_Grid.R`
**Purpose**: Cross-sectional factor momentum using JKP daily factors — full parameter grid replication of Arnott, Kalesnik & Linnainmaa (2023).

---

## Input Data

| File | Description |
|---|---|
| `01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet` | 36 human-readable JKP factors, daily VW-cap L-S returns |

Benchmarks downloaded live:
- `frenchdata::download_french_data("Fama/French 3 Factors [Daily]")` — market factor (Mkt-RF)
- `frenchdata::download_french_data("17 Industry Portfolios [Daily]")` — industry momentum benchmark

---

## Strategy

- **Signal**: Rolling log-return over a lookback window (1M/2M/3M/6M/12M)
- **Rebalancing**: Every 21 trading days (monthly), fixed holding period
- **Implementation lag**: 1 day (signal at T, trade at T+1 open / T close)
- **Split rules**: LS_Median (50/50), LS_33 (33/67), LS_25 (25/75), and LO variants
- **Universe**: 36 JKP core factors (USA)
- **Annualization**: 252/21 ≈ 12 periods/year

---

## Parameter Grid

| Parameter | Values |
|---|---|
| Lookback (days) | 21, 42, 63, 126, 252 (1M–12M) |
| Strategy type | LS_Median, LS_33, LS_25, LO_Median, LO_33, LO_25 |
| Holding days | 21 (fixed) |

Total combinations: 30

---

## Outputs

All figures saved to `03_Outputs/Figures/`, table to `03_Outputs/Tables/`.

### Core Grid Results
| File | Description |
|---|---|
| `Arnott_Grid_Sharpe_Heatmap.pdf` | Sharpe ratio across all 30 strategy×lookback combos |
| `Arnott_Grid_AnnReturn_Heatmap.pdf` | Annualized return heatmap |
| `Arnott_Full_Grid_CumWealth.pdf` | Faceted cumulative wealth for all 30 combos |
| `Arnott_Top3_vs_Benchmarks.pdf` | Best 3 strategies vs market + industry momentum |
| `Arnott_LS_vs_LO_Comparison.pdf` | LS vs LO at 1M lookback (all split rules) |
| `Arnott_Sharpe_BarChart.pdf` | Sharpe bar chart by strategy type and lookback |
| `Arnott_AnnReturn_BarChart.pdf` | Annualized return bar chart |
| `Arnott_Grid_Performance.csv` | Full performance table (Ann Ret, Sharpe, Max DD, Calmar) |

### Replication Plots
| File | Description |
|---|---|
| `Arnott_Focused_CumReturn.pdf` | 4-strategy focused comparison vs benchmarks |
| `Arnott_2023_Replication.pdf` | Main paper replication: Factor Mom vs Industry Mom vs Market |
| `Arnott_2023_Exact_1DayLag.pdf` | Arnott exact (1-day lag) vs our 2-day lag implementation |

### Exploratory Analyses
| File | Description |
|---|---|
| `Arnott_Factor_Correlation_Heatmap.pdf` | Factor return correlation matrix (corrplot, hclust order) |
| `Arnott_Factor_Spaghetti.pdf` | Individual factor cumulative wealth (all 36) |
| `Arnott_LS_Decomp_LongShort.pdf` | Long vs short leg decomposition with crisis shading |
| `Arnott_Drawdown_Analysis.pdf` | Drawdown from peak for key strategies and benchmarks |
| `Arnott_Subperiod_Pre_Post_2000.pdf` | Pre-2000 vs post-2000 faceted comparison |
| `Arnott_VolScaled_Momentum.pdf` | Volatility-scaled momentum (target 15% ann vol) |
| `Arnott_Cluster_Momentum.pdf` | Factor cluster analysis (5 clusters, Ward.D2) |
| `Arnott_PCA_Scree.pdf` | PCA scree plot (variance explained by component) |
| `Arnott_PCA_Momentum.pdf` | PCA momentum vs full factor momentum |

---

## Canonical Color Palette

Defined in the script config section — used consistently across all plots:

| Variable | Hex | Usage |
|---|---|---|
| `COL_FACTOR_MOM` | `#1565C0` | Factor Momentum LS (canonical blue) |
| `COL_FACTOR_LO` | `#42A5F5` | Factor Momentum LO (lighter blue) |
| `COL_INDUSTRY_MOM` | `#E53935` | Industry Momentum (canonical red) |
| `COL_MARKET` | `#424242` | Market / Mkt-RF (dark gray) |
| `COL_LS_LONG` | `#2ca02c` | Long leg (green) |
| `COL_LS_SHORT` | `#d62728` | Short leg / inverted (dark red) |
| `COL_VOLSCALE_LO` | `#2e7d32` | LO raw (dark green) |
| `COL_VOLSCALE_LO2` | `#66bb6a` | LO vol-scaled (light green) |

---

## Key Results

| Strategy | Ann Ret | Sharpe | Max DD |
|---|---|---|---|
| LS Median 1M | ~5–6% | 0.42 | ~-55% |
| LO Median 1M | ~8–9% | 0.98 | ~-30% |
| LO Top-25% 1M | ~7–8% | 0.82 | ~-32% |
| Industry Mom 1M | ~3–4% | ~0.30 | — |

---

## R Packages Required

```r
pacman::p_load(
  tidyverse, arrow, lubridate, zoo, ggplot2, scales, ggthemes,
  frenchdata, gt, patchwork, RColorBrewer, PerformanceAnalytics,
  corrplot, factoextra
)
```

---

## Notes

- Phase 1 uses **21-day rolling signals** evaluated at monthly rebalancing points — this is NOT exactly Arnott (2023) paper methodology (paper uses monthly calendar returns). The rolling approach approximately doubles the Sharpe ratio vs. monthly-only signals.
- The paper reports SR ≈ 0.78 using FF-style 2×3 double-sorted factors with broader cross-sectional spread. JKP tercile-sorted VW factors produce smaller L-S spreads, hence lower SR.
- Industry momentum benchmark constructed from FF 17 Industry daily portfolios using the same rolling signal and holding period.
