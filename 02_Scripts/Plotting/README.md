# Plotting Scripts

This folder contains four visualization scripts. Each serves a distinct purpose.

| Script | Lines | Purpose |
|---|---|---|
| `Plot_Replication_Clean.R` | 4,882 | **Main script.** Monthly factor replication plots — cumulative returns, correlations, and comparison vs. JKP benchmarks. Uses the SQLite database (`data/tidy_finance_r.sqlite`). |
| `Plot_Replication_Clean_DAILY.R` | 640 | Daily factor replication — rolling Sharpe, correlation heatmaps, and Fama-French comparison using daily frequency. Uses `frenchdata` and SQLite. |
| `Plot_Repication_Clean_DAILY_from_JKP.R` | 376 | JKP real-world factor momentum with 1-day trading delay. Reads from `01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet`. Output: `03_Outputs/Figures/JKP_Momentum_RealWorld_Replication.pdf`. |
| `other_factors_plot.R` | 89 | 6-factor panel replication (2×3 grid). Lightweight script for quick visual checks across all six factors. Uses SQLite + benchmark CSVs from `01_Data/Benchmark/`. |

## Typical Usage Order

1. Run `other_factors_plot.R` for a quick sanity check on factor replication quality.
2. Run `Plot_Replication_Clean.R` for the full monthly analysis and presentation-quality outputs.
3. Run `Plot_Replication_Clean_DAILY.R` for daily-frequency checks.
4. Run `Plot_Repication_Clean_DAILY_from_JKP.R` for the real-world momentum replication figure.

> Note: The typo in `Plot_Repication_Clean_DAILY_from_JKP.R` ("Repication") is intentional — preserved to avoid breaking any existing references.
