# ILab Factor Momentum — QFin Lab Codebase

Replication and extension of **Arnott, Kalesnik & Linnainmaa (2023) "Factor Momentum"** (Review of Financial Studies).

The project is organized in two phases:
- **Phase 1**: Factor-level replication using JKP daily factors
- **Phase 2**: Translation to a single-stock portfolio via constituent-weight netting

---

## Repository Structure

```
ILab_Code/
├── 01_Data/                        # Data files (gitignored — stored locally)
│   ├── Raw/                        # JKP source CSVs and parquets
│   ├── Processed/                  # Intermediate & final processed data
│   │   └── Phase2/                 # Phase 2 stock-level data
│   ├── Clean_Daily_Inputs/         # Cluster outputs (arnott_master.parquet etc.)
│   └── Benchmark/                  # JKP benchmark factor CSVs
│
├── 02_Scripts/
│   ├── 01_Phase_1/
│   │   ├── Arnott_Replication_Full_Grid.R   # MAIN Phase 1 script
│   │   └── DOCUMENTATION.md                 # Phase 1 documentation
│   ├── 02_Phase_2/
│   │   ├── Arnott_Phase2_Single_Stock_Full.R  # MAIN Phase 2 script
│   │   ├── DOCUMENTATION.md                   # Phase 2 documentation
│   │   └── legacy/                            # Original 9 individual scripts
│   ├── 99_Legacy/                  # Pre-consolidation exploratory scripts
│   └── Utils/                      # Helper scripts (rename map, SQLite)
│
├── 03_Outputs/
│   ├── Figures/                    # All plots (Phase 1 PDFs + Phase 2 PNGs)
│   ├── Reports/                    # Factor audit & liquidity reports (PDFs)
│   ├── Tables/                     # Performance CSV tables
│   └── Archive/                    # Legacy outputs that cannot be regenerated
│
├── 04_Presentations/               # PowerPoint slides from ILab sessions
├── 05_Docs/
│   └── Papers/                     # Reference papers (Arnott 2023, JKP, etc.)
│
└── jkp-data-replication/           # Python cluster code (not tracked by git)
```

---

## How to Run

### Phase 1 — Factor-Level Replication
```r
Rscript 02_Scripts/01_Phase_1/Arnott_Replication_Full_Grid.R
```
Requires: `01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet`
Outputs: 19 PDFs in `03_Outputs/Figures/`, 1 CSV in `03_Outputs/Tables/`

### Phase 2 — Single-Stock Portfolio
```r
Rscript 02_Scripts/02_Phase_2/Arnott_Phase2_Single_Stock_Full.R
```
Requires: `01_Data/Clean_Daily_Inputs/arnott_master.parquet`, `pfs_daily.parquet`, `usa_factor_weights.parquet`, `USA_daily_rets.parquet`
Optional: `WRDS_USER` / `WRDS_PASSWORD` env vars for Section 12 (liquidity analysis)
Outputs: PNGs in `03_Outputs/Figures/`, reports in `03_Outputs/Reports/`

---

## Key Results

| Strategy | Gross SR | Net SR (after TC/SC/Tax) |
|---|---|---|
| Factor Mom LS Median 1M (Phase 1) | 0.42 | — |
| CS_LO_50 1M (Phase 2, stock-level) | 0.68 | 0.26 |
| CS_LO_50 12M (Phase 2, best net) | 0.83 | 0.40 |

---

## Data Sources

- **JKP Factors**: Jensen, Kelly & Pedersen (2023) — `pfs_daily.parquet`, `usa_factor_weights.parquet`
- **Stock Returns**: `USA_daily_rets.parquet` (153 factors, daily L-S, direction-corrected)
- **Stock Chars**: `USA_stocks_char.parquet` (me, ret_exc_lead1m, div12m_me)
- **Fama-French**: downloaded live via `frenchdata` R package (market factor, 17 industries)
- **WRDS/CRSP**: WRDS credentials required for liquidity enrichment (Section 12 of Phase 2)

---

## Reference

Arnott, R., Kalesnik, V., & Linnainmaa, J. (2023). Factor Momentum. *Review of Financial Studies*, 36(8), 3034–3070.
