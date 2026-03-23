# Legacy Phase 2 Scripts

These are the original exploratory scripts written during Phase 2 development.
They have been consolidated into `Arnott_Phase2_Single_Stock_Full.R` (one level up).

| File | What it was |
|---|---|
| 01_Netting_from_JKP_Cluster_weights_2026_02_05.R | Core netting + daily backtest + event studies |
| 02_flip_and_factor_from_single_stock.R | Bottom-up replication audit → factors_to_flip |
| 03_daily_to_monthly_audit.R | Daily → monthly aggregation check |
| 04_real_world_factor_momentum_implementation.R | Grid search + WRDS enrichment + multiple backtests |
| 05_portfolio_analysis_and_wrds_mapping.R | Portfolio concentration + tradeability scoring |
| 06_enhanced_liquidity_analysis.R | CRSP market cap / volume / days-to-trade |
| 07_net_weights_analysis.R | Turnover diagnostics + local arnott_master backtest |
| 08_Limit_no_of_positions.R | Position limiting to extreme deciles |
| 09_Frictions.R | Full frictions model (TC + shorting + dividend tax) |

All logic is preserved in the consolidated script. These files are kept for reference only.
