# --- LIBRARIES ---
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  arrow,
  tidyverse,
  data.table,
  lubridate,
  PerformanceAnalytics,
  scales,
  ggplot2
)

# --- 1. CONFIGURATION ---
MASTER_FILE <- "C:/WU/ILab ZZ/QFin_ILab_V_2025/01_Data/Processed/arnott_master.parquet"
OUTPUT_FILE_10 <- "C:/WU/ILab ZZ/QFin_ILab_V_2025/01_Data/Processed/arnott_master_dec_ls.parquet"
OUTPUT_FILE_5 <- "C:/WU/ILab ZZ/QFin_ILab_V_2025/01_Data/Processed/arnott_master_top5_ls.parquet"

# --- 2. LOAD & PREP DATA ---
dt <- read_parquet(MASTER_FILE) %>% as.data.table()
dt[, eom := as.Date(eom)]

# --- 3. CALCULATE ALL PERCENTILES ---
# Compute the 2.5th, 5th, 95th, and 97.5th percentiles per month
dt[,
  `:=`(
    p025 = quantile(weight, 0.025, na.rm = TRUE),
    p05 = quantile(weight, 0.05, na.rm = TRUE),
    p95 = quantile(weight, 0.95, na.rm = TRUE),
    p975 = quantile(weight, 0.975, na.rm = TRUE)
  ),
  by = eom
]


# ==============================================================================
# VERSION A: TOP & BOTTOM 5 PERCENTILES (10% Total Positions)
# ==============================================================================

# Filter positions
dt_dec_ls <- dt[weight <= p05 | weight >= p95]

# Clean up percentile columns
dt_dec_ls[, `:=`(p025 = NULL, p05 = NULL, p95 = NULL, p975 = NULL)]

# Rescale Weights
dt_dec_ls[weight > 0, weight := 0.5 * (weight / sum(weight)), by = eom]
dt_dec_ls[weight < 0, weight := 0.5 * (weight / sum(abs(weight))), by = eom]

# Sanity Check & Save
cat("\n--- Sanity Check: 5% Long / 5% Short ---\n")
print(dt_dec_ls[,
  .(net_exp = sum(weight), gross_exp = sum(abs(weight)), n_pos = .N),
  by = eom
])
write_parquet(dt_dec_ls, OUTPUT_FILE_10)


# ==============================================================================
# VERSION B: TOP & BOTTOM 2.5 PERCENTILES (5% Total Positions)
# ==============================================================================

# Filter positions
dt_top5_ls <- dt[weight <= p025 | weight >= p975]

# Clean up percentile columns
dt_top5_ls[, `:=`(p025 = NULL, p05 = NULL, p95 = NULL, p975 = NULL)]

# Rescale Weights
dt_top5_ls[weight > 0, weight := 0.5 * (weight / sum(weight)), by = eom]
dt_top5_ls[weight < 0, weight := 0.5 * (weight / sum(abs(weight))), by = eom]

# Sanity Check & Save
cat("\n--- Sanity Check: 2.5% Long / 2.5% Short ---\n")
print(dt_top5_ls[,
  .(net_exp = sum(weight), gross_exp = sum(abs(weight)), n_pos = .N),
  by = eom
])
write_parquet(dt_top5_ls, OUTPUT_FILE_5)
