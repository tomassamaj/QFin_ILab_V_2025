# ==============================================================================
# MASTER PLOT: 6-Factor Replication Panel (2x3 Grid)
# ==============================================================================

library(tidyverse)
library(tidyfinance)
library(RSQLite)
library(scales)
library(lubridate)

# 1. Setup Data Map ------------------------------------------------------------
# Map your DB table names to the corresponding Benchmark CSV filenames
factor_map <- tribble(
  ~Factor_Name,          ~DB_Table,                    ~Benchmark_File,
  "1. Earnings/Price",   "factor_earnings_price",      "[usa]_[ni_me]_[monthly]_[vw_cap].csv",
  "2. OCF/Price",        "factor_ocf_price",           "[usa]_[ocf_me]_[monthly]_[vw_cap].csv",
  "3. FCF/Price",        "factor_fcf_price",           "[usa]_[fcf_me]_[monthly]_[vw_cap].csv",
  "4. Gross Profitability","factor_gross_profitability","[usa]_[gp_at]_[monthly]_[vw_cap].csv",
  "5. Asset Growth",     "factor_asset_growth",        "[usa]_[at_gr1]_[monthly]_[vw_cap].csv",
  "6. Op. Accruals",     "factor_accruals",            "[usa]_[oaccruals_at]_[monthly]_[vw_cap].csv"
)

# 2. Fetch & Process Function --------------------------------------------------
process_factor <- function(name, table, file) {
  
  # Connect to DB locally within function to ensure freshness
  conn <- dbConnect(SQLite(), "data/tidy_finance_r.sqlite")
  
  # A. Fetch Replicated Data
  if(!dbExistsTable(conn, table)) { dbDisconnect(conn); return(NULL) }
  
  rep_data <- tbl(conn, table) |> collect() |>
    mutate(date = ymd(date)) |>
    # Standardize column names (the 2nd col is always the return)
    select(date, ret_rep = 2) |>
    mutate(date = ceiling_date(date, "month") - days(1)) # End-of-month alignment
  
  dbDisconnect(conn)
  
  # B. Fetch Benchmark Data
  bench_path <- file.path("benchmark_data", file)
  if(!file.exists(bench_path)) return(NULL)
  
  bench_data <- read_csv(bench_path, show_col_types = FALSE) |>
    mutate(date = ymd(date)) |>
    select(date, ret_bench = ret)
  
  # C. Merge & Calc Cumulative
  merged <- inner_join(rep_data, bench_data, by = "date") |>
    arrange(date) |>
    mutate(
      Factor = name,
      Replicated = cumprod(1 + ret_rep) - 1,
      Benchmark  = cumprod(1 + ret_bench) - 1
    ) |>
    select(Factor, date, Replicated, Benchmark) |>
    pivot_longer(cols = c(Replicated, Benchmark), names_to = "Series", values_to = "CumRet")
  
  return(merged)
}

# 3. Execute & Plot ------------------------------------------------------------
message("Compiling data for 6-Factor Panel...")

plot_data <- pmap_dfr(factor_map, function(Factor_Name, DB_Table, Benchmark_File) {
  process_factor(Factor_Name, DB_Table, Benchmark_File)
})

# Create the 2x3 Grid Plot
p <- ggplot(plot_data, aes(x = date, y = CumRet, color = Series, linetype = Series)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Benchmark" = "gray50", "Replicated" = "#004697")) + # WU Blue
  scale_linetype_manual(values = c("Benchmark" = "dashed", "Replicated" = "solid")) +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~ Factor, scales = "free_y", ncol = 3) +
  labs(
    title = "QFin Factor Replication: Multi-Factor Validation",
    subtitle = "Comparison of Replicated Factors (Blue) vs. JKP Benchmarks (Gray)",
    y = "Cumulative Excess Return",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)