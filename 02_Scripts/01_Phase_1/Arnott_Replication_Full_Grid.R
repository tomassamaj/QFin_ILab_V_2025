# ==============================================================================
# ARNOTT (2023) FACTOR MOMENTUM: FULL PHASE 1 REPLICATION
# Reference: Arnott, Kalesnik & Linnainmaa (2023) "Factor Momentum", RFS
#
# Strategy: Cross-sectional factor momentum using JKP daily factors
#   - Signal:       Rolling log-return over lookback window
#   - Lag:          1-day implementation lag (realistic real-world constraint)
#   - Rebalancing:  Every 21 trading days (monthly)
#   - Universe:     All JKP core factors (USA)
#
# Parameter Grid:
#   - Split rules:    LS_Median, LS_33, LS_25, LO_Median, LO_33, LO_25
#   - Lookback (days):21, 42, 63, 126, 252  (1m, 2m, 3m, 6m, 12m)
#   - Holding days:   21 (fixed, monthly rebalancing)
#
# Benchmarks:
#   - Industry Momentum (Moskowitz & Grinblatt 1999): 17 FF Industries (Daily)
#   - Market Factor: FF3 Mkt-RF (Daily)
#
# Outputs:
#   - Performance table (Sharpe, Ann Ret, Ann Vol, Cum Ret, Max DD, Calmar)
#   - Sharpe ratio heatmap across parameter grid
#   - Cumulative wealth comparison plots (best variants vs benchmarks)
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  arrow,
  lubridate,
  zoo,
  ggplot2,
  scales,
  ggthemes,
  frenchdata,
  gt,
  patchwork,
  RColorBrewer
)

setwd(
  "/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code"
)

# --- CONFIGURATION ---
DAILY_PFS_FILE <- "01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet"
OUTPUT_DIR <- "03_Outputs/Figures"
OUTPUT_TABLE <- "03_Outputs/Tables"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_TABLE, showWarnings = FALSE, recursive = TRUE)

START_DATE <- as.Date("1963-01-01") # Analysis start date
HOLDING_DAYS <- 21 # Monthly rebalancing (fixed)
IMPL_LAG <- 1 # 1-day implementation lag

LOOKBACK_GRID <- c(21, 42, 63, 126, 252)
LOOKBACK_LABELS <- c("1M", "2M", "3M", "6M", "12M")
names(LOOKBACK_LABELS) <- LOOKBACK_GRID

STRATEGY_TYPES <- c(
  "LS_Median",
  "LS_33",
  "LS_25",
  "LO_Median",
  "LO_33",
  "LO_25"
)

ANN_FACTOR <- 252 / HOLDING_DAYS # Annualization factor (~12 periods/year)

cat("==============================================================\n")
cat("  Arnott (2023) Factor Momentum — Full Grid Replication\n")
cat("==============================================================\n")


# ==============================================================================
# 1. LOAD JKP DAILY FACTOR DATA
# ==============================================================================
cat("\n--- 1. Loading JKP Daily Factor Data ---\n")

daily_factors <- read_parquet(DAILY_PFS_FILE)

daily_factors_wide <- daily_factors %>%
  pivot_wider(names_from = characteristic, values_from = factor_ret) %>%
  arrange(date) %>%
  filter(date >= START_DATE)

factor_cols <- colnames(daily_factors_wide)[-1]
cat(
  "   Data loaded:",
  nrow(daily_factors_wide),
  "days x",
  length(factor_cols),
  "factors\n"
)
cat(
  "   Date range: ",
  format(min(daily_factors_wide$date)),
  "to",
  format(max(daily_factors_wide$date)),
  "\n"
)


# ==============================================================================
# 2. LOAD BENCHMARKS
# ==============================================================================
cat("\n--- 2. Loading Benchmark Data ---\n")

# --- 2a. Market Factor (FF3 Daily) ---
cat("   Fetching FF3 Mkt-RF daily...\n")
ff3_raw <- tryCatch(
  download_french_data("Fama/French 3 Factors [Daily]"),
  error = function(e) {
    cat("   WARNING: Could not download FF3.", conditionMessage(e), "\n")
    NULL
  }
)

mkt_daily <- NULL
if (!is.null(ff3_raw)) {
  mkt_daily <- ff3_raw$subsets$data[[1]] %>%
    mutate(
      date = ymd(date),
      mkt_rf = as.numeric(`Mkt-RF`) / 100
    ) %>%
    select(date, mkt_rf) %>%
    filter(!is.na(mkt_rf), date >= START_DATE)
  cat("   Market factor loaded:", nrow(mkt_daily), "days\n")
}

# --- 2b. Industry Momentum (17 FF Industries Daily) ---
cat("   Fetching 17 Industry Portfolios [Daily]...\n")
ind_raw <- tryCatch(
  download_french_data("17 Industry Portfolios [Daily]"),
  error = function(e) {
    cat("   WARNING: Could not download industries.", conditionMessage(e), "\n")
    NULL
  }
)

ind_daily <- NULL
if (!is.null(ind_raw)) {
  ind_daily <- ind_raw$subsets$data[[1]] %>%
    mutate(date = ymd(date)) %>%
    mutate(across(-date, ~ as.numeric(.) / 100)) %>%
    arrange(date) %>%
    filter(date >= START_DATE)
  cat(
    "   Industry data loaded:",
    nrow(ind_daily),
    "days x",
    ncol(ind_daily) - 1,
    "industries\n"
  )
}


# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================
cat("\n--- 3. Defining Strategy Functions ---\n")

# ------------------------------------------------------------------------------
# compute_positions(): Given signal values for a row, compute portfolio weights
#   strategy: one of LS_Median, LS_33, LS_25, LO_Median, LO_33, LO_25
#   Returns named vector of weights (positive = long, negative = short)
# ------------------------------------------------------------------------------
compute_weights <- function(signals, strategy) {
  n <- length(signals)
  if (n < 4) {
    return(rep(0, n))
  }

  weights <- rep(0.0, n)

  if (strategy == "LS_Median") {
    med <- median(signals, na.rm = TRUE)
    long <- which(!is.na(signals) & signals > med)
    short <- which(!is.na(signals) & signals <= med)
    if (length(long) > 0) {
      weights[long] <- 1 / length(long)
    }
    if (length(short) > 0) weights[short] <- -1 / length(short)
  } else if (strategy == "LS_33") {
    q <- quantile(signals, probs = c(1 / 3, 2 / 3), na.rm = TRUE)
    long <- which(!is.na(signals) & signals >= q[2])
    short <- which(!is.na(signals) & signals <= q[1])
    if (length(long) > 0) {
      weights[long] <- 1 / length(long)
    }
    if (length(short) > 0) weights[short] <- -1 / length(short)
  } else if (strategy == "LS_25") {
    q <- quantile(signals, probs = c(0.25, 0.75), na.rm = TRUE)
    long <- which(!is.na(signals) & signals >= q[2])
    short <- which(!is.na(signals) & signals <= q[1])
    if (length(long) > 0) {
      weights[long] <- 1 / length(long)
    }
    if (length(short) > 0) weights[short] <- -1 / length(short)
  } else if (strategy == "LO_Median") {
    med <- median(signals, na.rm = TRUE)
    long <- which(!is.na(signals) & signals > med)
    if (length(long) > 0) weights[long] <- 1 / length(long)
  } else if (strategy == "LO_33") {
    q <- quantile(signals, probs = 2 / 3, na.rm = TRUE)
    long <- which(!is.na(signals) & signals >= q)
    if (length(long) > 0) weights[long] <- 1 / length(long)
  } else if (strategy == "LO_25") {
    q <- quantile(signals, probs = 0.75, na.rm = TRUE)
    long <- which(!is.na(signals) & signals >= q)
    if (length(long) > 0) weights[long] <- 1 / length(long)
  }

  return(weights)
}

# ------------------------------------------------------------------------------
# calculate_factor_momentum(): Full strategy computation
#   Returns tibble with columns: date, period_ret
# ------------------------------------------------------------------------------
calculate_factor_momentum <- function(
  df,
  factor_cols,
  lookback_days = 21,
  holding_days = 21,
  impl_lag = 1,
  strategy = "LS_Median"
) {
  cols_exist <- intersect(factor_cols, colnames(df))
  if (length(cols_exist) < 4) {
    warning("Too few factor columns — returning NULL.")
    return(NULL)
  }

  total_lag <- 1 + impl_lag # 1 (execution) + impl_lag (skip days)

  # Step 1: Compute rolling log-return signal (lookback_days window)
  df_signals <- df %>%
    select(date, all_of(cols_exist)) %>%
    arrange(date) %>%
    mutate(across(
      all_of(cols_exist),
      \(x) {
        rollapply(
          log(1 + x),
          width = lookback_days,
          FUN = sum,
          fill = NA,
          align = "right"
        )
      },
      .names = "{.col}_signal"
    ))

  # Step 2: Lag the signal by total_lag
  df_signals <- df_signals %>%
    mutate(across(ends_with("_signal"), \(x) lag(x, n = total_lag)))

  # Step 3: Compute forward compound return (holding_days window)
  df_signals <- df_signals %>%
    mutate(across(
      all_of(cols_exist),
      \(x) {
        rollapply(
          log(1 + x),
          width = holding_days,
          FUN = sum,
          fill = NA,
          align = "left"
        )
      },
      .names = "{.col}_fwd"
    ))

  signal_cols <- paste0(cols_exist, "_signal")
  fwd_cols <- paste0(cols_exist, "_fwd")

  # Step 4: Drop rows where ALL signals are NA or ALL fwd returns are NA
  #         (keep rows where at least min_factors factors are valid)
  min_factors <- 4
  df_valid <- df_signals %>%
    filter(
      rowSums(!is.na(across(all_of(signal_cols)))) >= min_factors,
      rowSums(!is.na(across(all_of(fwd_cols)))) >= min_factors
    )

  if (nrow(df_valid) < 2) {
    return(NULL)
  }

  # Step 5: Monthly rebalancing — take every holding_days-th row
  rebal_idx <- seq(1, nrow(df_valid), by = holding_days)
  df_rebal <- df_valid[rebal_idx, ]

  # Step 6: Compute portfolio return — use only factors valid on each date
  period_rets <- map_dbl(seq_len(nrow(df_rebal)), function(i) {
    signals <- as.numeric(df_rebal[i, signal_cols])
    fwd_logret <- as.numeric(df_rebal[i, fwd_cols])

    # Only use factors where BOTH signal and fwd return are available
    valid_idx <- !is.na(signals) & !is.na(fwd_logret)
    if (sum(valid_idx) < min_factors) {
      return(NA_real_)
    }

    fwd_ret <- exp(fwd_logret[valid_idx]) - 1
    wts <- compute_weights(signals[valid_idx], strategy)
    sum(wts * fwd_ret, na.rm = TRUE)
  })

  tibble(
    date = df_rebal$date,
    period_ret = period_rets
  ) %>%
    filter(!is.na(period_ret))
}

# ------------------------------------------------------------------------------
# compute_industry_momentum(): 1-month lookback → 1-month holding (1d lag)
#   Returns tibble: date, period_ret
# ------------------------------------------------------------------------------
compute_industry_momentum <- function(
  ind_df,
  lookback_days = 21,
  holding_days = 21,
  impl_lag = 1,
  strategy = "LS_Median"
) {
  if (is.null(ind_df)) {
    return(NULL)
  }

  ind_cols <- setdiff(colnames(ind_df), "date")
  calculate_factor_momentum(
    df = ind_df,
    factor_cols = ind_cols,
    lookback_days = lookback_days,
    holding_days = holding_days,
    impl_lag = impl_lag,
    strategy = strategy
  )
}

# ------------------------------------------------------------------------------
# compute_metrics(): Performance statistics for a returns series
# ------------------------------------------------------------------------------
compute_metrics <- function(period_rets, ann_factor) {
  r <- period_rets[!is.na(period_rets)]
  n <- length(r)
  if (n < 2) {
    return(NULL)
  }

  cum_ret <- prod(1 + r) - 1
  ann_ret <- (prod(1 + r))^(ann_factor / n) - 1
  ann_vol <- sd(r) * sqrt(ann_factor)
  sharpe <- if (ann_vol > 0) ann_ret / ann_vol else NA

  # Max drawdown
  cum_wealth <- cumprod(1 + r)
  peak <- cummax(cum_wealth)
  drawdowns <- (cum_wealth - peak) / peak
  max_dd <- min(drawdowns)

  calmar <- if (abs(max_dd) > 1e-10) ann_ret / abs(max_dd) else NA

  tibble(
    N = n,
    Ann_Ret = ann_ret,
    Ann_Vol = ann_vol,
    Sharpe = sharpe,
    Cum_Ret = cum_ret,
    Max_DD = max_dd,
    Calmar = calmar
  )
}


# ==============================================================================
# 4. RUN FULL PARAMETER GRID
# ==============================================================================
cat(
  "\n--- 4. Running Parameter Grid (",
  length(LOOKBACK_GRID),
  "lookbacks x",
  length(STRATEGY_TYPES),
  "strategies =",
  length(LOOKBACK_GRID) * length(STRATEGY_TYPES),
  "combos) ---\n"
)

all_grid_results <- list()
all_grid_metrics <- list()

for (lb in LOOKBACK_GRID) {
  lb_label <- LOOKBACK_LABELS[as.character(lb)]
  cat("\n  Lookback =", lb_label, "(", lb, "days)\n")

  for (strat in STRATEGY_TYPES) {
    cat("    Strategy:", strat, "...")
    key <- paste0(strat, "_", lb_label)

    res <- tryCatch(
      calculate_factor_momentum(
        df = daily_factors_wide,
        factor_cols = factor_cols,
        lookback_days = lb,
        holding_days = HOLDING_DAYS,
        impl_lag = IMPL_LAG,
        strategy = strat
      ),
      error = function(e) {
        cat(" ERROR:", conditionMessage(e), "\n")
        NULL
      }
    )

    if (!is.null(res) && nrow(res) > 1) {
      metrics <- compute_metrics(res$period_ret, ANN_FACTOR)
      metrics$Strategy <- strat
      metrics$Lookback <- lb_label
      metrics$Lookback_Days <- lb
      metrics$Key <- key

      all_grid_results[[key]] <- res %>%
        mutate(Strategy = strat, Lookback = lb_label, Key = key)
      all_grid_metrics[[key]] <- metrics
      cat(" Sharpe =", round(metrics$Sharpe, 2), "\n")
    } else {
      cat(" skipped (insufficient data)\n")
    }
  }
}

cat("\n  Grid complete.", length(all_grid_results), "valid combinations.\n")

# Combine results
metrics_df <- bind_rows(all_grid_metrics)
results_all <- bind_rows(all_grid_results)


# ==============================================================================
# 5. COMPUTE BENCHMARKS
# ==============================================================================
cat("\n--- 5. Computing Benchmark Series ---\n")

# --- 5a. Industry Momentum (LS Median, 1M lookback — canonical Arnott spec) ---
ind_mom <- NULL
ind_metrics <- NULL
if (!is.null(ind_daily)) {
  cat("   Industry Momentum (LS_Median, 1M)...\n")
  ind_mom <- compute_industry_momentum(
    ind_df = ind_daily,
    lookback_days = 21,
    holding_days = HOLDING_DAYS,
    impl_lag = IMPL_LAG,
    strategy = "LS_Median"
  )
  if (!is.null(ind_mom)) {
    ind_metrics <- compute_metrics(ind_mom$period_ret, ANN_FACTOR) %>%
      mutate(
        Strategy = "Ind_Momentum",
        Lookback = "1M",
        Key = "Ind_Momentum_1M"
      )
    cat("   Industry Momentum Sharpe =", round(ind_metrics$Sharpe, 2), "\n")
  }
}

# --- 5b. Market Factor: Aggregate over same rebalancing windows ---
mkt_series <- NULL
mkt_metrics <- NULL
if (!is.null(mkt_daily)) {
  cat("   Market Factor (Mkt-RF, monthly-compounded)...\n")

  # Align market returns to same rebalancing cadence as strategy
  # Use the dates from the first strategy result for alignment
  if (length(all_grid_results) > 0) {
    ref_dates <- all_grid_results[[1]]$date

    mkt_series <- map_dfr(seq_along(ref_dates), function(i) {
      d_start <- if (i == 1) as.Date(min(mkt_daily$date)) else ref_dates[i - 1]
      d_end <- ref_dates[i]
      window <- mkt_daily %>% filter(date > d_start & date <= d_end)
      tibble(date = d_end, period_ret = prod(1 + window$mkt_rf) - 1)
    })

    mkt_metrics <- compute_metrics(mkt_series$period_ret, ANN_FACTOR) %>%
      mutate(Strategy = "Market (Mkt-RF)", Lookback = "—", Key = "Market")
    cat("   Market Sharpe =", round(mkt_metrics$Sharpe, 2), "\n")
  }
}


# ==============================================================================
# 6. PERFORMANCE TABLE
# ==============================================================================
cat("\n--- 6. Building Performance Table ---\n")

# Combine all metrics
bench_metrics <- bind_rows(
  if (!is.null(ind_metrics)) ind_metrics else NULL,
  if (!is.null(mkt_metrics)) mkt_metrics else NULL
)
full_metrics <- bind_rows(metrics_df, bench_metrics) %>%
  select(
    Key,
    Strategy,
    Lookback,
    Lookback_Days,
    N,
    Ann_Ret,
    Ann_Vol,
    Sharpe,
    Cum_Ret,
    Max_DD,
    Calmar
  ) %>%
  arrange(Strategy, Lookback_Days)

# Pretty-print to console
cat("\nPerformance Summary (Factor Momentum Strategies):\n")
print(
  full_metrics %>%
    mutate(
      Ann_Ret = scales::percent(Ann_Ret, accuracy = 0.1),
      Ann_Vol = scales::percent(Ann_Vol, accuracy = 0.1),
      Sharpe = round(Sharpe, 2),
      Cum_Ret = scales::percent(Cum_Ret, accuracy = 0.1),
      Max_DD = scales::percent(Max_DD, accuracy = 0.1),
      Calmar = round(Calmar, 2)
    ) %>%
    select(-Key, -Lookback_Days),
  n = 50
)

# Save CSV
write_csv(full_metrics, file.path(OUTPUT_TABLE, "Arnott_Grid_Performance.csv"))
cat("   Saved:", file.path(OUTPUT_TABLE, "Arnott_Grid_Performance.csv"), "\n")


# ==============================================================================
# 7. SHARPE RATIO HEATMAP (Factor Momentum Grid)
# ==============================================================================
cat("\n--- 7. Plotting Sharpe Heatmap ---\n")

heatmap_data <- metrics_df %>%
  filter(Strategy %in% STRATEGY_TYPES) %>%
  mutate(
    Lookback = factor(Lookback, levels = LOOKBACK_LABELS),
    Strategy = factor(Strategy, levels = STRATEGY_TYPES)
  )

p_heatmap <- ggplot(
  heatmap_data,
  aes(x = Lookback, y = Strategy, fill = Sharpe)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Sharpe, 2)), size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#1a9850",
    midpoint = 0,
    name = "Sharpe\nRatio"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title = "Factor Momentum: Sharpe Ratio Across Parameter Grid",
    subtitle = paste0(
      "JKP Daily Factors | 1963– | 1-Day Implementation Lag | ",
      HOLDING_DAYS,
      "-Day Holding Period"
    ),
    x = "Lookback Window",
    y = "Strategy Type",
    caption = "LS = Long-Short | LO = Long-Only | Median / 33% / 25% split"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(p_heatmap)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_Grid_Sharpe_Heatmap.pdf"),
  plot = p_heatmap,
  width = 9,
  height = 5.5
)
cat("   Saved: Arnott_Grid_Sharpe_Heatmap.pdf\n")


# ==============================================================================
# 8. ANNUALIZED RETURN HEATMAP
# ==============================================================================
p_ret_heatmap <- ggplot(
  heatmap_data,
  aes(x = Lookback, y = Strategy, fill = Ann_Ret)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = scales::percent(Ann_Ret, accuracy = 0.1)),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "#ffffbf",
    high = "#1a9850",
    midpoint = 0,
    labels = scales::percent_format(),
    name = "Ann.\nReturn"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title = "Factor Momentum: Annualized Return Across Parameter Grid",
    subtitle = paste0(
      "JKP Daily Factors | 1963– | 1-Day Implementation Lag | ",
      HOLDING_DAYS,
      "-Day Holding Period"
    ),
    x = "Lookback Window",
    y = "Strategy Type",
    caption = "LS = Long-Short | LO = Long-Only | Median / 33% / 25% split"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(p_ret_heatmap)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_Grid_AnnReturn_Heatmap.pdf"),
  plot = p_ret_heatmap,
  width = 9,
  height = 5.5
)
cat("   Saved: Arnott_Grid_AnnReturn_Heatmap.pdf\n")


# ==============================================================================
# 9. CUMULATIVE WEALTH: BEST FACTOR MOMENTUM vs BENCHMARKS
# ==============================================================================
cat(
  "\n--- 9. Cumulative Wealth Comparison (Best Strategies vs Benchmarks) ---\n"
)

# Pick top-3 by Sharpe from factor momentum grid
top3 <- metrics_df %>%
  arrange(desc(Sharpe)) %>%
  slice_head(n = 3) %>%
  pull(Key)

cat("   Top 3 strategies by Sharpe:", paste(top3, collapse = ", "), "\n")

build_cumwealth <- function(series_df, label) {
  series_df %>%
    arrange(date) %>%
    mutate(
      cum_wealth = cumprod(1 + period_ret),
      Series = label
    ) %>%
    select(date, cum_wealth, Series)
}

# Factor momentum: top 3
cum_data_list <- map(top3, function(k) {
  df <- all_grid_results[[k]]
  lbl <- paste0(df$Strategy[1], " (", df$Lookback[1], ")")
  build_cumwealth(df %>% select(date, period_ret), lbl)
})

# Industry momentum benchmark
if (!is.null(ind_mom)) {
  cum_data_list <- c(
    cum_data_list,
    list(build_cumwealth(
      ind_mom %>% select(date, period_ret),
      "Industry Momentum"
    ))
  )
}

# Market factor benchmark
if (!is.null(mkt_series)) {
  cum_data_list <- c(
    cum_data_list,
    list(build_cumwealth(
      mkt_series %>% select(date, period_ret),
      "Market (Mkt-RF)"
    ))
  )
}

cum_data <- bind_rows(cum_data_list)

# Add start row (cum_wealth = 1)
start_date <- min(cum_data$date) - 1
start_rows <- tibble(
  date = start_date,
  cum_wealth = 1.0,
  Series = unique(cum_data$Series)
)
cum_data_plot <- bind_rows(start_rows, cum_data) %>% arrange(Series, date)

# Color palette
n_series <- length(unique(cum_data_plot$Series))
pal_lines <- c(
  RColorBrewer::brewer.pal(min(8, n_series - 2), "Set1"),
  "gray40",
  "black"
)[seq_len(n_series)]
names(pal_lines) <- unique(cum_data_plot$Series)
line_types <- c(rep("solid", n_series - 2), "dashed", "dotted")[seq_len(
  n_series
)]
names(line_types) <- unique(cum_data_plot$Series)

p_cumwealth <- ggplot(
  cum_data_plot,
  aes(x = date, y = cum_wealth, color = Series, linetype = Series)
) +
  geom_line(linewidth = 1) +
  scale_y_log10(
    labels = scales::comma_format(accuracy = 0.1),
    breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)
  ) +
  scale_color_manual(values = pal_lines) +
  scale_linetype_manual(values = line_types) +
  labs(
    title = "Factor Momentum vs Benchmarks: Cumulative Wealth (Log Scale)",
    subtitle = paste0(
      "Top 3 Factor Momentum Strategies | 1963– | 1-Day Lag | ",
      HOLDING_DAYS,
      "d Holding"
    ),
    x = NULL,
    y = "Cumulative Wealth (Start = 1, Log Scale)",
    color = NULL,
    linetype = NULL,
    caption = "Data: JKP Daily Factors & French Data Library"
  ) +
  # ...existing code...
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

print(p_cumwealth)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_Top3_vs_Benchmarks.pdf"),
  plot = p_cumwealth,
  width = 11,
  height = 7
)
cat("   Saved: Arnott_Top3_vs_Benchmarks.pdf\n")


# ==============================================================================
# 10. FACETED GRID: ALL STRATEGY TYPES × ALL LOOKBACKS
# ==============================================================================
cat("\n--- 10. Faceted Grid Plot (all combinations) ---\n")

# Build cumulative wealth for all grid combinations + benchmarks
cum_all_list <- map(names(all_grid_results), function(k) {
  df <- all_grid_results[[k]]
  lbl <- paste0(df$Strategy[1], " / ", df$Lookback[1])
  df %>%
    arrange(date) %>%
    mutate(
      cum_wealth = cumprod(1 + period_ret),
      Series = "Factor Momentum",
      Label = lbl,
      Strategy = df$Strategy[1],
      Lookback = df$Lookback[1]
    ) %>%
    select(date, cum_wealth, Series, Label, Strategy, Lookback)
})

cum_all <- bind_rows(cum_all_list)

# Add benchmarks as reference lines per facet (replicate across all facets)
bench_traces <- list()
if (!is.null(ind_mom)) {
  bench_traces[["Industry Momentum"]] <- ind_mom %>%
    arrange(date) %>%
    mutate(cum_wealth = cumprod(1 + period_ret)) %>%
    select(date, cum_wealth)
}
if (!is.null(mkt_series)) {
  bench_traces[["Market (Mkt-RF)"]] <- mkt_series %>%
    arrange(date) %>%
    mutate(cum_wealth = cumprod(1 + period_ret)) %>%
    select(date, cum_wealth)
}

# For faceted plot, cross-join benchmarks to every strategy×lookback combo
strat_lb_combos <- cum_all %>% distinct(Strategy, Lookback)

bench_for_facets <- imap_dfr(bench_traces, function(bdf, bname) {
  strat_lb_combos %>%
    mutate(bench_name = bname) %>%
    left_join(bdf %>% mutate(across(cum_wealth, ~.x)), by = character()) %>%
    # Only keep dates within range of factor momentum
    filter(!is.na(date))
}) %>%
  rename(bench_cum = cum_wealth)

# Assemble main panel data with start rows per facet
cum_all_start <- strat_lb_combos %>%
  mutate(
    date = min(cum_all$date) - 1,
    cum_wealth = 1.0,
    Series = "Factor Momentum",
    Label = paste0(Strategy, " / ", Lookback)
  )
cum_all_plot <- bind_rows(cum_all_start, cum_all) %>%
  arrange(Strategy, Lookback, date)

cum_all_plot$Lookback <- factor(cum_all_plot$Lookback, levels = LOOKBACK_LABELS)
cum_all_plot$Strategy <- factor(cum_all_plot$Strategy, levels = STRATEGY_TYPES)

p_grid <- ggplot() +
  geom_line(
    data = cum_all_plot,
    aes(x = date, y = cum_wealth),
    color = "#004697",
    linewidth = 0.8
  ) +
  facet_grid(Strategy ~ Lookback, scales = "free_y") +
  scale_y_log10(labels = scales::comma_format(accuracy = 0.01)) +
  labs(
    title = "Factor Momentum: Full Parameter Grid — Cumulative Wealth",
    subtitle = paste0(
      "JKP Daily Factors | 1-Day Implementation Lag | ",
      HOLDING_DAYS,
      "-Day Rebalancing"
    ),
    x = NULL,
    y = "Cumulative Wealth (Log Scale)",
    caption = paste0(
      "Rows: Strategy type (LS = Long-Short, LO = Long-Only; Median/33%/25% split)\n",
      "Cols: Lookback window (1M=21d, 2M=42d, 3M=63d, 6M=126d, 12M=252d)"
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

print(p_grid)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_Full_Grid_CumWealth.pdf"),
  plot = p_grid,
  width = 16,
  height = 12
)
cat("   Saved: Arnott_Full_Grid_CumWealth.pdf\n")


# ==============================================================================
# 11. LONG-SHORT vs LONG-ONLY COMPARISON (1M Lookback, Median Split)
# ==============================================================================
cat("\n--- 11. Long-Short vs Long-Only Comparison ---\n")

ls_lo_keys <- c(
  "LS_Median_1M",
  "LO_Median_1M",
  "LS_33_1M",
  "LO_33_1M",
  "LS_25_1M",
  "LO_25_1M"
)
ls_lo_avail <- intersect(ls_lo_keys, names(all_grid_results))

ls_lo_cum <- map_dfr(ls_lo_avail, function(k) {
  df <- all_grid_results[[k]]
  df %>%
    arrange(date) %>%
    mutate(
      cum_wealth = cumprod(1 + period_ret),
      Series = k
    ) %>%
    select(date, cum_wealth, Series)
})

if (!is.null(ind_mom)) {
  ls_lo_cum <- bind_rows(
    ls_lo_cum,
    ind_mom %>%
      arrange(date) %>%
      mutate(
        cum_wealth = cumprod(1 + period_ret),
        Series = "Industry Momentum"
      ) %>%
      select(date, cum_wealth, Series)
  )
}

if (!is.null(mkt_series)) {
  ls_lo_cum <- bind_rows(
    ls_lo_cum,
    mkt_series %>%
      arrange(date) %>%
      mutate(
        cum_wealth = cumprod(1 + period_ret),
        Series = "Market (Mkt-RF)"
      ) %>%
      select(date, cum_wealth, Series)
  )
}

start_rows_lslo <- tibble(
  date = min(ls_lo_cum$date) - 1,
  cum_wealth = 1.0,
  Series = unique(ls_lo_cum$Series)
)
ls_lo_plot <- bind_rows(start_rows_lslo, ls_lo_cum) %>% arrange(Series, date)

p_lslo <- ggplot(
  ls_lo_plot,
  aes(x = date, y = cum_wealth, color = Series, linetype = Series)
) +
  geom_line(linewidth = 0.9) +
  scale_y_log10(labels = scales::comma_format(accuracy = 0.1)) +
  scale_color_manual(
    values = c(
      "LS_Median_1M" = "#1f78b4",
      "LO_Median_1M" = "#a6cee3",
      "LS_33_1M" = "#33a02c",
      "LO_33_1M" = "#b2df8a",
      "LS_25_1M" = "#e31a1c",
      "LO_25_1M" = "#fb9a99",
      "Industry Momentum" = "gray40",
      "Market (Mkt-RF)" = "black"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "LS_Median_1M" = "solid",
      "LO_Median_1M" = "dashed",
      "LS_33_1M" = "solid",
      "LO_33_1M" = "dashed",
      "LS_25_1M" = "solid",
      "LO_25_1M" = "dashed",
      "Industry Momentum" = "dotdash",
      "Market (Mkt-RF)" = "dotted"
    )
  ) +
  labs(
    title = "Long-Short vs Long-Only Factor Momentum (1M Lookback)",
    subtitle = "1963– | Solid = Long-Short | Dashed = Long-Only | 1-Day Implementation Lag",
    x = NULL,
    y = "Cumulative Wealth (Log Scale, Start = 1)",
    color = NULL,
    linetype = NULL,
    caption = "Data: JKP Daily Factors"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 3), linetype = guide_legend(nrow = 3))

print(p_lslo)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_LS_vs_LO_Comparison.pdf"),
  plot = p_lslo,
  width = 11,
  height = 7
)
cat("   Saved: Arnott_LS_vs_LO_Comparison.pdf\n")


# ==============================================================================
# 12. SUMMARY METRICS: SIDE-BY-SIDE BAR CHART (Sharpe & Ann Return)
# ==============================================================================
cat("\n--- 12. Summary Bar Charts ---\n")

bar_data <- metrics_df %>%
  mutate(
    Lookback = factor(Lookback, levels = LOOKBACK_LABELS),
    Strategy = factor(Strategy, levels = STRATEGY_TYPES)
  )

p_sharpe_bar <- ggplot(
  bar_data,
  aes(x = Lookback, y = Sharpe, fill = Strategy)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Sharpe Ratio by Strategy Type and Lookback Window",
    subtitle = "Factor Momentum | JKP Daily | 1963– | 1-Day Lag",
    x = "Lookback Window",
    y = "Sharpe Ratio (Annualized)",
    fill = "Strategy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_sharpe_bar)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_Sharpe_BarChart.pdf"),
  plot = p_sharpe_bar,
  width = 10,
  height = 6
)
cat("   Saved: Arnott_Sharpe_BarChart.pdf\n")


p_ret_bar <- ggplot(bar_data, aes(x = Lookback, y = Ann_Ret, fill = Strategy)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Annualized Return by Strategy Type and Lookback Window",
    subtitle = "Factor Momentum | JKP Daily | 1963– | 1-Day Lag",
    x = "Lookback Window",
    y = "Annualized Return",
    fill = "Strategy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_ret_bar)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_AnnReturn_BarChart.pdf"),
  plot = p_ret_bar,
  width = 10,
  height = 6
)
cat("   Saved: Arnott_AnnReturn_BarChart.pdf\n")


# ==============================================================================
# 13. FOCUSED CUMULATIVE RETURN: Market | Ind Momentum | LS_Median_1M | LO_25_1M
# ==============================================================================
cat("\n--- 13. Focused Cumulative Return Plot ---\n")

focused_series <- list()

# Market
if (!is.null(mkt_series)) {
  focused_series[["Market (Mkt-RF)"]] <- mkt_series %>%
    arrange(date) %>%
    mutate(cum_wealth = cumprod(1 + period_ret), Series = "Market (Mkt-RF)") %>%
    select(date, cum_wealth, Series)
}

# Industry Momentum
if (!is.null(ind_mom)) {
  focused_series[["Industry Momentum"]] <- ind_mom %>%
    arrange(date) %>%
    mutate(
      cum_wealth = cumprod(1 + period_ret),
      Series = "Industry Momentum"
    ) %>%
    select(date, cum_wealth, Series)
}

# LS_Median_1M
if ("LS_Median_1M" %in% names(all_grid_results)) {
  focused_series[["LS Median (1M)"]] <- all_grid_results[["LS_Median_1M"]] %>%
    arrange(date) %>%
    mutate(cum_wealth = cumprod(1 + period_ret), Series = "LS Median (1M)") %>%
    select(date, cum_wealth, Series)
}

# LO_25_1M
if ("LO_25_1M" %in% names(all_grid_results)) {
  focused_series[["LO Top-25% (1M)"]] <- all_grid_results[["LO_25_1M"]] %>%
    arrange(date) %>%
    mutate(cum_wealth = cumprod(1 + period_ret), Series = "LO Top-25% (1M)") %>%
    select(date, cum_wealth, Series)
}

focused_cum <- bind_rows(focused_series)

# Add start row (cum_wealth = 1)
focused_start <- tibble(
  date = min(focused_cum$date) - 1,
  cum_wealth = 1.0,
  Series = unique(focused_cum$Series)
)
focused_plot <- bind_rows(focused_start, focused_cum) %>% arrange(Series, date)

focused_colors <- c(
  "Market (Mkt-RF)" = "black",
  "Industry Momentum" = "gray50",
  "LS Median (1M)" = "#e31a1c",
  "LO Top-25% (1M)" = "#1f78b4"
)
focused_linetypes <- c(
  "Market (Mkt-RF)" = "dotted",
  "Industry Momentum" = "dashed",
  "LS Median (1M)" = "solid",
  "LO Top-25% (1M)" = "solid"
)

p_focused <- ggplot(
  focused_plot,
  aes(x = date, y = cum_wealth, color = Series, linetype = Series)
) +
  geom_line(linewidth = 1) +
  scale_y_log10(
    labels = scales::comma_format(accuracy = 0.1),
    breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100)
  ) +
  scale_color_manual(values = focused_colors) +
  scale_linetype_manual(values = focused_linetypes) +
  labs(
    title = "Cumulative Return: Factor Momentum vs Benchmarks",
    subtitle = "1963\u2013 | Log Scale | Market (Mkt-RF) | Industry Momentum | LS Median 1M | LO Top-25% 1M",
    x = NULL,
    y = "Cumulative Wealth (Start = 1, Log Scale)",
    color = NULL,
    linetype = NULL,
    caption = "Data: JKP Daily Factors & French Data Library | Excess Returns"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

print(p_focused)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_Focused_CumReturn.pdf"),
  plot = p_focused,
  width = 11,
  height = 7
)
cat("   Saved: Arnott_Focused_CumReturn.pdf\n")


# ==============================================================================
# 13b. ARNOTT (2023) EXACT REPLICATION PLOT
#      Closest match to paper Figure: LS_Median_1M vs Industry Momentum vs Market
#      Log scale, starts at $1, minimal classic theme, paper-style colors
# ==============================================================================
cat("\n--- 13b. Arnott (2023) Exact Replication Plot ---\n")

arnott_series <- list()

if ("LS_Median_1M" %in% names(all_grid_results)) {
  arnott_series[["Factor Momentum (LS Median, 1M)"]] <-
    all_grid_results[["LS_Median_1M"]] %>%
    arrange(date) %>%
    mutate(
      cum_wealth = cumprod(1 + period_ret),
      Series = "Factor Momentum (LS Median, 1M)"
    ) %>%
    select(date, cum_wealth, Series)
}

if (!is.null(ind_mom)) {
  arnott_series[["Industry Momentum (LS Median, 1M)"]] <-
    ind_mom %>%
    arrange(date) %>%
    mutate(
      cum_wealth = cumprod(1 + period_ret),
      Series = "Industry Momentum (LS Median, 1M)"
    ) %>%
    select(date, cum_wealth, Series)
}

if (!is.null(mkt_series)) {
  arnott_series[["Market (Mkt-RF)"]] <-
    mkt_series %>%
    arrange(date) %>%
    mutate(
      cum_wealth = cumprod(1 + period_ret),
      Series = "Market (Mkt-RF)"
    ) %>%
    select(date, cum_wealth, Series)
}

arnott_cum <- bind_rows(arnott_series)
arnott_start <- tibble(
  date = min(arnott_cum$date) - 1,
  cum_wealth = 1.0,
  Series = unique(arnott_cum$Series)
)
arnott_plot <- bind_rows(arnott_start, arnott_cum) %>% arrange(Series, date)

arnott_colors <- c(
  "Factor Momentum (LS Median, 1M)" = "#1565C0",
  "Industry Momentum (LS Median, 1M)" = "#E53935",
  "Market (Mkt-RF)" = "#424242"
)
arnott_ltypes <- c(
  "Factor Momentum (LS Median, 1M)" = "solid",
  "Industry Momentum (LS Median, 1M)" = "solid",
  "Market (Mkt-RF)" = "dashed"
)
arnott_lwidths <- c(
  "Factor Momentum (LS Median, 1M)" = 1.1,
  "Industry Momentum (LS Median, 1M)" = 1.1,
  "Market (Mkt-RF)" = 0.8
)

p_arnott_replication <- ggplot(
  arnott_plot,
  aes(
    x = date,
    y = cum_wealth,
    color = Series,
    linetype = Series,
    linewidth = Series
  )
) +
  geom_line() +
  scale_y_log10(
    labels = scales::dollar_format(prefix = "$", accuracy = 0.1),
    breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50),
    minor_breaks = NULL
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(values = arnott_colors) +
  scale_linetype_manual(values = arnott_ltypes) +
  scale_linewidth_manual(values = arnott_lwidths) +
  labs(
    title = "Factor Momentum vs. Industry Momentum vs. Market",
    subtitle = paste0(
      "Cumulative Wealth, $1 Invested in 1963 | Log Scale | ",
      "LS Median, 1-Month Lookback | 1-Day Implementation Lag"
    ),
    x = NULL,
    y = "Cumulative Wealth (Log Scale)",
    color = NULL,
    linetype = NULL,
    linewidth = NULL,
    caption = paste0(
      "Replication of Arnott et al. (2023) | ",
      "Data: JKP Daily Factors & Ken French Data Library | Excess Returns"
    )
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50"),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.key.width = unit(2, "cm"),
    axis.line = element_line(color = "gray60"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1),
    linewidth = guide_legend(nrow = 1)
  )

print(p_arnott_replication)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_2023_Replication.pdf"),
  plot = p_arnott_replication,
  width = 11,
  height = 7
)
cat("   Saved: Arnott_2023_Replication.pdf\n")


# ==============================================================================
# 13c. ARNOTT EXACT 1-DAY LAG REPLICATION vs 2-DAY LAG vs BENCHMARKS
# ==============================================================================
cat("\n--- 13c. Arnott Exact 1-Day Lag Replication Plot ---\n")

arnott_exact_res <- calculate_factor_momentum(
  df = daily_factors_wide,
  factor_cols = factor_cols,
  lookback_days = 21,
  holding_days = HOLDING_DAYS,
  impl_lag = 0, # Arnott exact: signal at T, trade at T+1 (total_lag = 1)
  strategy = "LS_Median"
)

build_cum <- function(res, label) {
  res |>
    arrange(date) |>
    mutate(cum_wealth = cumprod(1 + period_ret), Series = label) |>
    select(date, cum_wealth, Series)
}

arnott_compare <- bind_rows(
  build_cum(arnott_exact_res, "Factor Mom \u2013 1-Day Lag (Arnott)"),
  build_cum(
    all_grid_results[["LS_Median_1M"]],
    "Factor Mom \u2013 2-Day Lag (Ours)"
  ),
  build_cum(ind_mom, "Industry Momentum"),
  build_cum(mkt_series, "Market (Mkt-RF)")
)

arnott_compare_start <- tibble(
  date = min(arnott_compare$date) - 1,
  cum_wealth = 1.0,
  Series = unique(arnott_compare$Series)
)
arnott_compare_plot <- bind_rows(arnott_compare_start, arnott_compare) |>
  arrange(Series, date)

arnott_exact_colors <- c(
  "Factor Mom \u2013 1-Day Lag (Arnott)" = "#1565C0",
  "Factor Mom \u2013 2-Day Lag (Ours)" = "#42A5F5",
  "Industry Momentum" = "#E53935",
  "Market (Mkt-RF)" = "#424242"
)
arnott_exact_ltypes <- c(
  "Factor Mom \u2013 1-Day Lag (Arnott)" = "solid",
  "Factor Mom \u2013 2-Day Lag (Ours)" = "dashed",
  "Industry Momentum" = "solid",
  "Market (Mkt-RF)" = "dotted"
)

p_arnott_exact <- ggplot(
  arnott_compare_plot,
  aes(x = date, y = cum_wealth, color = Series, linetype = Series)
) +
  geom_line(linewidth = 1) +
  scale_y_log10(
    labels = scales::dollar_format(prefix = "$", accuracy = 0.1),
    breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50),
    minor_breaks = NULL
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_color_manual(values = arnott_exact_colors) +
  scale_linetype_manual(values = arnott_exact_ltypes) +
  labs(
    title = "Factor Momentum: Arnott Exact Replication vs Our Implementation",
    subtitle = "LS Median, 1M Lookback | Blue solid = 1-day lag (Arnott exact) | Blue dashed = 2-day lag (Ours)",
    x = NULL,
    y = "Cumulative Wealth (Log Scale)",
    color = NULL,
    linetype = NULL,
    caption = "Replication of Arnott et al. (2023) | Data: JKP Daily Factors & Ken French | Excess Returns"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray50"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.8, "cm"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

print(p_arnott_exact)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_2023_Exact_1DayLag.pdf"),
  plot = p_arnott_exact,
  width = 11,
  height = 7
)
cat("   Saved: Arnott_2023_Exact_1DayLag.pdf\n")


# ==============================================================================
# 14. FINAL CONSOLE SUMMARY
# ==============================================================================
cat("\n==============================================================\n")
cat("  RESULTS SUMMARY\n")
cat("==============================================================\n")

top5 <- metrics_df %>%
  arrange(desc(Sharpe)) %>%
  slice_head(n = 5) %>%
  mutate(
    Ann_Ret = scales::percent(Ann_Ret, accuracy = 0.1),
    Ann_Vol = scales::percent(Ann_Vol, accuracy = 0.1),
    Sharpe = round(Sharpe, 2),
    Max_DD = scales::percent(Max_DD, accuracy = 0.1),
    Calmar = round(Calmar, 2)
  ) %>%
  select(Strategy, Lookback, Sharpe, Ann_Ret, Ann_Vol, Max_DD, Calmar)

cat("\nTop 5 Factor Momentum Combinations (by Sharpe):\n")
print(top5, n = 5)

if (!is.null(ind_metrics)) {
  cat("\nIndustry Momentum Benchmark:\n")
  cat(
    "  Sharpe =",
    round(ind_metrics$Sharpe, 2),
    "| Ann Ret =",
    scales::percent(ind_metrics$Ann_Ret, 0.1),
    "| Ann Vol =",
    scales::percent(ind_metrics$Ann_Vol, 0.1),
    "\n"
  )
}

if (!is.null(mkt_metrics)) {
  cat("\nMarket Factor Benchmark:\n")
  cat(
    "  Sharpe =",
    round(mkt_metrics$Sharpe, 2),
    "| Ann Ret =",
    scales::percent(mkt_metrics$Ann_Ret, 0.1),
    "| Ann Vol =",
    scales::percent(mkt_metrics$Ann_Vol, 0.1),
    "\n"
  )
}

cat("\n  Outputs saved to:\n")
cat("    Figures: ", OUTPUT_DIR, "\n")
cat("    Tables:  ", OUTPUT_TABLE, "\n")
cat("\n Arnott (2023) Phase 1 Replication Complete!\n")


# ==============================================================================
# ADDITIONAL EXPLORATORY PLOTS
# (Adapted from 99_Legacy/Plotting/Plot_Replication_Clean.R)
#
# Sections:
#   15a. Monthly factor returns (prerequisite)
#   15b. Correlation heatmap of individual factors
#   15c. Individual factor spaghetti plot
#   15d. Long vs. Short leg decomposition (LS Median, 1M)
#   15e. Drawdown analysis
#   15f. Pre / Post 2000 subperiod comparison
#   15g. Volatility-scaled factor momentum
#   15h. Factor cluster analysis (hierarchical)
#   15i. PCA analysis — systematic vs. idiosyncratic momentum
# ==============================================================================

# Load corrplot for the correlation heatmap
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
library(corrplot)


# ==============================================================================
# 15a. MONTHLY FACTOR RETURNS
#      Resample daily factor data to monthly by accumulating geometric returns
#      within each calendar month.  Used by sections 15b–15i.
# ==============================================================================
cat("\n--- 15a. Resampling daily factors to monthly returns ---\n")

monthly_factors <- daily_factors_wide %>%
  mutate(ym = format(date, "%Y-%m")) %>%
  group_by(ym) %>%
  summarise(
    date = max(date),
    across(
      all_of(factor_cols),
      ~ {
        vals <- .[!is.na(.)]
        if (length(vals) == 0) NA_real_ else prod(1 + vals) - 1
      }
    ),
    .groups = "drop"
  ) %>%
  arrange(date) %>%
  select(-ym)

cat(
  "   Monthly factors:",
  nrow(monthly_factors), "months x",
  length(factor_cols), "factors\n"
)


# ==============================================================================
# 15b. CORRELATION HEATMAP OF INDIVIDUAL FACTORS
# ==============================================================================
cat("\n--- 15b. Factor Correlation Heatmap ---\n")

corr_data <- monthly_factors %>%
  select(all_of(factor_cols)) %>%
  na.omit()

if (ncol(corr_data) >= 2 && nrow(corr_data) >= 10) {
  cor_mat    <- cor(corr_data)
  col_pal_bw <- colorRampPalette(c("#8B4513", "white", "#1E90FF"))(200)

  # Save to PDF
  pdf(
    file.path(OUTPUT_DIR, "Arnott_Factor_Correlation_Heatmap.pdf"),
    width = 14, height = 12
  )
  corrplot(
    cor_mat,
    method = "color",
    type   = "upper",
    order  = "hclust",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.6,
    col    = col_pal_bw,
    diag   = FALSE,
    cl.cex = 0.7,
    mar    = c(0, 0, 2, 0),
    title  = "Factor Return Correlation Matrix (Monthly, Hierarchically Ordered)"
  )
  dev.off()
  cat("   Saved: Arnott_Factor_Correlation_Heatmap.pdf\n")

  # Also display inline
  corrplot(
    cor_mat,
    method = "color",
    type   = "upper",
    order  = "hclust",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.6,
    col    = col_pal_bw,
    diag   = FALSE,
    cl.cex = 0.7,
    mar    = c(0, 0, 2, 0),
    title  = "Factor Return Correlation Matrix (Monthly, Hierarchically Ordered)"
  )
}


# ==============================================================================
# 15c. INDIVIDUAL FACTOR SPAGHETTI PLOT
#      Cumulative wealth of every individual factor, one line each.
# ==============================================================================
cat("\n--- 15c. Individual Factor Spaghetti Plot ---\n")

factor_cumulative <- monthly_factors %>%
  select(date, all_of(factor_cols)) %>%
  pivot_longer(-date, names_to = "factor_name", values_to = "monthly_ret") %>%
  filter(!is.na(monthly_ret)) %>%
  group_by(factor_name) %>%
  arrange(date) %>%
  mutate(cum_wealth = cumprod(1 + monthly_ret)) %>%
  ungroup()

p_spaghetti <- ggplot(
  factor_cumulative,
  aes(x = date, y = cum_wealth, group = factor_name)
) +
  geom_line(alpha = 0.35, linewidth = 0.4, color = "steelblue") +
  scale_y_log10(
    labels = scales::comma_format(accuracy = 0.1),
    breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50)
  ) +
  labs(
    title    = "Cumulative Wealth of All Individual Factors",
    subtitle = paste0(
      "Monthly returns (resampled from daily) | Log scale | N = ",
      length(factor_cols), " factors"
    ),
    x       = NULL,
    y       = "Cumulative Wealth (Start = 1, Log Scale)",
    caption = "Data: JKP Daily Factors"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    legend.position  = "none",
    panel.grid.minor = element_blank()
  )

print(p_spaghetti)
ggsave(
  file.path(OUTPUT_DIR, "Arnott_Factor_Spaghetti.pdf"),
  plot = p_spaghetti, width = 12, height = 7
)
cat("   Saved: Arnott_Factor_Spaghetti.pdf\n")


# ==============================================================================
# 15d. LONG vs. SHORT LEG DECOMPOSITION (LS Median, 1M lookback)
#      Signal = prior calendar month return; holding = current calendar month.
#      Short leg is inverted to show its PnL contribution.
# ==============================================================================
cat("\n--- 15d. Long vs. Short Leg Decomposition ---\n")

ls_decomp_raw <- monthly_factors %>%
  select(date, all_of(factor_cols)) %>%
  arrange(date) %>%
  mutate(across(all_of(factor_cols), ~ lag(.), .names = "{.col}_lag")) %>%
  na.omit() %>%
  rowwise() %>%
  mutate(
    sigs      = list(c_across(ends_with("_lag"))),
    rets      = list(c_across(all_of(factor_cols))),
    med_sig   = median(unlist(sigs), na.rm = TRUE),
    long_ret  = {
      s <- unlist(sigs); r <- unlist(rets)
      idx <- which(!is.na(s) & s > med_sig)
      if (length(idx) > 0) mean(r[idx], na.rm = TRUE) else 0
    },
    short_ret = {
      s <- unlist(sigs); r <- unlist(rets)
      idx <- which(!is.na(s) & s <= med_sig)
      if (length(idx) > 0) mean(r[idx], na.rm = TRUE) else 0
    },
    ls_ret    = long_ret - short_ret
  ) %>%
  ungroup() %>%
  select(date, long_ret, short_ret, ls_ret)

if (nrow(ls_decomp_raw) > 0) {
  ls_decomp_long <- ls_decomp_raw %>%
    mutate(short_ret_inv = -short_ret) %>%
    select(date,
           "Long Leg"              = long_ret,
           "Short Leg (inverted)"  = short_ret_inv,
           "Long\u2013Short"       = ls_ret) %>%
    pivot_longer(-date, names_to = "Leg", values_to = "ret") %>%
    group_by(Leg) %>%
    arrange(date) %>%
    mutate(cum_wealth = cumprod(1 + ret)) %>%
    ungroup()

  ls_leg_colors <- c(
    "Long Leg"             = "#2ca02c",
    "Short Leg (inverted)" = "#d62728",
    "Long\u2013Short"      = "#1f77b4"
  )

  p_ls_decomp <- ggplot(
    ls_decomp_long,
    aes(x = date, y = cum_wealth, color = Leg)
  ) +
    annotate("rect",
      xmin = ymd("2000-03-01"), xmax = ymd("2002-10-31"),
      ymin = 0, ymax = Inf, alpha = 0.07, fill = "gray30"
    ) +
    annotate("rect",
      xmin = ymd("2007-10-01"), xmax = ymd("2009-03-31"),
      ymin = 0, ymax = Inf, alpha = 0.07, fill = "gray30"
    ) +
    annotate("rect",
      xmin = ymd("2020-02-01"), xmax = ymd("2020-06-30"),
      ymin = 0, ymax = Inf, alpha = 0.07, fill = "gray30"
    ) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      labels = scales::comma_format(accuracy = 0.1),
      breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50)
    ) +
    scale_color_manual(values = ls_leg_colors) +
    labs(
      title    = "Factor Momentum: Long vs. Short Leg Decomposition",
      subtitle = "LS Median, 1M Lookback | Short leg inverted to show PnL contribution | Shaded = crises",
      x        = NULL,
      y        = "Cumulative Wealth (Log Scale)",
      color    = NULL,
      caption  = "Data: JKP Daily Factors (monthly resampled) | Excess Returns"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )

  print(p_ls_decomp)
  ggsave(
    file.path(OUTPUT_DIR, "Arnott_LS_Decomp_LongShort.pdf"),
    plot = p_ls_decomp, width = 11, height = 7
  )
  cat("   Saved: Arnott_LS_Decomp_LongShort.pdf\n")
}


# ==============================================================================
# 15e. DRAWDOWN ANALYSIS
#      Drawdown from peak for the key strategies and benchmarks.
# ==============================================================================
cat("\n--- 15e. Drawdown Analysis ---\n")

dd_series_list <- list()
if ("LS_Median_1M" %in% names(all_grid_results))
  dd_series_list[["Factor Mom (LS Median 1M)"]]   <- all_grid_results[["LS_Median_1M"]]
if ("LO_25_1M" %in% names(all_grid_results))
  dd_series_list[["Factor Mom (LO Top-25% 1M)"]]  <- all_grid_results[["LO_25_1M"]]
if (!is.null(ind_mom))
  dd_series_list[["Industry Momentum"]]            <- ind_mom
if (!is.null(mkt_series))
  dd_series_list[["Market (Mkt-RF)"]]              <- mkt_series

if (length(dd_series_list) > 0) {
  dd_df <- imap_dfr(dd_series_list, function(s, nm) {
    s %>%
      arrange(date) %>%
      mutate(
        cum_wealth = cumprod(1 + period_ret),
        peak       = cummax(cum_wealth),
        drawdown   = (cum_wealth - peak) / peak,
        Series     = nm
      ) %>%
      select(date, drawdown, Series)
  })

  dd_colors <- c(
    "Factor Mom (LS Median 1M)"   = "#e31a1c",
    "Factor Mom (LO Top-25% 1M)"  = "#1f78b4",
    "Industry Momentum"           = "gray50",
    "Market (Mkt-RF)"             = "black"
  )

  p_drawdown <- ggplot(dd_df, aes(x = date, y = drawdown, color = Series)) +
    geom_line(linewidth = 0.7) +
    scale_y_continuous(
      labels  = scales::percent_format(accuracy = 1),
      expand  = expansion(mult = c(0.05, 0))
    ) +
    scale_color_manual(
      values = dd_colors[names(dd_colors) %in% unique(dd_df$Series)]
    ) +
    labs(
      title    = "Drawdown from Peak",
      subtitle = "LS Median 1M | LO Top-25% 1M | Industry Momentum | Market",
      x        = NULL,
      y        = "Drawdown from Peak",
      color    = NULL,
      caption  = "Data: JKP Daily Factors & Ken French Data Library"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )

  print(p_drawdown)
  ggsave(
    file.path(OUTPUT_DIR, "Arnott_Drawdown_Analysis.pdf"),
    plot = p_drawdown, width = 11, height = 7
  )
  cat("   Saved: Arnott_Drawdown_Analysis.pdf\n")
}


# ==============================================================================
# 15f. PRE / POST 2000 SUBPERIOD COMPARISON
# ==============================================================================
cat("\n--- 15f. Pre/Post 2000 Subperiod Comparison ---\n")

subperiod_series <- list()
if ("LS_Median_1M" %in% names(all_grid_results))
  subperiod_series[["Factor Mom (LS Median 1M)"]] <- all_grid_results[["LS_Median_1M"]]
if (!is.null(ind_mom))
  subperiod_series[["Industry Momentum"]]          <- ind_mom
if (!is.null(mkt_series))
  subperiod_series[["Market (Mkt-RF)"]]            <- mkt_series

if (length(subperiod_series) > 0) {
  sub_df <- imap_dfr(subperiod_series, function(s, nm) {
    s %>% mutate(
      Series = nm,
      Period = if_else(year(date) < 2000, "Pre-2000", "Post-2000")
    )
  })

  sub_cum <- sub_df %>%
    arrange(Series, Period, date) %>%
    group_by(Series, Period) %>%
    mutate(cum_wealth = cumprod(1 + period_ret)) %>%
    ungroup()

  sub_metrics <- sub_df %>%
    group_by(Series, Period) %>%
    summarise(
      N       = n(),
      Ann_Ret = (prod(1 + period_ret))^(ANN_FACTOR / n()) - 1,
      Ann_Vol = sd(period_ret) * sqrt(ANN_FACTOR),
      Sharpe  = Ann_Ret / Ann_Vol,
      .groups = "drop"
    )
  cat("   Subperiod Metrics:\n")
  print(sub_metrics)

  sub_colors <- c(
    "Factor Mom (LS Median 1M)" = "#1565C0",
    "Industry Momentum"         = "#E53935",
    "Market (Mkt-RF)"           = "#424242"
  )

  p_subperiod <- ggplot(sub_cum, aes(x = date, y = cum_wealth, color = Series)) +
    geom_line(linewidth = 1) +
    facet_wrap(~Period, scales = "free_x") +
    scale_y_log10(
      labels = scales::dollar_format(prefix = "$", accuracy = 0.1)
    ) +
    scale_color_manual(
      values = sub_colors[names(sub_colors) %in% unique(sub_cum$Series)]
    ) +
    labs(
      title    = "Factor Momentum: Pre-2000 vs. Post-2000 Performance",
      subtitle = "Each panel re-indexed to $1 at period start | Log scale",
      x        = NULL,
      y        = "Cumulative Wealth (Log Scale)",
      color    = NULL,
      caption  = "Data: JKP Daily Factors & Ken French Data Library | Excess Returns"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank()
    )

  print(p_subperiod)
  ggsave(
    file.path(OUTPUT_DIR, "Arnott_Subperiod_Pre_Post_2000.pdf"),
    plot = p_subperiod, width = 12, height = 7
  )
  cat("   Saved: Arnott_Subperiod_Pre_Post_2000.pdf\n")
}


# ==============================================================================
# 15g. VOLATILITY-SCALED FACTOR MOMENTUM
#      Scale raw strategy returns to a target annualized volatility using a
#      rolling realized-vol estimate and a capped leverage multiplier.
# ==============================================================================
cat("\n--- 15g. Volatility-Scaled Factor Momentum ---\n")

TARGET_ANN_VOL_15g  <- 0.10  # 10% annualized target
VOLSCALE_LOOKBACK   <- 12    # periods lookback (in rebalancing units)
MAX_LEVERAGE        <- 5     # cap leverage at 5x

vol_scale_series <- function(period_rets, target_vol, lookback, max_lev, ann_fac) {
  rolling_vol <- rollapply(
    period_rets,
    width   = lookback,
    FUN     = sd,
    na.rm   = TRUE,
    fill    = NA,
    align   = "right",
    partial = max(6, floor(lookback / 2))
  ) * sqrt(ann_fac)
  leverage <- lag(pmin(max_lev, target_vol / rolling_vol), 1)
  leverage[is.na(leverage) | is.infinite(leverage)] <- 1
  period_rets * leverage
}

vs_list <- list()

for (key_vs in c("LS_Median_1M", "LO_25_1M")) {
  if (!(key_vs %in% names(all_grid_results))) next
  base_s <- all_grid_results[[key_vs]] %>% arrange(date)
  label_raw    <- paste0(key_vs, " (Raw)")
  label_scaled <- paste0(key_vs, " (Vol-Scaled)")
  scaled_rets  <- vol_scale_series(
    base_s$period_ret,
    TARGET_ANN_VOL_15g, VOLSCALE_LOOKBACK, MAX_LEVERAGE, ANN_FACTOR
  )
  vs_list[[label_raw]]    <- tibble(date = base_s$date, period_ret = base_s$period_ret,  type = label_raw)
  vs_list[[label_scaled]] <- tibble(date = base_s$date, period_ret = scaled_rets, type = label_scaled) %>%
    filter(!is.na(period_ret))
}

if (length(vs_list) > 0) {
  vs_df <- bind_rows(vs_list) %>%
    group_by(type) %>%
    arrange(date) %>%
    mutate(cum_wealth = cumprod(1 + period_ret)) %>%
    ungroup()

  vs_colors <- c(
    "LS_Median_1M (Raw)"         = "#d62728",
    "LS_Median_1M (Vol-Scaled)"  = "#ff7f0e",
    "LO_25_1M (Raw)"             = "#1f77b4",
    "LO_25_1M (Vol-Scaled)"      = "#17becf"
  )

  p_volscale <- ggplot(vs_df, aes(x = date, y = cum_wealth, color = type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(
      labels = scales::comma_format(accuracy = 0.1),
      breaks = c(0.5, 1, 2, 5, 10, 20, 50)
    ) +
    scale_color_manual(
      values = vs_colors[names(vs_colors) %in% unique(vs_df$type)]
    ) +
    labs(
      title    = paste0(
        "Volatility-Scaled Factor Momentum (Target = ",
        scales::percent(TARGET_ANN_VOL_15g, 1), " Ann. Vol)"
      ),
      subtitle = paste0(
        "Rolling ", VOLSCALE_LOOKBACK, "-period vol estimate | Max leverage ",
        MAX_LEVERAGE, "x | Log scale"
      ),
      x       = NULL,
      y       = "Cumulative Wealth (Log Scale)",
      color   = NULL,
      caption = "Data: JKP Daily Factors | Excess Returns"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )

  print(p_volscale)
  ggsave(
    file.path(OUTPUT_DIR, "Arnott_VolScaled_Momentum.pdf"),
    plot = p_volscale, width = 11, height = 7
  )
  cat("   Saved: Arnott_VolScaled_Momentum.pdf\n")
}


# ==============================================================================
# 15h. FACTOR CLUSTER ANALYSIS
#      Hierarchical clustering of factors by |correlation| distance (Ward.D2).
#      Run within-cluster LS Median 1M momentum via existing daily function.
# ==============================================================================
cat("\n--- 15h. Factor Cluster Analysis ---\n")

K_CLUSTERS_15h <- 5

clust_input <- monthly_factors %>% select(all_of(factor_cols)) %>% na.omit()

if (nrow(clust_input) >= 20 && ncol(clust_input) >= K_CLUSTERS_15h * 2) {
  clust_cor  <- cor(clust_input)
  clust_dist <- as.dist(1 - abs(clust_cor))
  clust_hc   <- hclust(clust_dist, method = "ward.D2")
  clust_cut  <- cutree(clust_hc, k = K_CLUSTERS_15h)
  cluster_map <- split(names(clust_cut), clust_cut)
  names(cluster_map) <- paste0("Cluster_", seq_len(K_CLUSTERS_15h))

  cat("   Factors per cluster:",
    paste(names(cluster_map), lengths(cluster_map), sep = "=", collapse = "  "), "\n")

  cluster_mom_list <- imap(cluster_map, function(facs, cname) {
    facs_ok <- intersect(facs, factor_cols)
    if (length(facs_ok) < 2) return(NULL)
    res <- tryCatch(
      calculate_factor_momentum(
        df           = daily_factors_wide,
        factor_cols  = facs_ok,
        lookback_days = 21,
        holding_days  = HOLDING_DAYS,
        impl_lag      = IMPL_LAG,
        strategy      = "LS_Median"
      ),
      error = function(e) NULL
    )
    if (!is.null(res)) res %>% mutate(Cluster = cname) else NULL
  })

  cluster_mom_df <- bind_rows(Filter(Negate(is.null), cluster_mom_list))

  if (nrow(cluster_mom_df) > 0) {
    cluster_mom_cum <- cluster_mom_df %>%
      group_by(Cluster) %>%
      arrange(date) %>%
      mutate(cum_wealth = cumprod(1 + period_ret)) %>%
      ungroup()

    p_cluster <- ggplot(
      cluster_mom_cum,
      aes(x = date, y = cum_wealth, color = Cluster)
    ) +
      geom_line(linewidth = 0.9) +
      scale_y_log10(
        labels = scales::comma_format(accuracy = 0.1),
        breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50)
      ) +
      labs(
        title    = "Factor Momentum by Correlation Cluster",
        subtitle = paste0(
          K_CLUSTERS_15h, " hierarchical clusters (Ward.D2) | LS Median, 1M lookback | Log scale"
        ),
        x       = NULL,
        y       = "Cumulative Wealth (Log Scale)",
        color   = NULL,
        caption = "Data: JKP Daily Factors | Clustering on |1 - corr| distance"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "bold", size = 14),
        legend.position  = "bottom",
        panel.grid.minor = element_blank()
      )

    print(p_cluster)
    ggsave(
      file.path(OUTPUT_DIR, "Arnott_Cluster_Momentum.pdf"),
      plot = p_cluster, width = 11, height = 7
    )
    cat("   Saved: Arnott_Cluster_Momentum.pdf\n")

    cluster_perf <- cluster_mom_df %>%
      group_by(Cluster) %>%
      summarise(
        N       = n(),
        Ann_Ret = (prod(1 + period_ret))^(ANN_FACTOR / n()) - 1,
        Ann_Vol = sd(period_ret) * sqrt(ANN_FACTOR),
        Sharpe  = Ann_Ret / Ann_Vol,
        .groups = "drop"
      ) %>%
      arrange(desc(Sharpe))
    cat("   Cluster performance (by Sharpe):\n")
    print(cluster_perf)
  }
}


# ==============================================================================
# 15i. PCA ANALYSIS — SYSTEMATIC vs. IDIOSYNCRATIC MOMENTUM
#      Run PCA on monthly factor returns, scale PC scores to 10% ann vol,
#      apply median-split momentum on top PCs, compare vs. full strategy.
# ==============================================================================
cat("\n--- 15i. PCA Analysis ---\n")

pca_input <- monthly_factors %>%
  select(date, all_of(factor_cols)) %>%
  na.omit()

if (nrow(pca_input) >= 36 && ncol(pca_input) >= 4) {
  pca_mat   <- pca_input %>% select(-date) %>% as.matrix()
  pca_model <- prcomp(pca_mat, scale. = TRUE)

  var_exp   <- (pca_model$sdev^2) / sum(pca_model$sdev^2)
  cum_var   <- cumsum(var_exp)
  n_pcs_80  <- min(which(cum_var >= 0.80))
  cat("   PCs needed to explain >=80% variance:", n_pcs_80, "\n")

  # --- Scree plot ---
  scree_df <- tibble(PC = seq_along(var_exp), VarExp = var_exp, CumVar = cum_var)

  p_scree <- ggplot(scree_df %>% head(min(20, nrow(scree_df))), aes(x = PC, y = VarExp)) +
    geom_col(fill = "#1f78b4", alpha = 0.8) +
    geom_line(aes(y = CumVar), color = "#e31a1c", linewidth = 1) +
    geom_point(aes(y = CumVar), color = "#e31a1c", size = 2) +
    geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray40") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = "PCA Scree Plot: Factor Return Variance Explained",
      subtitle = "Bars = individual PC | Red line = cumulative | Dashed = 80% threshold",
      x        = "Principal Component",
      y        = "Variance Explained",
      caption  = "Data: JKP Daily Factors (monthly resampled)"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", size = 14))

  print(p_scree)
  ggsave(
    file.path(OUTPUT_DIR, "Arnott_PCA_Scree.pdf"),
    plot = p_scree, width = 10, height = 6
  )
  cat("   Saved: Arnott_PCA_Scree.pdf\n")

  # --- PC returns scaled to 10% ann vol ---
  target_monthly_sd_pca <- 0.10 / sqrt(12)
  pc_scores_raw    <- as.data.frame(pca_model$x[, 1:n_pcs_80, drop = FALSE])
  pc_scores_scaled <- pc_scores_raw %>%
    mutate(across(everything(), ~ .x * (target_monthly_sd_pca / sd(.x)))) %>%
    bind_cols(date = pca_input$date, .)
  pc_cols_pca <- paste0("PC", 1:n_pcs_80)

  # --- Monthly momentum on PC scores (1-month lookback, median split) ---
  pca_mom_df <- pc_scores_scaled %>%
    select(date, all_of(pc_cols_pca)) %>%
    arrange(date) %>%
    mutate(across(all_of(pc_cols_pca), ~ lag(.), .names = "{.col}_lag")) %>%
    na.omit() %>%
    rowwise() %>%
    mutate(
      sigs    = list(c_across(ends_with("_lag"))),
      rets    = list(c_across(all_of(pc_cols_pca))),
      med     = median(unlist(sigs), na.rm = TRUE),
      pca_ret = {
        s <- unlist(sigs); r <- unlist(rets)
        li <- which(s > med); si <- which(s <= med)
        (if (length(li) > 0) mean(r[li]) else 0) -
          (if (length(si) > 0) mean(r[si]) else 0)
      }
    ) %>%
    ungroup() %>%
    select(date, period_ret = pca_ret)

  # --- Compare PCA mom vs. full LS_Median_1M ---
  if ("LS_Median_1M" %in% names(all_grid_results) && nrow(pca_mom_df) > 0) {
    full_mom_pca <- all_grid_results[["LS_Median_1M"]] %>%
      mutate(ym = format(date, "%Y-%m")) %>%
      group_by(ym) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      select(date, period_ret)

    pca_compare <- bind_rows(
      full_mom_pca %>% mutate(Strategy = "All Factors (LS Median 1M)"),
      pca_mom_df   %>% mutate(Strategy = paste0("PCA Momentum (Top ", n_pcs_80, " PCs)"))
    ) %>%
      group_by(Strategy) %>%
      arrange(date) %>%
      mutate(cum_wealth = cumprod(1 + period_ret)) %>%
      ungroup()

    pca_colors <- setNames(
      c("#1565C0", "#e65100"),
      c("All Factors (LS Median 1M)", paste0("PCA Momentum (Top ", n_pcs_80, " PCs)"))
    )

    p_pca <- ggplot(pca_compare, aes(x = date, y = cum_wealth, color = Strategy)) +
      geom_line(linewidth = 1) +
      scale_y_log10(
        labels = scales::comma_format(accuracy = 0.1),
        breaks = c(0.5, 1, 2, 5, 10, 20, 50)
      ) +
      scale_color_manual(values = pca_colors) +
      labs(
        title    = "PCA Momentum vs. Full Factor Momentum",
        subtitle = paste0(
          "Top ", n_pcs_80, " PCs explain 80% of factor variance | ",
          "Monthly median-split | PC scores scaled to 10% ann vol"
        ),
        x       = NULL,
        y       = "Cumulative Wealth (Log Scale)",
        color   = NULL,
        caption = "Data: JKP Daily Factors (monthly resampled)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "bold", size = 14),
        legend.position  = "bottom",
        panel.grid.minor = element_blank()
      )

    print(p_pca)
    ggsave(
      file.path(OUTPUT_DIR, "Arnott_PCA_Momentum.pdf"),
      plot = p_pca, width = 11, height = 7
    )
    cat("   Saved: Arnott_PCA_Momentum.pdf\n")
  }
}


# ==============================================================================
# ADDITIONAL EXPLORATORY PLOTS — SUMMARY
# ==============================================================================
cat("\n==============================================================\n")
cat("  ADDITIONAL EXPLORATORY PLOTS COMPLETE\n")
cat("  New output files in", OUTPUT_DIR, ":\n")
cat("    Arnott_Factor_Correlation_Heatmap.pdf\n")
cat("    Arnott_Factor_Spaghetti.pdf\n")
cat("    Arnott_LS_Decomp_LongShort.pdf\n")
cat("    Arnott_Drawdown_Analysis.pdf\n")
cat("    Arnott_Subperiod_Pre_Post_2000.pdf\n")
cat("    Arnott_VolScaled_Momentum.pdf\n")
cat("    Arnott_Cluster_Momentum.pdf\n")
cat("    Arnott_PCA_Scree.pdf\n")
cat("    Arnott_PCA_Momentum.pdf\n")
cat("==============================================================\n")
