# ==============================================================================
#  Quant Lab — Arnott (2023) Factor Momentum + Analysis Dashboard
# ==============================================================================

# --- 0. Libraries ---
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  shiny,
  shinymanager,
  dplyr,
  tidyr,
  purrr,
  lubridate,
  stringr,
  RSQLite,
  ggplot2,
  scales,
  corrplot,
  RColorBrewer,
  zoo,
  broom,
  arrow,
  frenchdata
)

# --- 1. Load Pre-processed Data Bundle ---
load("quant_data.RData")
factor_rename_map <- app_data$factor_rename_map
ff_reg_factors_loaded <- app_data$ff_reg_factors_loaded

# ==============================================================================
# SHARED HELPER FUNCTIONS
# ==============================================================================
compute_weights <- function(signals, strategy) {
  n <- length(signals)
  weights <- rep(0.0, n)
  if (n < 4) {
    return(weights)
  }
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
  weights
}

calculate_factor_momentum <- function(
  df,
  factor_cols,
  lookback_days = 21,
  holding_days = 21,
  impl_lag = 0,
  strategy = "LS_Median"
) {
  cols_exist <- intersect(factor_cols, colnames(df))
  if (length(cols_exist) < 4) {
    return(NULL)
  }
  total_lag <- 1L + as.integer(impl_lag)
  min_factors <- 4L

  df_signals <- df |>
    dplyr::select(date, dplyr::all_of(cols_exist)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols_exist),
      \(x) {
        as.numeric(zoo::rollapply(
          log(1 + x),
          width = lookback_days,
          FUN = sum,
          fill = NA,
          align = "right"
        ))
      },
      .names = "{.col}_signal"
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::ends_with("_signal"),
      \(x) dplyr::lag(x, n = total_lag)
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols_exist),
      \(x) {
        as.numeric(zoo::rollapply(
          log(1 + x),
          width = holding_days,
          FUN = sum,
          fill = NA,
          align = "left"
        ))
      },
      .names = "{.col}_fwd"
    ))

  signal_cols <- paste0(cols_exist, "_signal")
  fwd_cols <- paste0(cols_exist, "_fwd")

  df_valid <- df_signals |>
    dplyr::filter(
      rowSums(!is.na(dplyr::across(dplyr::all_of(signal_cols)))) >= min_factors,
      rowSums(!is.na(dplyr::across(dplyr::all_of(fwd_cols)))) >= min_factors
    )
  if (nrow(df_valid) < 2) {
    return(NULL)
  }

  rebal_idx <- seq(1, nrow(df_valid), by = holding_days)
  df_rebal <- df_valid[rebal_idx, ]

  period_rets <- purrr::map_dbl(seq_len(nrow(df_rebal)), function(i) {
    sig <- as.numeric(df_rebal[i, signal_cols])
    fwd <- as.numeric(df_rebal[i, fwd_cols])
    vi <- !is.na(sig) & !is.na(fwd)
    if (sum(vi) < min_factors) {
      return(NA_real_)
    }
    sum(compute_weights(sig[vi], strategy) * (exp(fwd[vi]) - 1), na.rm = TRUE)
  })

  tibble::tibble(date = df_rebal$date, period_ret = period_rets) |>
    dplyr::filter(!is.na(period_ret))
}

compute_metrics <- function(period_rets, ann_factor) {
  r <- period_rets[!is.na(period_rets)]
  n <- length(r)
  if (n < 2) {
    return(NULL)
  }
  cw <- cumprod(1 + r)
  max_dd <- min((cw - cummax(cw)) / cummax(cw))
  ann_ret <- cw[n]^(ann_factor / n) - 1
  ann_vol <- sd(r) * sqrt(ann_factor)
  tibble(
    N = n,
    Ann_Ret = ann_ret,
    Ann_Vol = ann_vol,
    Sharpe = if (ann_vol > 0) ann_ret / ann_vol else NA_real_,
    Cum_Ret = cw[n] - 1,
    Max_DD = max_dd,
    Calmar = if (abs(max_dd) > 1e-10) ann_ret / abs(max_dd) else NA_real_
  )
}

build_cum_wealth <- function(res, label) {
  res |>
    arrange(date) |>
    mutate(cum_wealth = cumprod(1 + period_ret), Series = label) |>
    select(date, cum_wealth, Series)
}

add_start_row <- function(df) {
  bind_rows(
    tibble(
      date = min(df$date) - 1,
      cum_wealth = 1.0,
      Series = unique(df$Series)
    ),
    df
  ) |>
    arrange(Series, date)
}

compound_mkt <- function(ref_dates, mkt_df) {
  purrr::map_dfr(seq_along(ref_dates), function(i) {
    d_s <- if (i == 1) min(mkt_df$date) else ref_dates[i - 1]
    d_e <- ref_dates[i]
    w <- dplyr::filter(mkt_df, date > d_s & date <= d_e)
    tibble::tibble(date = d_e, period_ret = prod(1 + w$mkt_rf) - 1)
  })
}

fmt_pct <- function(x, acc = 0.1) scales::percent(x, accuracy = acc)
fmt_tbl <- function(m, name) {
  if (is.null(m)) {
    return(NULL)
  }
  tibble(
    Strategy = name,
    N = m$N,
    `Ann. Return` = fmt_pct(m$Ann_Ret),
    `Ann. Vol` = fmt_pct(m$Ann_Vol),
    Sharpe = round(m$Sharpe, 2),
    `Cum. Return` = fmt_pct(m$Cum_Ret),
    `Max DD` = fmt_pct(m$Max_DD),
    Calmar = round(m$Calmar, 2)
  )
}

# Fixed pal/lty helpers — take actual series labels present in data
make_pal <- function(series_in_data) {
  full_pal <- c(
    "Market (Mkt-RF)" = "#424242",
    "Industry Momentum" = "#E53935",
    "LO Top-25% 1M" = "#2e7d32",
    "LO Median 1M" = "#6a1b9a",
    "LS 25% 1M" = "#e65100"
  )
  # Factor momentum gets blue regardless of lag label
  factor_mom <- series_in_data[grepl(
    "^Factor Momentum|^LS_|^LO_|^LS Median|Median 1M",
    series_in_data
  )]
  extra <- setNames(rep("#1565C0", length(factor_mom)), factor_mom)
  c(extra, full_pal)[series_in_data]
}

make_lty <- function(series_in_data) {
  lty_map <- function(s) {
    if (grepl("Market", s)) {
      return("dotted")
    }
    if (grepl("Industry", s)) {
      return("dashed")
    }
    return("solid")
  }
  setNames(sapply(series_in_data, lty_map), series_in_data)
}

theme_ql <- function(base = 13) {
  theme_classic(base_size = base) %+replace%
    theme(
      plot.title = element_text(face = "bold", size = base + 1),
      plot.subtitle = element_text(size = base - 2, color = "gray30"),
      plot.caption = element_text(size = base - 4, color = "gray50"),
      legend.position = "bottom",
      legend.key.width = unit(1.8, "cm"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor = element_blank()
    )
}

apply_yscale <- function(p, scale_val) {
  if (scale_val == "log") {
    p +
      scale_y_log10(
        labels = dollar_format(prefix = "$", accuracy = 0.1),
        breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)
      )
  } else {
    p + scale_y_continuous(labels = dollar_format(prefix = "$", accuracy = 0.1))
  }
}

# ==============================================================================
# CONSTANTS
# ==============================================================================
LOOKBACK_LABELS <- c(
  "1M" = "21",
  "2M" = "42",
  "3M" = "63",
  "6M" = "126",
  "12M" = "252"
)
STRATEGY_TYPES <- c(
  "LS_Median",
  "LS_33",
  "LS_25",
  "LO_Median",
  "LO_33",
  "LO_25"
)
HOLDING_DAYS <- 21L
ANN_FACTOR <- 252 / HOLDING_DAYS
GLOBAL_START <- as.Date("1963-01-01")

# ==============================================================================
# PRE-LOAD DAILY DATA
# ==============================================================================
daily_factors_wide <- tryCatch(
  {
    arrow::read_parquet(
      "../01_Data/Processed/USA_Valid_Factor_Returns_Daily.parquet"
    ) |>
      tidyr::pivot_wider(
        names_from = characteristic,
        values_from = factor_ret
      ) |>
      dplyr::arrange(date) |>
      dplyr::filter(date >= GLOBAL_START)
  },
  error = function(e) {
    message("Parquet load failed: ", e$message)
    NULL
  }
)

daily_factor_cols <- if (!is.null(daily_factors_wide)) {
  setdiff(colnames(daily_factors_wide), "date")
} else {
  character(0)
}

mkt_daily_global <- tryCatch(
  {
    frenchdata::download_french_data(
      "Fama/French 3 Factors [Daily]"
    )$subsets$data[[1]] |>
      dplyr::mutate(
        date = lubridate::ymd(date),
        mkt_rf = as.numeric(`Mkt-RF`) / 100
      ) |>
      dplyr::select(date, mkt_rf) |>
      dplyr::filter(!is.na(mkt_rf), date >= GLOBAL_START)
  },
  error = function(e) NULL
)

ind_daily_global <- tryCatch(
  {
    frenchdata::download_french_data(
      "17 Industry Portfolios [Daily]"
    )$subsets$data[[1]] |>
      dplyr::mutate(date = lubridate::ymd(date)) |>
      dplyr::mutate(dplyr::across(-date, ~ as.numeric(.) / 100)) |>
      dplyr::arrange(date) |>
      dplyr::filter(date >= GLOBAL_START)
  },
  error = function(e) NULL
)

ind_cols_global <- if (!is.null(ind_daily_global)) {
  setdiff(colnames(ind_daily_global), "date")
} else {
  character(0)
}

# ==============================================================================
# UI HELPERS
# ==============================================================================
lag_radio <- function(id, sel = "0") {
  radioButtons(
    id,
    "Implementation Lag:",
    choices = c("1-Day — Arnott exact" = "0", "2-Day — conservative" = "1"),
    selected = sel
  )
}
date_range_ui <- function(id, label = "Analysis Period:") {
  dateRangeInput(
    id,
    label,
    start = "1963-01-01",
    end = "2023-12-31",
    format = "yyyy-mm-dd",
    min = "1963-01-01",
    max = "2023-12-31"
  )
}
scale_radio <- function(id, sel = "log") {
  radioButtons(
    id,
    "Y-Axis Scale:",
    choices = c("Log Scale" = "log", "Linear" = "linear"),
    selected = sel
  )
}
mbox <- function(id) div(class = "metric-box", uiOutput(id))

# ==============================================================================
# UI
# ==============================================================================
ui <- shinymanager::secure_app(tagList(
  tags$head(tags$style(HTML(
    "
    body{font-family:'Helvetica Neue',Arial,sans-serif;}
    .well{background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;}
    h4{color:#1565C0;margin-top:10px;font-weight:600;}
    h5{color:#2c3e50;font-weight:600;}
    .metric-box{background:#f0f4ff;border-left:4px solid #1565C0;
      padding:8px 14px;margin:4px 2px;border-radius:4px;}
    .btn-primary{background-color:#1565C0;border-color:#1565C0;}
  "
  ))),
  navbarPage(
    title = span(strong("Quant Lab"), " — Arnott (2023) Factor Momentum"),
    id = "mainNav",

    # TAB 1: ARNOTT REPLICATION
    tabPanel(
      "📈 Arnott Replication",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Settings"),
          lag_radio("arnott_lag"),
          hr(),
          date_range_ui("arnott_dates"),
          hr(),
          scale_radio("arnott_scale"),
          hr(),
          checkboxInput("arnott_show_mkt", "Show Market", TRUE),
          checkboxInput("arnott_show_ind", "Show Industry Momentum", TRUE),
          hr(),
          p(
            style = "font-size:0.82em;color:#555;",
            "Arnott (2023): LS Median, 1M lookback, 1-day lag.",
            br(),
            "All returns excess of risk-free rate.",
            br(),
            "Source: JKP Daily + Ken French Data Library."
          )
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(3, mbox("arn_sharpe")),
            column(3, mbox("arn_ret")),
            column(3, mbox("arn_vol")),
            column(3, mbox("arn_dd"))
          ),
          br(),
          plotOutput("arnott_plot", height = "460px"),
          br(),
          h5("Performance Summary"),
          tableOutput("arnott_table")
        )
      )
    ),

    # TAB 2: PARAMETER GRID
    tabPanel(
      "🔲 Parameter Grid",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Grid Settings"),
          checkboxGroupInput(
            "grid_lbs",
            "Lookback Windows:",
            choices = LOOKBACK_LABELS,
            selected = unname(LOOKBACK_LABELS)
          ),
          hr(),
          checkboxGroupInput(
            "grid_strats",
            "Strategies:",
            choices = STRATEGY_TYPES,
            selected = STRATEGY_TYPES
          ),
          hr(),
          lag_radio("grid_lag"),
          hr(),
          radioButtons(
            "grid_metric",
            "Heatmap Metric:",
            choices = c("Sharpe Ratio" = "Sharpe", "Ann. Return" = "Ann_Ret"),
            selected = "Sharpe"
          ),
          hr(),
          date_range_ui("grid_dates"),
          hr(),
          actionButton(
            "run_grid",
            "▶  Run Grid",
            class = "btn-primary btn-block",
            style = "font-weight:600;"
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Heatmap",
              br(),
              plotOutput("grid_heatmap", height = "420px"),
              br(),
              h5("Full Results Table"),
              tableOutput("grid_table")
            ),
            tabPanel(
              "Cumulative Wealth",
              br(),
              scale_radio("grid_cum_scale"),
              plotOutput("grid_cumwealth", height = "640px")
            )
          )
        )
      )
    ),

    # TAB 3: FOCUSED COMPARISON
    tabPanel(
      "🎯 Focused Comparison",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Series to Display"),
          checkboxGroupInput(
            "focus_series",
            NULL,
            choices = c(
              "Market (Mkt-RF)" = "market",
              "Industry Momentum" = "ind_mom",
              "LS Median 1M" = "ls_median",
              "LO Top-25% 1M" = "lo_25",
              "LO Median 1M" = "lo_median",
              "LS 25% 1M" = "ls_25"
            ),
            selected = c("market", "ind_mom", "ls_median", "lo_25")
          ),
          hr(),
          lag_radio("focus_lag"),
          hr(),
          date_range_ui("focus_dates"),
          hr(),
          scale_radio("focus_scale")
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(3, mbox("foc_sharpe")),
            column(3, mbox("foc_ret")),
            column(3, mbox("foc_dd")),
            column(3, mbox("foc_calmar"))
          ),
          br(),
          plotOutput("focus_plot", height = "460px"),
          br(),
          h5("Performance Summary"),
          tableOutput("focus_table")
        )
      )
    ),

    # TAB 4: STRATEGY EXPLORER (daily engine)
    tabPanel(
      "🔍 Strategy Explorer",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Strategy Parameters"),
          radioButtons(
            "exp_strategy",
            "Strategy Type:",
            choices = STRATEGY_TYPES,
            selected = "LS_Median"
          ),
          hr(),
          radioButtons(
            "exp_lookback",
            "Lookback Window:",
            choices = LOOKBACK_LABELS,
            selected = "21"
          ),
          hr(),
          lag_radio("exp_lag"),
          hr(),
          checkboxInput("exp_vs_market", "vs Market", TRUE),
          checkboxInput("exp_vs_ind", "vs Industry Momentum", TRUE),
          hr(),
          date_range_ui("exp_dates"),
          hr(),
          scale_radio("exp_scale"),
          hr(),
          actionButton(
            "run_explorer",
            "▶  Run",
            class = "btn-primary btn-block",
            style = "font-weight:600;"
          )
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(3, mbox("exp_sharpe")),
            column(3, mbox("exp_ret")),
            column(3, mbox("exp_vol")),
            column(3, mbox("exp_dd"))
          ),
          br(),
          plotOutput("explorer_plot", height = "440px"),
          br(),
          h5("Performance Summary"),
          tableOutput("explorer_table"),
          br(),
          h5("Period Return Distribution"),
          plotOutput("explorer_dist", height = "260px")
        )
      )
    ),

    # TAB 5: CORRELATIONS
    tabPanel(
      "🔗 Correlations",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Settings"),
          radioButtons(
            "corr_order",
            "Factor Ordering:",
            choices = c(
              "Original" = "original",
              "Hierarchical" = "hclust",
              "Alphabet" = "alphabet"
            ),
            selected = "hclust"
          ),
          radioButtons(
            "corr_type",
            "Display Type:",
            choices = c(
              "Color + Number" = "number",
              "Color only" = "color",
              "Ellipse" = "ellipse"
            ),
            selected = "color"
          ),
          hr(),
          sliderInput(
            "corr_font",
            "Label Size:",
            min = 0.3,
            max = 1.2,
            value = 0.65,
            step = 0.05
          ),
          hr(),
          date_range_ui("corr_dates", "Sample Period:")
        ),
        mainPanel(width = 9, plotOutput("corr_plot", height = "720px"))
      )
    ),

    # TAB 6: REGRESSION
    tabPanel(
      "📊 Regression",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Target Series"),
          radioButtons(
            "reg_type",
            "Regress on:",
            choices = c(
              "JKP Factor" = "jkp",
              "Factor Momentum Strategy" = "mom"
            ),
            selected = "mom"
          ),
          conditionalPanel(
            "input.reg_type == 'jkp'",
            uiOutput("reg_factor_ui")
          ),
          conditionalPanel(
            "input.reg_type == 'mom'",
            radioButtons(
              "reg_strat",
              "Strategy:",
              choices = STRATEGY_TYPES,
              selected = "LS_Median"
            ),
            radioButtons(
              "reg_lb",
              "Lookback:",
              choices = LOOKBACK_LABELS,
              selected = "21"
            ),
            lag_radio("reg_lag")
          ),
          hr(),
          h4("Model Factors"),
          checkboxGroupInput(
            "reg_ff",
            "Fama-French Factors:",
            choices = c("mkt_excess", "smb", "hml", "rmw", "cma", "mom"),
            selected = c("mkt_excess", "smb", "hml")
          ),
          hr(),
          date_range_ui("reg_dates"),
          hr(),
          sliderInput(
            "reg_window",
            "Rolling Window (months):",
            min = 12,
            max = 60,
            value = 36,
            step = 6
          )
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(4, mbox("reg_alpha")),
            column(4, mbox("reg_r2")),
            column(4, mbox("reg_beta"))
          ),
          br(),
          h5("OLS Regression Summary"),
          verbatimTextOutput("reg_summary"),
          br(),
          h5("Rolling Alpha"),
          plotOutput("reg_rolling", height = "320px")
        )
      )
    )
  )
))

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = "credentials.sqlite",
      passphrase = "Ilab_member_12_1"
    )
  )

  flt <- function(df, ri) dplyr::filter(df, date >= ri[1], date <= ri[2])

  # ── 1. ARNOTT REPLICATION ──────────────────────────────────────────────────
  arnott_res <- eventReactive(
    list(input$arnott_lag, input$arnott_show_mkt, input$arnott_show_ind),
    {
      req(!is.null(daily_factors_wide))
      lag_val <- as.integer(input$arnott_lag)
      withProgress(message = "Computing Arnott replication…", value = 0.1, {
        fac <- calculate_factor_momentum(
          daily_factors_wide,
          daily_factor_cols,
          21,
          HOLDING_DAYS,
          lag_val,
          "LS_Median"
        )
        incProgress(0.35)
        ind <- if (
          isTRUE(input$arnott_show_ind) && !is.null(ind_daily_global)
        ) {
          calculate_factor_momentum(
            ind_daily_global,
            ind_cols_global,
            21,
            HOLDING_DAYS,
            lag_val,
            "LS_Median"
          )
        } else {
          NULL
        }
        incProgress(0.35)
        mkt <- if (
          isTRUE(input$arnott_show_mkt) && !is.null(mkt_daily_global)
        ) {
          compound_mkt(fac$date, mkt_daily_global)
        } else {
          NULL
        }
        incProgress(0.2)
        list(fac = fac, ind = ind, mkt = mkt)
      })
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  arnott_cum <- reactive({
    d <- arnott_res()
    req(d)
    # Use a simple, stable label for the factor momentum series
    ll <- if (input$arnott_lag == "0") "1-Day Lag" else "2-Day Lag"
    fac_lbl <- paste0("Factor Momentum (", ll, ")")
    rows <- list(build_cum_wealth(d$fac, fac_lbl))
    if (!is.null(d$ind)) {
      rows <- c(rows, list(build_cum_wealth(d$ind, "Industry Momentum")))
    }
    if (!is.null(d$mkt)) {
      rows <- c(rows, list(build_cum_wealth(d$mkt, "Market (Mkt-RF)")))
    }
    bind_rows(rows) |> add_start_row() |> flt(input$arnott_dates)
  })

  arnott_met <- reactive({
    d <- arnott_res()
    req(d)
    compute_metrics(flt(d$fac, input$arnott_dates)$period_ret, ANN_FACTOR)
  })

  output$arn_sharpe <- renderUI({
    m <- arnott_met()
    req(m)
    tagList(strong("Sharpe"), br(), h4(round(m$Sharpe, 2)))
  })
  output$arn_ret <- renderUI({
    m <- arnott_met()
    req(m)
    tagList(strong("Ann. Return"), br(), h4(fmt_pct(m$Ann_Ret)))
  })
  output$arn_vol <- renderUI({
    m <- arnott_met()
    req(m)
    tagList(strong("Ann. Vol"), br(), h4(fmt_pct(m$Ann_Vol)))
  })
  output$arn_dd <- renderUI({
    m <- arnott_met()
    req(m)
    tagList(strong("Max DD"), br(), h4(fmt_pct(m$Max_DD)))
  })

  output$arnott_plot <- renderPlot({
    df <- arnott_cum()
    req(nrow(df) > 0)
    ll <- if (input$arnott_lag == "0") {
      "1-Day Lag (Arnott exact)"
    } else {
      "2-Day Lag"
    }
    ser <- unique(df$Series)
    p <- ggplot(df, aes(date, cum_wealth, color = Series, linetype = Series)) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = make_pal(ser)) +
      scale_linetype_manual(values = make_lty(ser)) +
      scale_x_date(
        date_breaks = "5 years",
        date_labels = "%Y",
        expand = expansion(mult = 0.01)
      ) +
      labs(
        title = "Replication of Arnott et al. (2023)",
        subtitle = paste0(
          "Factor Momentum vs Industry Momentum vs Market | ",
          ll,
          " | ",
          format(input$arnott_dates[1], "%Y"),
          "–",
          format(input$arnott_dates[2], "%Y")
        ),
        x = NULL,
        y = "Cumulative Wealth",
        color = NULL,
        linetype = NULL,
        caption = "Data: JKP Daily Factors & Ken French | Excess Returns"
      ) +
      theme_ql() +
      guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))
    apply_yscale(p, input$arnott_scale)
  })

  output$arnott_table <- renderTable({
    d <- arnott_res()
    req(d)
    ll <- if (input$arnott_lag == "0") "1-Day" else "2-Day"
    dr <- input$arnott_dates
    rows <- list(fmt_tbl(
      compute_metrics(flt(d$fac, dr)$period_ret, ANN_FACTOR),
      paste0("Factor Momentum LS Median 1M (", ll, ")")
    ))
    if (!is.null(d$ind)) {
      rows <- c(
        rows,
        list(fmt_tbl(
          compute_metrics(flt(d$ind, dr)$period_ret, ANN_FACTOR),
          "Industry Momentum"
        ))
      )
    }
    if (!is.null(d$mkt)) {
      rows <- c(
        rows,
        list(fmt_tbl(
          compute_metrics(flt(d$mkt, dr)$period_ret, ANN_FACTOR),
          "Market (Mkt-RF)"
        ))
      )
    }
    bind_rows(rows)
  })

  # ── 2. PARAMETER GRID ─────────────────────────────────────────────────────
  grid_res <- eventReactive(
    input$run_grid,
    {
      req(
        !is.null(daily_factors_wide),
        length(input$grid_lbs) > 0,
        length(input$grid_strats) > 0
      )
      sel_lbs <- as.integer(input$grid_lbs)
      lag_val <- as.integer(input$grid_lag)
      withProgress(message = "Running parameter grid…", value = 0, {
        all_res <- list()
        all_met <- list()
        step <- 1 / (length(sel_lbs) * length(input$grid_strats))
        for (lb in sel_lbs) {
          lb_lbl <- names(LOOKBACK_LABELS)[LOOKBACK_LABELS == as.character(lb)]
          for (st in input$grid_strats) {
            key <- paste0(st, "_", lb_lbl)
            res <- tryCatch(
              calculate_factor_momentum(
                daily_factors_wide,
                daily_factor_cols,
                lb,
                HOLDING_DAYS,
                lag_val,
                st
              ),
              error = function(e) NULL
            )
            if (!is.null(res) && nrow(res) > 1) {
              all_res[[key]] <- res |> mutate(Strategy = st, Lookback = lb_lbl)
              all_met[[key]] <- compute_metrics(res$period_ret, ANN_FACTOR) |>
                mutate(Strategy = st, Lookback = lb_lbl)
            }
            incProgress(step)
          }
        }
        list(results = all_res, metrics = bind_rows(all_met))
      })
    },
    ignoreNULL = TRUE
  )

  output$grid_heatmap <- renderPlot({
    gd <- grid_res()
    req(gd)
    metric <- input$grid_metric
    lb_ord <- names(LOOKBACK_LABELS)
    st_ord <- STRATEGY_TYPES
    hm <- gd$metrics |>
      mutate(
        Lookback = factor(Lookback, levels = lb_ord[lb_ord %in% Lookback]),
        Strategy = factor(Strategy, levels = st_ord[st_ord %in% Strategy]),
        val = .data[[metric]],
        lbl = if (metric == "Sharpe") {
          as.character(round(val, 2))
        } else {
          fmt_pct(val)
        }
      )
    ttl <- if (metric == "Sharpe") "Sharpe Ratio" else "Annualized Return"
    ggplot(hm, aes(Lookback, Strategy, fill = val)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = lbl), size = 4, fontface = "bold") +
      scale_fill_gradient2(
        low = "#d73027",
        mid = "#ffffbf",
        high = "#1a9850",
        midpoint = 0,
        name = ttl,
        labels = if (metric == "Sharpe") waiver() else percent_format()
      ) +
      scale_x_discrete(position = "top") +
      labs(
        title = paste("Factor Momentum:", ttl, "— Parameter Grid"),
        subtitle = paste0(
          "JKP Daily | 1963–2023 | ",
          if (input$grid_lag == "0") "1" else "2",
          "-Day Lag"
        ),
        x = "Lookback",
        y = "Strategy",
        caption = "LS=Long-Short | LO=Long-Only | Median/33%/25% split"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank()
      )
  })

  output$grid_cumwealth <- renderPlot({
    gd <- grid_res()
    req(gd, length(gd$results) > 0)
    lb_ord <- names(LOOKBACK_LABELS)
    st_ord <- STRATEGY_TYPES
    dr <- input$grid_dates
    cum_all <- map_dfr(names(gd$results), function(k) {
      df <- gd$results[[k]]
      df |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          cum_wealth = cumprod(1 + period_ret),
          Strategy = factor(Strategy, levels = st_ord),
          Lookback = factor(Lookback, levels = lb_ord)
        ) |>
        dplyr::filter(date >= dr[1], date <= dr[2])
    })
    p <- ggplot(cum_all, aes(date, cum_wealth)) +
      geom_line(color = "#1565C0", linewidth = 0.8) +
      facet_grid(Strategy ~ Lookback, scales = "free_y") +
      scale_x_date(
        date_breaks = "10 years",
        date_labels = "%y",
        expand = expansion(mult = 0.01)
      ) +
      labs(
        title = "Factor Momentum: Full Parameter Grid",
        subtitle = paste0(
          "JKP Daily | ",
          format(dr[1], "%Y"),
          "–",
          format(dr[2], "%Y"),
          " | ",
          if (input$grid_lag == "0") "1" else "2",
          "-Day Lag"
        ),
        x = NULL,
        y = "Cumulative Wealth",
        caption = "Rows: Strategy | Cols: Lookback"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        strip.text = element_text(face = "bold", size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid.minor = element_blank()
      )
    apply_yscale(p, input$grid_cum_scale)
  })

  output$grid_table <- renderTable({
    gd <- grid_res()
    req(gd)
    gd$metrics |>
      arrange(desc(Sharpe)) |>
      transmute(
        Strategy,
        Lookback,
        N,
        `Ann. Return` = fmt_pct(Ann_Ret),
        `Ann. Vol` = fmt_pct(Ann_Vol),
        Sharpe = round(Sharpe, 2),
        `Cum. Return` = fmt_pct(Cum_Ret),
        `Max DD` = fmt_pct(Max_DD),
        Calmar = round(Calmar, 2)
      )
  })

  # ── 3. FOCUSED COMPARISON ─────────────────────────────────────────────────
  focus_res <- reactive({
    req(!is.null(daily_factors_wide), length(input$focus_series) > 0)
    lag_val <- as.integer(input$focus_lag)
    selected <- input$focus_series
    withProgress(message = "Building comparison…", value = 0.05, {
      strat_map <- list(
        ls_median = list(lbl = "LS Median 1M", st = "LS_Median"),
        lo_25 = list(lbl = "LO Top-25% 1M", st = "LO_25"),
        lo_median = list(lbl = "LO Median 1M", st = "LO_Median"),
        ls_25 = list(lbl = "LS 25% 1M", st = "LS_25")
      )
      ref_res <- calculate_factor_momentum(
        daily_factors_wide,
        daily_factor_cols,
        21,
        HOLDING_DAYS,
        lag_val,
        "LS_Median"
      )
      incProgress(0.25)
      slist <- list()
      if ("market" %in% selected && !is.null(mkt_daily_global)) {
        slist[["Market (Mkt-RF)"]] <- build_cum_wealth(
          compound_mkt(ref_res$date, mkt_daily_global),
          "Market (Mkt-RF)"
        )
      }
      if ("ind_mom" %in% selected && !is.null(ind_daily_global)) {
        ind <- calculate_factor_momentum(
          ind_daily_global,
          ind_cols_global,
          21,
          HOLDING_DAYS,
          lag_val,
          "LS_Median"
        )
        slist[["Industry Momentum"]] <- build_cum_wealth(
          ind,
          "Industry Momentum"
        )
      }
      incProgress(0.2)
      for (s in names(strat_map)) {
        if (s %in% selected) {
          res <- if (s == "ls_median") {
            ref_res
          } else {
            calculate_factor_momentum(
              daily_factors_wide,
              daily_factor_cols,
              21,
              HOLDING_DAYS,
              lag_val,
              strat_map[[s]]$st
            )
          }
          slist[[strat_map[[s]]$lbl]] <-
            build_cum_wealth(res, strat_map[[s]]$lbl)
        }
        incProgress(0.1)
      }
      # Return raw period_ret alongside cum_wealth for clean metric computation
      bind_rows(slist)
    })
  })

  focus_cum <- reactive({
    df <- focus_res()
    req(nrow(df) > 0)
    df |> add_start_row() |> flt(input$focus_dates)
  })

  # BUG FIX: compute metrics from period_ret, not from diff(log(cum_wealth))
  # which breaks on the synthetic start row and single-row groups
  focus_perf <- reactive({
    df <- focus_res()
    req(nrow(df) > 0)
    dr <- input$focus_dates
    df |>
      dplyr::filter(date >= dr[1], date <= dr[2]) |>
      dplyr::group_by(Series) |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        period_ret = cum_wealth / dplyr::lag(cum_wealth, default = 1) - 1
      ) |>
      dplyr::summarise(
        m = list(compute_metrics(period_ret[-1], ANN_FACTOR)),
        .groups = "drop"
      ) |>
      dplyr::filter(!purrr::map_lgl(m, is.null)) |>
      dplyr::mutate(
        Ann_Ret = purrr::map_dbl(m, "Ann_Ret"),
        Ann_Vol = purrr::map_dbl(m, "Ann_Vol"),
        Sharpe = purrr::map_dbl(m, "Sharpe"),
        Cum_Ret = purrr::map_dbl(m, "Cum_Ret"),
        Max_DD = purrr::map_dbl(m, "Max_DD"),
        Calmar = purrr::map_dbl(m, "Calmar")
      ) |>
      dplyr::select(-m)
  })

  output$foc_sharpe <- renderUI({
    p <- focus_perf()
    req(nrow(p) > 0)
    b <- p |> slice_max(Sharpe, n = 1)
    tagList(
      strong(paste0("Best Sharpe — ", b$Series)),
      br(),
      h4(round(b$Sharpe, 2))
    )
  })
  output$foc_ret <- renderUI({
    p <- focus_perf()
    req(nrow(p) > 0)
    b <- p |> slice_max(Ann_Ret, n = 1)
    tagList(
      strong(paste0("Best Return — ", b$Series)),
      br(),
      h4(fmt_pct(b$Ann_Ret))
    )
  })
  output$foc_dd <- renderUI({
    p <- focus_perf()
    req(nrow(p) > 0)
    b <- p |> slice_min(abs(Max_DD), n = 1)
    tagList(
      strong(paste0("Smallest DD — ", b$Series)),
      br(),
      h4(fmt_pct(b$Max_DD))
    )
  })
  output$foc_calmar <- renderUI({
    p <- focus_perf()
    req(nrow(p) > 0)
    b <- p |> slice_max(Calmar, n = 1)
    tagList(
      strong(paste0("Best Calmar — ", b$Series)),
      br(),
      h4(round(b$Calmar, 2))
    )
  })

  output$focus_plot <- renderPlot({
    df <- focus_cum()
    req(nrow(df) > 0)
    ll <- if (input$focus_lag == "0") "1-Day Lag" else "2-Day Lag"
    ser <- unique(df$Series)
    p <- ggplot(df, aes(date, cum_wealth, color = Series, linetype = Series)) +
      geom_line(linewidth = 1.05) +
      scale_color_manual(values = make_pal(ser)) +
      scale_linetype_manual(values = make_lty(ser)) +
      scale_x_date(
        date_breaks = "5 years",
        date_labels = "%Y",
        expand = expansion(mult = 0.01)
      ) +
      labs(
        title = "Factor Momentum Focused Comparison",
        subtitle = paste0(
          format(input$focus_dates[1], "%Y"),
          "–",
          format(input$focus_dates[2], "%Y"),
          " | ",
          ll,
          " | 1M Lookback | 21-Day Holding"
        ),
        x = NULL,
        y = "Cumulative Wealth (Start = $1)",
        color = NULL,
        linetype = NULL,
        caption = "Data: JKP Daily Factors & Ken French | Excess Returns"
      ) +
      theme_ql() +
      guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))
    apply_yscale(p, input$focus_scale)
  })

  output$focus_table <- renderTable({
    focus_perf() |>
      arrange(desc(Sharpe)) |>
      transmute(
        Series,
        `Ann. Return` = fmt_pct(Ann_Ret),
        `Ann. Vol` = fmt_pct(Ann_Vol),
        Sharpe = round(Sharpe, 2),
        `Cum. Return` = fmt_pct(Cum_Ret),
        `Max DD` = fmt_pct(Max_DD),
        Calmar = round(Calmar, 2)
      )
  })

  # ── 4. STRATEGY EXPLORER (daily engine — no more flawed monthly data) ──────
  explorer_raw <- eventReactive(
    input$run_explorer,
    {
      req(!is.null(daily_factors_wide))
      lag_val <- as.integer(input$exp_lag)
      lb <- as.integer(input$exp_lookback)
      st <- input$exp_strategy
      lb_lbl <- names(LOOKBACK_LABELS)[LOOKBACK_LABELS == as.character(lb)]
      withProgress(message = "Computing strategy…", value = 0.1, {
        fac <- calculate_factor_momentum(
          daily_factors_wide,
          daily_factor_cols,
          lb,
          HOLDING_DAYS,
          lag_val,
          st
        )
        incProgress(0.4)
        ind <- if (input$exp_vs_ind && !is.null(ind_daily_global)) {
          calculate_factor_momentum(
            ind_daily_global,
            ind_cols_global,
            lb,
            HOLDING_DAYS,
            lag_val,
            st
          )
        } else {
          NULL
        }
        incProgress(0.3)
        mkt <- if (input$exp_vs_market && !is.null(mkt_daily_global)) {
          compound_mkt(fac$date, mkt_daily_global)
        } else {
          NULL
        }
        incProgress(0.2)
        srows <- list(build_cum_wealth(fac, paste0(st, " (", lb_lbl, ")")))
        if (!is.null(ind)) {
          srows <- c(
            srows,
            list(build_cum_wealth(
              ind,
              paste0("Industry Momentum (", lb_lbl, ")")
            ))
          )
        }
        if (!is.null(mkt)) {
          srows <- c(srows, list(build_cum_wealth(mkt, "Market (Mkt-RF)")))
        }
        list(
          cum = bind_rows(srows) |> add_start_row(),
          fac_r = fac,
          lbl = lb_lbl
        )
      })
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  explorer_cum <- reactive({
    d <- explorer_raw()
    req(d)
    flt(d$cum, input$exp_dates)
  })

  exp_met <- reactive({
    d <- explorer_raw()
    req(d)
    compute_metrics(flt(d$fac_r, input$exp_dates)$period_ret, ANN_FACTOR)
  })

  output$exp_sharpe <- renderUI({
    m <- exp_met()
    req(m)
    tagList(strong("Sharpe"), br(), h4(round(m$Sharpe, 2)))
  })
  output$exp_ret <- renderUI({
    m <- exp_met()
    req(m)
    tagList(strong("Ann. Return"), br(), h4(fmt_pct(m$Ann_Ret)))
  })
  output$exp_vol <- renderUI({
    m <- exp_met()
    req(m)
    tagList(strong("Ann. Vol"), br(), h4(fmt_pct(m$Ann_Vol)))
  })
  output$exp_dd <- renderUI({
    m <- exp_met()
    req(m)
    tagList(strong("Max DD"), br(), h4(fmt_pct(m$Max_DD)))
  })

  output$explorer_plot <- renderPlot({
    df <- explorer_cum()
    req(nrow(df) > 0)
    d <- explorer_raw()
    ll <- if (input$exp_lag == "0") "1-Day Lag" else "2-Day Lag"
    ser <- unique(df$Series)
    p <- ggplot(df, aes(date, cum_wealth, color = Series)) +
      geom_line(linewidth = 1.05) +
      scale_color_manual(values = make_pal(ser)) +
      scale_x_date(
        date_breaks = "5 years",
        date_labels = "%Y",
        expand = expansion(mult = 0.01)
      ) +
      labs(
        title = paste0(
          "Strategy Explorer — ",
          input$exp_strategy,
          " | ",
          d$lbl,
          " Lookback"
        ),
        subtitle = paste0(
          format(input$exp_dates[1], "%Y"),
          "–",
          format(input$exp_dates[2], "%Y"),
          " | ",
          ll,
          " | ",
          HOLDING_DAYS,
          "-Day Holding"
        ),
        x = NULL,
        y = "Cumulative Wealth",
        color = NULL,
        caption = "Data: JKP Daily Factors & Ken French | Excess Returns"
      ) +
      theme_ql() +
      guides(color = guide_legend(nrow = 1))
    apply_yscale(p, input$exp_scale)
  })

  output$explorer_table <- renderTable({
    d <- explorer_raw()
    req(d)
    dr <- input$exp_dates
    ll <- if (input$exp_lag == "0") "1-Day" else "2-Day"
    rows <- list(fmt_tbl(
      compute_metrics(flt(d$fac_r, dr)$period_ret, ANN_FACTOR),
      paste0(input$exp_strategy, " (", d$lbl, " | ", ll, " Lag)")
    ))
    if (input$exp_vs_ind && !is.null(ind_daily_global)) {
      ir <- calculate_factor_momentum(
        ind_daily_global,
        ind_cols_global,
        as.integer(input$exp_lookback),
        HOLDING_DAYS,
        as.integer(input$exp_lag),
        input$exp_strategy
      )
      if (!is.null(ir)) {
        rows <- c(
          rows,
          list(fmt_tbl(
            compute_metrics(flt(ir, dr)$period_ret, ANN_FACTOR),
            paste0("Industry Momentum (", d$lbl, ")")
          ))
        )
      }
    }
    if (input$exp_vs_market && !is.null(mkt_daily_global)) {
      mr <- compound_mkt(d$fac_r$date, mkt_daily_global)
      rows <- c(
        rows,
        list(fmt_tbl(
          compute_metrics(flt(mr, dr)$period_ret, ANN_FACTOR),
          "Market (Mkt-RF)"
        ))
      )
    }
    bind_rows(rows)
  })

  output$explorer_dist <- renderPlot({
    d <- explorer_raw()
    req(d)
    dr <- input$exp_dates
    r_df <- flt(d$fac_r, dr)
    req(nrow(r_df) > 5)
    ggplot(r_df, aes(period_ret)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 50,
        fill = "#1565C0",
        alpha = 0.7,
        color = "white"
      ) +
      geom_density(color = "#E53935", linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
      scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(
        title = paste0(
          "Return Distribution — ",
          input$exp_strategy,
          " (",
          d$lbl,
          ")"
        ),
        subtitle = paste0(format(dr[1], "%Y"), "–", format(dr[2], "%Y")),
        x = "Period Return",
        y = "Density",
        caption = "Red line = kernel density estimate"
      ) +
      theme_ql(12) +
      theme(legend.position = "none")
  })

  # ── 5. CORRELATIONS ───────────────────────────────────────────────────────
  output$corr_plot <- renderPlot({
    dr <- input$corr_dates
    fac_data <- app_data$data |>
      dplyr::filter(date >= dr[1], date <= dr[2]) |>
      dplyr::select(dplyr::any_of(app_data$factor_cols)) |>
      na.omit()
    req(ncol(fac_data) >= 2)
    corrplot(
      cor(fac_data),
      method = input$corr_type,
      type = "upper",
      order = input$corr_order,
      tl.col = "black",
      tl.cex = input$corr_font,
      col = colorRampPalette(RColorBrewer::brewer.pal(11, "BrBG"))(200),
      diag = FALSE,
      title = paste0(
        "JKP Factor Correlation Matrix (",
        format(dr[1], "%Y"),
        "–",
        format(dr[2], "%Y"),
        ")"
      ),
      mar = c(0, 0, 2, 0)
    )
  })

  # ── 6. REGRESSION ─────────────────────────────────────────────────────────
  output$reg_factor_ui <- renderUI({
    selectInput(
      "reg_jkp_factor",
      "JKP Factor:",
      choices = sort(app_data$factor_cols),
      selected = if ("Book_to_Market_HML" %in% app_data$factor_cols) {
        "Book_to_Market_HML"
      } else {
        app_data$factor_cols[1]
      }
    )
  })

  reg_series <- reactive({
    req(input$reg_type, input$reg_ff, input$reg_dates)
    dr <- input$reg_dates
    if (input$reg_type == "jkp") {
      req(input$reg_jkp_factor, ff_reg_factors_loaded)
      app_data$data |>
        dplyr::select(date, ret = dplyr::all_of(input$reg_jkp_factor)) |>
        dplyr::inner_join(ff_reg_factors_loaded, by = "date") |>
        dplyr::mutate(dep = ret - rf) |>
        dplyr::filter(date >= dr[1], date <= dr[2]) |>
        na.omit()
    } else {
      req(!is.null(daily_factors_wide), ff_reg_factors_loaded)
      res <- calculate_factor_momentum(
        daily_factors_wide,
        daily_factor_cols,
        as.integer(input$reg_lb),
        HOLDING_DAYS,
        as.integer(input$reg_lag),
        input$reg_strat
      )
      req(!is.null(res))
      res |>
        dplyr::select(date, ret = period_ret) |>
        dplyr::inner_join(ff_reg_factors_loaded, by = "date") |>
        dplyr::mutate(dep = ret) |>
        dplyr::filter(date >= dr[1], date <= dr[2]) |>
        na.omit()
    }
  })

  reg_model <- reactive({
    rd <- reg_series()
    req(nrow(rd) > 10)
    lm(
      as.formula(paste("dep ~", paste(input$reg_ff, collapse = "+"))),
      data = rd
    )
  })

  output$reg_alpha <- renderUI({
    m <- reg_model()
    req(m)
    alp <- coef(m)[["(Intercept)"]]
    tagList(
      strong("Monthly Alpha"),
      br(),
      h4(fmt_pct(alp, 0.01)),
      p(
        style = "font-size:0.8em;color:gray;",
        if (alp > 0) "✅ Positive" else "⚠️ Negative"
      )
    )
  })
  output$reg_r2 <- renderUI({
    m <- reg_model()
    req(m)
    tagList(strong("R²"), br(), h4(round(summary(m)$r.squared, 3)))
  })
  output$reg_beta <- renderUI({
    m <- reg_model()
    req(m)
    cf <- coef(m)
    mkt <- if ("mkt_excess" %in% names(cf)) {
      round(cf[["mkt_excess"]], 2)
    } else {
      "—"
    }
    tagList(strong("Market Beta"), br(), h4(mkt))
  })

  output$reg_summary <- renderPrint({
    m <- reg_model()
    req(m)
    summary(m)
  })

  output$reg_rolling <- renderPlot({
    rd <- reg_series()
    req(nrow(rd) > input$reg_window)
    fml <- as.formula(paste("dep ~", paste(input$reg_ff, collapse = "+")))
    win <- as.integer(input$reg_window)
    alphas <- map_dfr(seq(win, nrow(rd)), function(i) {
      sub <- rd[seq(i - win + 1, i), ]
      m <- lm(fml, data = sub)
      tibble(
        date = rd$date[i],
        alpha = coef(m)[["(Intercept)"]],
        se = sqrt(vcov(m)[1, 1])
      )
    })
    lbl <- if (input$reg_type == "jkp") {
      input$reg_jkp_factor
    } else {
      paste0(
        input$reg_strat,
        " (",
        names(LOOKBACK_LABELS)[LOOKBACK_LABELS == input$reg_lb],
        ")"
      )
    }
    ggplot(alphas, aes(date, alpha)) +
      geom_ribbon(
        aes(ymin = alpha - 2 * se, ymax = alpha + 2 * se),
        fill = "#1565C0",
        alpha = 0.15
      ) +
      geom_line(color = "#1565C0", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
      scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
      labs(
        title = paste0("Rolling ", win, "-Month Alpha — ", lbl),
        subtitle = paste0(
          "Model: dep ~ ",
          paste(input$reg_ff, collapse = " + ")
        ),
        x = NULL,
        y = "Monthly Alpha",
        caption = "Shaded band = ±2 SE"
      ) +
      theme_ql(12) +
      theme(legend.position = "none")
  })
}

# ==============================================================================
shinyApp(ui = ui, server = server)
