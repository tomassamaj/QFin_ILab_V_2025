# --- 0. Load Required Libraries ---
library(shiny)
library(shinymanager) 
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(RSQLite)
library(ggplot2)
library(scales)
library(corrplot)
library(RColorBrewer)
library(zoo)
library(broom)
library(quantmod) # <-- Added for SP500 and Bond data

# --- 1. Load Pre-processed Data Bundle ---
load("quant_data.RData")

# --- 2. Extract Data Objects ---
factor_rename_map <- app_data$factor_rename_map
ff_reg_factors_loaded <- app_data$ff_reg_factors_loaded

# --- 3. Helper Functions ---

# Helper to fetch and process benchmarks
get_benchmark_data <- function(start_date, end_date) {
  # We fetch slightly earlier to ensure we have returns for the start date
  fetch_from <- as.Date(start_date) - 45 
  
  # 1. Fetch S&P 500
  tryCatch({
    sp500 <- getSymbols("^GSPC", src = "yahoo", from = fetch_from, auto.assign = FALSE)
    sp500_m <- monthlyReturn(Cl(sp500), type = "log") # Log returns
    
    # Convert to Dataframe
    df_sp <- data.frame(date = index(sp500_m), return = coredata(sp500_m)) %>%
      rename(momentum_return = monthly.returns) %>%
      mutate(strategy_type = "S&P 500") %>%
      mutate(date = floor_date(date, "month")) # Align to 1st of month to match your data
    
  }, error = function(e) { return(NULL) })
  
  # 2. Fetch Bond Proxy (IEF - 7-10 Year Treasury) 
  # Note: LUATRU is not on Yahoo. IEF is a standard proxy for intermediate treasuries.
  tryCatch({
    # IEF inception is approx 2002. If user selects 1990, this will just return available data.
    bonds <- getSymbols("IEF", src = "yahoo", from = fetch_from, auto.assign = FALSE)
    bonds_m <- monthlyReturn(Cl(bonds), type = "log")
    
    df_bnd <- data.frame(date = index(bonds_m), return = coredata(bonds_m)) %>%
      rename(momentum_return = monthly.returns) %>%
      mutate(strategy_type = "US Treasuries (IEF)") %>%
      mutate(date = floor_date(date, "month"))
    
  }, error = function(e) { return(NULL) })
  
  # Combine
  benchmarks <- bind_rows(df_sp, df_bnd) %>%
    filter(date >= start_date & date <= end_date)
  
  return(benchmarks)
}

# Dynamic Momentum Calculation (Same as before)
calculate_dynamic_momentum <- function(df, target_cols, strategy_name, lookback_months, skip_months, holding_period) {
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) return(NULL)
  
  mom_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), ~ log(1 + .), .names = "{.col}_logret")) %>%
    mutate(across(ends_with("_logret"), 
                  ~ zoo::rollsum(., k = lookback_months, fill = NA, align = "right"),
                  .names = "{sub('_logret', '_signal', .col)}")) %>%
    mutate(across(ends_with("_signal"), 
                  ~ dplyr::lag(., n = skip_months))) %>%
    rowwise() %>%
    mutate(median_signal = median(c_across(ends_with("_signal")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(ends_with("_signal"),
                  ~ case_when(!is.na(.) & . > median_signal ~ 1, !is.na(.) & . <= median_signal ~ -1, TRUE ~ 0),
                  .names = "{sub('_signal', '_pos', .col)}")) %>%
    rowwise() %>%
    mutate(n_long = sum(c_across(ends_with("_pos")) == 1, na.rm=TRUE),
           n_short = sum(c_across(ends_with("_pos")) == -1, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(across(ends_with("_pos"),
                  ~ case_when(. == 1 & n_long > 0 ~ 1 / n_long, . == -1 & n_short > 0 ~ 1 / n_short, TRUE ~ 0),
                  .names = "{sub('_pos', '_wgt', .col)}")) %>%
    rowwise() %>%
    mutate(raw_strat_ret = sum(c_across(ends_with("_pos")) * c_across(ends_with("_wgt")) * c_across(all_of(target_cols_exist)), na.rm = TRUE)) %>%
    ungroup()
  
  if (holding_period > 1) {
    mom_df <- mom_df %>% mutate(final_ret = zoo::rollmean(raw_strat_ret, k = holding_period, fill = NA, align = "right"))
  } else {
    mom_df <- mom_df %>% mutate(final_ret = raw_strat_ret)
  }
  
  output <- mom_df %>%
    select(date, momentum_return = final_ret) %>%
    mutate(strategy_type = strategy_name) %>%
    filter(!is.na(momentum_return))
  
  return(output)
}

# --- 5. Shiny UI ---
ui <- shinymanager::secure_app(
  shiny::tagList(
    shiny::fluidPage(
      shiny::titlePanel("Quant Lab: Dynamic Momentum vs Benchmarks"),
      
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h4("1. Configuration"),
          shiny::dateRangeInput("dateRange", "Select Analysis Period:",
                                start = "2005-01-01", # Started later to ensure ETF data usually exists
                                end = Sys.Date(),
                                format = "yyyy-mm-dd"),
          
          shiny::tags$hr(),
          shiny::h4("2. Momentum Construction"),
          shiny::sliderInput("lookback", "Formation Lookback (Months) [J]:", min = 1, max = 24, value = 12, step = 1),
          shiny::sliderInput("skip", "Skip/Lag Period (Months):", min = 0, max = 3, value = 1, step = 1),
          shiny::sliderInput("holding", "Holding Period (Months) [K]:", min = 1, max = 12, value = 1, step = 1),
          
          shiny::tags$hr(),
          shiny::h4("3. Regression Settings"),
          shiny::uiOutput("factor_selector_ui"),
          shiny::checkboxGroupInput("ffModelFactors", "FF Model Factors:",
                                    choices = c("mkt_excess", "smb", "hml", "rmw", "cma", "mom"),
                                    selected = c("mkt_excess", "smb", "hml"))
        ),
        
        shiny::mainPanel(
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("Performance",
                            shiny::br(),
                            shiny::plotOutput("rawMomentumPlot", height = "500px"),
                            shiny::br(),
                            shiny::h5("Performance Summary"),
                            shiny::tableOutput("perfStats"),
                            shiny::p(style="font-size:0.8em; color:grey;", 
                                     "Note: Benchmarks (SP500, Bonds) are fetched live from Yahoo Finance. Bonds proxy is IEF (7-10y Treasury).")
            ),
            shiny::tabPanel("Correlations", shiny::plotOutput("correlationHeatmap", height = "700px")),
            shiny::tabPanel("Regression", shiny::verbatimTextOutput("regressionSummary"))
          )
        )
      )
    ),
    shiny::tags$head(shiny::tags$style(shiny::HTML(".shinymanager-logout { color: #337ab7; font-weight: bold; }")))
  )
)

# --- 6. Shiny Server ---
server <- function(input, output, session) {
  
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = "credentials.sqlite", passphrase = "Ilab_member_12_1" 
    )
  )
  
  data_list <- reactive({ app_data })
  
  # 6.2 Calculate Strategy Data
  momentum_data_calc <- reactive({
    data_obj <- data_list()
    req(input$lookback, input$skip, input$holding)
    
    ind_mom <- calculate_dynamic_momentum(data_obj$data, data_obj$industry_cols, "Industry Momentum", input$lookback, input$skip, input$holding)
    fac_mom <- calculate_dynamic_momentum(data_obj$data, data_obj$factor_cols, "Factor Momentum", input$lookback, input$skip, input$holding)
    
    bind_rows(ind_mom, fac_mom)
  })
  
  # 6.3 Fetch Benchmarks (Reactive to Date Change)
  benchmark_data_calc <- reactive({
    req(input$dateRange)
    # Wrap in progress bar because fetching data takes a second
    withProgress(message = 'Fetching Benchmark Data...', value = 0.5, {
      get_benchmark_data(input$dateRange[1], input$dateRange[2])
    })
  })
  
  # 6.4 Merge and Normalize Data
  final_plot_data <- reactive({
    req(momentum_data_calc(), benchmark_data_calc(), input$dateRange)
    
    # 1. Strategies
    strat_df <- momentum_data_calc() %>%
      filter(date >= input$dateRange[1], date <= input$dateRange[2])
    
    # 2. Benchmarks
    bench_df <- benchmark_data_calc() 
    
    # 3. Combine
    combined <- bind_rows(strat_df, bench_df) %>%
      arrange(strategy_type, date) %>%
      group_by(strategy_type) %>%
      mutate(
        cum_ret = cumprod(1 + momentum_return),
        # Rebase each series to start at 1.0 on the first available date within the window
        cum_ret_norm = cum_ret / first(cum_ret)
      ) %>%
      ungroup()
    
    return(combined)
  })
  
  # 6.5 Render Plot with Distinct Colors
  output$rawMomentumPlot <- renderPlot({
    plot_data <- final_plot_data()
    req(nrow(plot_data) > 0)
    
    # Custom Colors: 
    # Strategies = Blue Tones. SP500 = Red. Bonds = Gold/Orange.
    custom_colors <- c(
      "Industry Momentum" = "#2c7bb6",    # Medium Blue
      "Factor Momentum"   = "#08306b",    # Dark Navy
      "S&P 500"           = "#d73027",    # Dark Red
      "US Treasuries (IEF)" = "#f46d43"   # Orange/Gold
    )
    
    ggplot(plot_data, aes(x = date, y = cum_ret_norm, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
      scale_color_manual(values = custom_colors) +
      labs(
        title = paste0("Momentum (J=", input$lookback, ", K=", input$holding, ") vs Benchmarks"),
        subtitle = "Cumulative Wealth (Log Scale) | Rebased to 1.0",
        x = NULL, y = "Cumulative Wealth ($)", color = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        plot.title = element_text(face="bold")
      )
  })
  
  # 6.6 Render Performance Stats
  output$perfStats <- renderTable({
    df <- final_plot_data()
    req(nrow(df) > 0)
    
    df %>%
      group_by(Strategy = strategy_type) %>%
      summarise(
        `Total Return` = scales::percent(last(cum_ret_norm) - 1, accuracy=0.1),
        `Ann. Return` = scales::percent((last(cum_ret_norm)^(12/n()) - 1), accuracy=0.1),
        `Ann. Vol` = scales::percent(sd(momentum_return) * sqrt(12), accuracy=0.1),
        `Sharpe` = round((mean(momentum_return) * 12) / (sd(momentum_return) * sqrt(12)), 2)
      )
  }, digits = 2)
  
  # 6.7 Correlation Heatmap (Unchanged)
  output$correlationHeatmap <- renderPlot({
    data_obj <- data_list()
    factor_data_for_corr <- data_obj$data %>% select(any_of(data_obj$factor_cols)) %>% na.omit()
    cor_matrix <- cor(factor_data_for_corr)
    corrplot(cor_matrix, method = "color", type = "upper", order = "original",
             tl.col = "black", tl.cex = 0.7, col = colorRampPalette(brewer.pal(n=11, name="BrBG"))(200),
             diag = FALSE, title = "JKP Factor Correlation")
  })
  
  # 6.8 Regression Logic (Unchanged)
  output$factor_selector_ui <- renderUI({
    data_obj <- data_list()
    shiny::selectInput("jkpFactor", "Target Factor:", choices = sort(data_obj$factor_cols), selected = "Book_to_Market_HML")
  })
  
  output$regressionSummary <- renderPrint({
    data_obj <- data_list()
    req(input$jkpFactor, input$ffModelFactors, ff_reg_factors_loaded)
    
    jkp_data <- data_obj$data %>% select(date, all_of(input$jkpFactor))
    reg_data <- jkp_data %>%
      inner_join(ff_reg_factors_loaded, by = "date") %>%
      mutate(Factor_Excess = .data[[input$jkpFactor]] - rf) %>%
      na.omit() %>%
      filter(date >= input$dateRange[1], date <= input$dateRange[2])
    
    formula_str <- paste("Factor_Excess ~", paste(input$ffModelFactors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = reg_data)
    
    print(summary(model))
  })
}

# --- 7. Run App ---
shinyApp(ui = ui, server = server)