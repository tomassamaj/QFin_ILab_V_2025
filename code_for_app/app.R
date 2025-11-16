# --- 0. Load Required Libraries ---
library(shiny)
library(shinymanager) # For password protection
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(RSQLite) # <-- Added RSQLite
library(ggplot2)
library(scales)
library(corrplot)
library(RColorBrewer)
library(zoo)
library(broom)

# --- 1. Load Pre-processed Data Bundle ---
# This file (quant_data.RData) must be in the same folder as app.R
load("quant_data.RData")
# This loads the 'app_data' list object we created in the prep script

# --- 2. Extract Data Objects into Global Environment ---
factor_rename_map <- app_data$factor_rename_map
ff_reg_factors_loaded <- app_data$ff_reg_factors_loaded

# --- 3. Define Credentials for Password Protection ---
# === THIS SECTION IS NO LONGER NEEDED ===
# We will read from the 'credentials.sqlite' file


# --- 4. Helper Functions (Unchanged) ---
calculate_momentum <- function(df, target_cols, strategy_name) {
  if (length(target_cols) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 columns provided."))
    return(NULL) # Return NULL if not enough columns
  }
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 valid columns found in dataframe."))
    return(NULL) # Return NULL if not enough valid columns
  }
  
  momentum_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), lag, .names = "{.col}_lag1")) %>%
    filter(row_number() > 1) %>%
    rowwise() %>%
    mutate(median_lag1_ret = median(c_across(ends_with("_lag1")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(ends_with("_lag1"),
                  ~ case_when(
                    !is.na(.) & . > median_lag1_ret ~ 1,
                    !is.na(.) & . <= median_lag1_ret ~ -1,
                    TRUE ~ 0
                  ),
                  .names = "{sub('_lag1', '_pos', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      n_long = sum(c_across(ends_with("_pos")) == 1),
      n_short = sum(c_across(ends_with("_pos")) == -1)
    ) %>%
    ungroup() %>%
    mutate(across(ends_with("_pos"),
                  ~ case_when(
                    . == 1 & n_long > 0 ~ 1 / n_long,
                    . == -1 & n_short > 0 ~ 1 / n_short,
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      momentum_return = sum(
        c_across(ends_with("_pos")) *
          c_across(ends_with("_wgt")) *
          c_across(all_of(target_cols_exist)),
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    mutate(strategy_type = strategy_name)
  
  return(momentum_df)
}

scale_volatility <- function(df, target_ann_vol = 0.10, lookback_months = 36, min_obs = 12) {
  if (!"momentum_return" %in% colnames(df)) {
    stop("Input dataframe must contain 'momentum_return' column.")
  }
  if (!"date" %in% colnames(df)) {
    stop("Input dataframe must contain 'date' column.")
  }
  df <- df %>% arrange(date)
  
  rolling_sd <- rollapply(df$momentum_return,
                          width = lookback_months,
                          FUN = sd,
                          na.rm = TRUE,
                          fill = NA,
                          align = "right",
                          partial = min_obs)
  
  annualized_rolling_vol <- rolling_sd * sqrt(12)
  leverage_factor <- lag(pmin(5, target_ann_vol / annualized_rolling_vol, na.rm = TRUE), 1)
  leverage_factor[is.infinite(leverage_factor) | is.na(leverage_factor)] <- 1
  
  df <- df %>%
    mutate(
      rolling_ann_vol = annualized_rolling_vol,
      leverage = leverage_factor,
      scaled_momentum_return = momentum_return * leverage
    ) %>%
    filter(!is.na(scaled_momentum_return))
  
  return(df)
}


# --- 5. Shiny UI (User Interface) ---
ui <- shinymanager::secure_app(
  shiny::tagList( # Add this tagList wrapper
    shiny::fluidPage(
      shiny::titlePanel("Quantitative Finance Lab: Factor Momentum Replication"),
      
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h4("App Secured"),
          shiny::p("Data is pre-loaded. No uploads required."),
          
          shiny::tags$hr(),
          shiny::h4("2. Adjust Parameters"),
          shiny::sliderInput("volTarget", "Target Annual Volatility:",
                             min = 0.05, max = 0.20, value = 0.10, step = 0.01, post = "%"),
          shiny::sliderInput("lookback", "Volatility Lookback (Months):",
                             min = 12, max = 60, value = 36, step = 1),
          
          shiny::tags$hr(),
          shiny::h4("3. Regression Analysis"),
          shiny::uiOutput("factor_selector_ui"), # Dynamic selector
          shiny::checkboxGroupInput("ffModelFactors", "Select Factors for Model:",
                                    choices = c("mkt_excess", "smb", "hml", "rmw", "cma", "mom"),
                                    selected = c("mkt_excess", "smb", "hml"))
        ),
        
        shiny::mainPanel(
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("Momentum Performance",
                            shiny::plotOutput("rawMomentumPlot", height = "400px"),
                            shiny::plotOutput("scaledMomentumPlot", height = "400px")
            ),
            shiny::tabPanel("Factor Correlation",
                            shiny::plotOutput("correlationHeatmap", height = "700px")
            ),
            shiny::tabPanel("Factor Regression",
                            shiny::verbatimTextOutput("regressionSummary")
            )
          )
        )
      )
    ),
    # Add a CSS tag to make the logout button visible
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      ".shinymanager-logout { color: #337ab7; font-weight: bold; }"
    )))
  )
)


# --- 6. Shiny Server (Backend Logic) ---
server <- function(input, output, session) {
  
  # --- 6.1. Add shinymanager Server-side Logic ---
  # *** Pointing to the SQLite database ***
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = "credentials.sqlite",
      # *** UPDATED to your new passphrase ***
      passphrase = "Ilab_member_12_1" 
    )
  )
  
  # --- 6.2. Reactive: Load Data from Pre-loaded List ---
  data_list <- reactive({
    app_data
  })
  
  # --- 6.3. Reactive: Calculate Raw Momentum (Unchanged) ---
  raw_momentum_data <- reactive({
    data_obj <- data_list() 
    
    industry_momentum <- calculate_momentum(data_obj$data, data_obj$industry_cols, "Industry Momentum")
    factor_momentum <- calculate_momentum(data_obj$data, data_obj$factor_cols, "Factor Momentum")
    
    req(industry_momentum, factor_momentum)
    
    common_start_date <- max(min(industry_momentum$date), min(factor_momentum$date))
    
    bind_rows(industry_momentum, factor_momentum) %>%
      filter(date >= common_start_date) %>%
      arrange(strategy_type, date) %>%
      group_by(strategy_type) %>%
      mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
      ungroup()
  })
  
  # --- 6.4. Reactive: Calculate Scaled Momentum (Unchanged) ---
  scaled_momentum_data <- reactive({
    raw_data <- raw_momentum_data()
    req(raw_data)
    
    target_vol <- input$volTarget
    lookback <- input$lookback
    
    raw_data %>%
      group_by(strategy_type) %>%
      do(scale_volatility(., target_ann_vol = target_vol, lookback_months = lookback)) %>%
      ungroup() %>%
      filter(!is.null(scaled_momentum_return)) %>%
      arrange(strategy_type, date) %>%
      group_by(strategy_type) %>%
      mutate(cumulative_return_scaled_log = cumprod(1 + scaled_momentum_return)) %>%
      ungroup()
  })
  
  # --- 6.5. Render Raw Momentum Plot (Unchanged) ---
  output$rawMomentumPlot <- renderPlot({
    plot_data <- raw_momentum_data()
    req(nrow(plot_data) > 0)
    
    ggplot(plot_data, aes(x = date, y = cumulative_return_log, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(breaks = scales::log_breaks(n = 10), labels = scales::label_number(accuracy = 0.1)) +
      scale_color_manual(values = c("Industry Momentum" = "black", "Factor Momentum" = "blue")) +
      labs(
        title = "Cumulative Performance of Factor vs. Industry Momentum",
        subtitle = "Value of $1 invested (Log Scale)",
        x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # --- 6.6. Render Scaled Momentum Plot (Unchanged) ---
  output$scaledMomentumPlot <- renderPlot({
    plot_data_scaled <- scaled_momentum_data()
    req(nrow(plot_data_scaled) > 0)
    
    target_vol_label <- scales::percent(input$volTarget, accuracy = 1)
    
    ggplot(plot_data_scaled, aes(x = date, y = cumulative_return_scaled_log, color = strategy_type)) +
      geom_line(linewidth = 1) +
      scale_y_log10(breaks = scales::log_breaks(n = 10), labels = scales::label_number(accuracy = 0.1)) +
      scale_color_manual(values = c("Industry Momentum" = "black", "Factor Momentum" = "blue")) +
      labs(
        title = paste0("Cumulative Performance (Scaled to ", target_vol_label, " Ann. Volatility)"),
        subtitle = paste0("Value of $1 invested (Log Scale) | Lookback: ", input$lookback, " months"),
        x = "Year", y = "Cumulative Performance ($)", color = "Strategy"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  # --- 6.7. Render Correlation Heatmap (Unchanged) ---
  output$correlationHeatmap <- renderPlot({
    data_obj <- data_list()
    req(data_obj$factor_cols)
    
    factor_data_for_corr <- data_obj$data %>%
      select(any_of(data_obj$factor_cols)) %>%
      na.omit()
    
    req(ncol(factor_data_for_corr) >= 2)
    
    cor_matrix <- cor(factor_data_for_corr) # <-- FIX 2: Removed '.x ='
    col_palette <- colorRampPalette(brewer.pal(n = 11, name = "BrBG"))(200)
    
    corrplot(cor_matrix,
             method = "color",
             type = "upper",
             order = "original",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             tl.cex = 0.7,
             number.cex = 0.5,
             col = col_palette,
             diag = FALSE,
             cl.cex = 0.8,
             mar = c(0,0,1,0),
             title = "Correlation Heatmap of JKP Factors"
    )
  })
  
  # --- 6.8. Render Dynamic UI for Factor Selector (Unchanged) ---
  output$factor_selector_ui <- renderUI({
    data_obj <- data_list()
    req(data_obj$factor_cols)
    
    shiny::selectInput("jkpFactor", "Select JKP Factor (Dependent Var):",
                       choices = sort(data_obj$factor_cols),
                       selected = "Book_to_Market_HML")
  })
  
  # --- 6.9. Render Regression Summary (Unchanged) ---
  output$regressionSummary <- renderPrint({
    data_obj <- data_list()
    req(input$jkpFactor, input$ffModelFactors, ff_reg_factors_loaded)
    
    jkp_data <- data_obj$data %>%
      select(date, all_of(input$jkpFactor))
    
    reg_data <- jkp_data %>%
      inner_join(ff_reg_factors_loaded, by = "date") %>%
      mutate(Factor_Excess = .data[[input$jkpFactor]] - rf) %>%
      na.omit()
    
    req(nrow(reg_data) > 0, length(input$ffModelFactors) > 0)
    
    formula_str <- paste("Factor_Excess ~", paste(input$ffModelFactors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = reg_data)
    
    cat("--- Regression Summary ---\n\n")
    cat("Dependent Variable (JKP Factor):", input$jkpFactor, "(Excess Return)\n")
    cat("Independent Variables:", paste(input$ffModelFactors, collapse = ", "), "\n")
    cat("Sample Period:", as.character(min(reg_data$date)), "to", as.character(max(reg_data$date)), "\n")
    cat("----------------------------------\n\n")
    
    print(summary(model))
    
    cat("\n--- Model Fit (Glance) ---\n")
    print(glance(model))
  })
  
}

# --- 7. Run the App ---
shinyApp(ui = ui, server = server)