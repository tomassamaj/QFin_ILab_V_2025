# Set max upload size to 100MB
options(shiny.maxRequestSize = 100*1024^2)

# --- 0. Load Required Libraries ---
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(frenchdata)
library(ggplot2)
library(scales)
library(corrplot)
library(RColorBrewer)
library(zoo)      # For rollapply
library(broom)    # For tidying model output

# --- 1. Define Helper Functions (from your script) ---

# Function to calculate 1-month momentum returns
calculate_momentum <- function(df, target_cols, strategy_name) {
  if (length(target_cols) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 columns provided."))
    return(NULL) # Return NULL if not enough columns
  }
  # Check if target columns exist
  target_cols_exist <- intersect(target_cols, colnames(df))
  if (length(target_cols_exist) < 2) {
    warning(paste("Skipping momentum for", strategy_name, "- less than 2 valid columns found in dataframe."))
    return(NULL) # Return NULL if not enough valid columns
  }
  
  momentum_df <- df %>%
    select(date, all_of(target_cols_exist)) %>%
    arrange(date) %>%
    mutate(across(all_of(target_cols_exist), lag, .names = "{.col}_lag1")) %>%
    filter(row_number() > 1) %>% # Remove first row with NAs
    rowwise() %>%
    mutate(median_lag1_ret = median(c_across(ends_with("_lag1")), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(across(ends_with("_lag1"),
                  ~ case_when(
                    !is.na(.) & . > median_lag1_ret ~ 1,
                    !is.na(.) & . <= median_lag1_ret ~ -1,
                    TRUE ~ 0 # Handle NAs in lagged returns
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
                    . == -1 & n_short > 0 ~ 1 / n_short, # Weight magnitude
                    TRUE ~ 0
                  ),
                  .names = "{sub('_pos', '_wgt', .col)}"
    )) %>%
    rowwise() %>%
    mutate(
      momentum_return = sum(
        c_across(ends_with("_pos")) * # Position (+1 or -1)
          c_across(ends_with("_wgt")) * # Weight magnitude (1/N)
          c_across(all_of(target_cols_exist)), # Current month returns
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    select(date, momentum_return) %>%
    mutate(strategy_type = strategy_name)
  
  return(momentum_df)
}

# Function for Volatility Scaling
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
                          partial = min_obs) # Allow calculation with fewer initial obs
  
  annualized_rolling_vol <- rolling_sd * sqrt(12)
  leverage_factor <- lag(pmin(5, target_ann_vol / annualized_rolling_vol, na.rm = TRUE), 1)
  leverage_factor[is.infinite(leverage_factor) | is.na(leverage_factor)] <- 1 # Default to 1x leverage if vol is 0 or NA
  
  df <- df %>%
    mutate(
      rolling_ann_vol = annualized_rolling_vol,
      leverage = leverage_factor,
      scaled_momentum_return = momentum_return * leverage
    ) %>%
    filter(!is.na(scaled_momentum_return)) # Remove initial rows where scaling isn't possible
  
  return(df)
}

# --- 2. Factor Renaming Map (from your script) ---
factor_rename_map <- c(
  "Size_SMB" = "market_equity", "Book_to_Market_HML" = "be_me",
  "Operating_Profitability_RMW" = "ope_be", "Asset_Growth_CMA" = "at_gr1",
  "Long_Term_Reversals_LTREV" = "ret_60_12", "Residual_Variance_RVAR" = "ivol_ff3_21d",
  "Quality_Minus_Junk_QMJ" = "qmj", "Low_Beta_BAB" = "betabab_1260d",
  "Amihud_Illiquidity" = "ami_126d", "Firm_Age" = "age",
  "Nominal_Price" = "prc", "High_Volume_Premium" = "dolvol_126d",
  "Gross_Profitability" = "gp_at", "Return_on_Equity" = "ni_be",
  "Return_on_Assets" = "niq_at", "Profit_Margin" = "ebit_sale",
  "Change_in_Asset_Turnover" = "at_turnover",
  "Accruals_Factor" = "oaccruals_at", "Net_Operating_Assets" = "noa_at",
  "Net_Working_Capital_Changes" = "cowc_gr1a", "Cash_Flow_to_Price" = "ocf_me",
  "Earnings_to_Price" = "ni_me", "Enterprise_Multiple" = "ebitda_mev",
  "Sales_to_Price" = "sale_me",
  "Growth_in_Inventory" = "inv_gr1", "Sales_Growth" = "sale_gr1",
  "Growth_in_Sales_Inventory" = "dsale_dinv", "Abnormal_Investment" = "capex_abn",
  "CAPX_Growth_Rate" = "capx_gr1",
  "Debt_Issuance_Factor" = "dbnetis_at", "Leverage_Factor" = "at_be",
  "One_Year_Share_Issuance" = "chcsho_12m", "Total_External_Financing" = "netis_at",
  "Ohlson_O_Score" = "o_score", "Altman_Z_Score" = "z_score",
  "Piotroski_F_Score" = "f_score"
)


# --- 3. Pre-load Fama-French Regression Factors ---
# This runs once when the app starts
ff_reg_factors_loaded <- tryCatch({
  start_date_ff <- ymd("1963-07-01")
  end_date_ff <- ymd("2024-12-31")
  
  factors_ff5_monthly_raw <- download_french_data("Fama/French 5 Factors (2x3)")
  factors_ff5_monthly <- factors_ff5_monthly_raw$subsets$data[[1]] |>
    mutate(
      date = floor_date(ymd(str_c(date, "01")), "month"),
      across(c(RF, `Mkt-RF`, SMB, HML, RMW, CMA), ~as.numeric(.) / 100),
      .keep = "none"
    ) |>
    rename_with(str_to_lower) |>
    rename(mkt_excess = `mkt-rf`) |>
    filter(date >= start_date_ff & date <= end_date_ff) |>
    mutate(date = ceiling_date(date, "month") - days(1))
  
  factors_mom_monthly_raw <- download_french_data("Momentum Factor (Mom)")
  factors_mom_monthly <- factors_mom_monthly_raw$subsets$data[[1]] |>
    mutate(
      date = floor_date(ymd(str_c(date, "01")), "month"),
      across(c(Mom), ~as.numeric(.) / 100),
      .keep = "none"
    ) |>
    rename_with(str_to_lower) |>
    filter(date >= start_date_ff & date <= end_date_ff) |>
    mutate(date = ceiling_date(date, "month") - days(1))
  
  inner_join(factors_ff5_monthly, factors_mom_monthly, by = "date")
}, error = function(e) {
  message("Error downloading FF regression factors: ", e$message)
  NULL
})


# --- 4. Shiny UI (User Interface) ---
ui <- fluidPage(
  titlePanel("Quantitative Finance Lab: Factor Momentum Replication"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Load Data"),
      fileInput("fileFactors", "Upload Factor CSV",
                accept = c("text/csv", ".csv")),
      fileInput("fileThemes", "Upload Themes CSV",
                accept = c("text/csv", ".csv")),
      fileInput("fileIndustries", "Upload Industry Names CSV",
                accept = c("text/csv", ".csv")),
      
      tags$hr(),
      h4("2. Adjust Parameters"),
      sliderInput("volTarget", "Target Annual Volatility:",
                  min = 0.05, max = 0.20, value = 0.10, step = 0.01, post = "%"),
      sliderInput("lookback", "Volatility Lookback (Months):",
                  min = 12, max = 60, value = 36, step = 1),
      
      tags$hr(),
      h4("3. Regression Analysis"),
      uiOutput("factor_selector_ui"), # Dynamic selector
      checkboxGroupInput("ffModelFactors", "Select Factors for Model:",
                         choices = c("mkt_excess", "smb", "hml", "rmw", "cma", "mom"),
                         selected = c("mkt_excess", "smb", "hml"))
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Momentum Performance",
                 plotOutput("rawMomentumPlot", height = "400px"),
                 plotOutput("scaledMomentumPlot", height = "400px")
        ),
        tabPanel("Factor Correlation",
                 plotOutput("correlationHeatmap", height = "700px")
        ),
        tabPanel("Factor Regression",
                 verbatimTextOutput("regressionSummary")
        )
      )
    )
  )
)


# --- 5. Shiny Server (Backend Logic) ---
server <- function(input, output, session) {
  
  # --- 5.1. Reactive: Load, Merge, and Rename Data ---
  data_list <- reactive({
    req(input$fileFactors, input$fileThemes, input$fileIndustries)
    
    # Define start and end dates (from your script)
    start_date <- ymd("1963-07-01")
    end_date <- ymd("2024-12-31")
    
    # --- Load user-uploaded files ---
    all_factors_wide <- read.csv(input$fileFactors$datapath) %>%
      select(date, name, ret) %>%
      mutate(date = ymd(date)) %>%
      filter(date >= start_date & date <= end_date) %>%
      pivot_wider(names_from = name, values_from = ret)
    
    all_themes_wide <- read.csv(input$fileThemes$datapath) %>%
      select(date, name, ret) %>%
      mutate(date = ymd(date)) %>%
      filter(date >= start_date & date <= end_date) %>%
      pivot_wider(names_from = name, values_from = ret)
    
    ind_ports <- read.csv(input$fileIndustries$datapath)
    # This logic is from your script to get the industry names
    industry_names <- ind_ports[7:23, 1] 
    industry_cols <- str_to_lower(industry_names)
    
    # --- Load and process downloaded FF Industry Data ---
    ff_17_industry_monthly_raw <- download_french_data("17 Industry Portfolios")
    
    ff_17_industry_monthly <- ff_17_industry_monthly_raw$subsets$data[[1]] |>
      mutate(
        date = floor_date(ymd(str_c(date, "01")), "month"),
        across(all_of(industry_names), ~ as.numeric(.) / 100), # Use names from CSV
        .keep = "none" # Keep only specified columns
      ) |>
      rename_with(str_to_lower) |>
      filter(date >= start_date & date <= end_date) |>
      mutate(date = ceiling_date(date, "month") - days(1))
    
    
    # --- Merge all data sources ---
    merged_factors <- inner_join(all_themes_wide, all_factors_wide, by = "date") %>%
      mutate(date = ceiling_date(date, "month") - days(1)) # Align date format
    
    final_merged <- inner_join(ff_17_industry_monthly, merged_factors, by = "date")
    
    final_merged_renamed <- final_merged %>%
      rename(any_of(factor_rename_map))
    
    # Get the successfully renamed factor names
    renamed_factor_cols <- intersect(names(factor_rename_map), colnames(final_merged_renamed))
    
    # Return all necessary objects
    list(
      data = final_merged_renamed,
      industry_cols = industry_cols,
      factor_cols = renamed_factor_cols
    )
  })
  
  # --- 5.2. Reactive: Calculate Raw Momentum ---
  raw_momentum_data <- reactive({
    data_obj <- data_list() # Get data from the core reactive
    
    industry_momentum <- calculate_momentum(data_obj$data, data_obj$industry_cols, "Industry Momentum")
    factor_momentum <- calculate_momentum(data_obj$data, data_obj$factor_cols, "Factor Momentum")
    
    req(industry_momentum, factor_momentum) # Ensure both calculations succeeded
    
    common_start_date <- max(min(industry_momentum$date), min(factor_momentum$date))
    
    bind_rows(industry_momentum, factor_momentum) %>%
      filter(date >= common_start_date) %>%
      arrange(strategy_type, date) %>%
      group_by(strategy_type) %>%
      mutate(cumulative_return_log = cumprod(1 + momentum_return)) %>%
      ungroup()
  })
  
  # --- 5.3. Reactive: Calculate Scaled Momentum ---
  scaled_momentum_data <- reactive({
    raw_data <- raw_momentum_data() # Get raw series
    req(raw_data)
    
    # Get inputs for scaling
    target_vol <- input$volTarget
    lookback <- input$lookback
    
    # Scale both series
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
  
  # --- 5.4. Render Raw Momentum Plot ---
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
  
  # --- 5.5. Render Scaled Momentum Plot ---
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
  
  # --- 5.6. Render Correlation Heatmap ---
  output$correlationHeatmap <- renderPlot({
    data_obj <- data_list()
    req(data_obj$factor_cols)
    
    factor_data_for_corr <- data_obj$data %>%
      select(any_of(data_obj$factor_cols)) %>%
      na.omit()
    
    req(ncol(factor_data_for_corr) >= 2)
    
    cor_matrix <- cor(factor_data_for_corr)
    col_palette <- colorRampPalette(brewer.pal(n = 11, name = "BrBG"))(200)
    
    corrplot(cor_matrix,
             method = "color",
             type = "upper",
             order = "original",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             tl.cex = 0.7,      # Adjusted text size
             number.cex = 0.5,  # Adjusted number size
             col = col_palette,
             diag = FALSE,
             cl.cex = 0.8,
             mar = c(0,0,1,0),
             title = "Correlation Heatmap of JKP Factors"
    )
  })
  
  # --- 5.7. Render Dynamic UI for Factor Selector ---
  output$factor_selector_ui <- renderUI({
    data_obj <- data_list()
    req(data_obj$factor_cols)
    
    selectInput("jkpFactor", "Select JKP Factor (Dependent Var):",
                choices = sort(data_obj$factor_cols),
                selected = "Book_to_Market_HML")
  })
  
  # --- 5.8. Render Regression Summary ---
  output$regressionSummary <- renderPrint({
    data_obj <- data_list()
    req(input$jkpFactor, input$ffModelFactors, ff_reg_factors_loaded)
    
    # Get the selected JKP factor data
    jkp_data <- data_obj$data %>%
      select(date, all_of(input$jkpFactor))
    
    # Prepare data for regression
    reg_data <- jkp_data %>%
      inner_join(ff_reg_factors_loaded, by = "date") %>%
      # Calculate excess return for the JKP factor
      mutate(Factor_Excess = .data[[input$jkpFactor]] - rf) %>%
      na.omit()
    
    req(nrow(reg_data) > 0, length(input$ffModelFactors) > 0)
    
    # Build the formula dynamically
    formula_str <- paste("Factor_Excess ~", paste(input$ffModelFactors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = reg_data)
    
    # Print the summary
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

# --- 6. Run the App ---
shinyApp(ui = ui, server = server)