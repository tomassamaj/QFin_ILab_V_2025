# ==============================================================================
# JKP PROJECT: SMART DATA DICTIONARY (V2)
# Purpose: document all data files with precise roles and variable lists
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, readxl, openxlsx, RSQLite, fs, tools)

# --- CONFIGURATION ---
DATA_DIR <- "01_Data"
OUTPUT_FILE <- "06_Docs/Project_Data_Dictionary.xlsx"

# --- SMART CLASSIFIER FUNCTION ---
classify_file <- function(fname, fpath) {
  fname_lower <- tolower(fname)
  
  # 1. THE "GOLDEN" FILES (What we actually use)
  if (grepl("usa_valid_factor_returns_daily", fname_lower)) return(c("Strategy - PRODUCTION", "The FINAL, clean, validated daily factor returns used for the momentum strategy."))
  if (grepl("usa_valid_factor_returns", fname_lower))       return(c("Strategy - PRODUCTION", "The FINAL, clean, validated monthly factor returns."))
  
  # 2. THE RAW ENGINES
  if (fname_lower == "usa.parquet")           return(c("Engine - Stock Selection", "The 'Universe File'. Contains characteristics (signals) for every stock to construct portfolios."))
  if (fname_lower == "usa_returns.parquet")   return(c("Engine - Execution", "The 'Price File'. Contains Prices, Dividends, and Returns for calculating trades and friction."))
  if (grepl("pfs_daily", fname_lower))        return(c("Engine - Raw Factors", "JKP's Raw Daily Portfolios (Buckets 1-3). We process this to create Long-Short factors."))
  if (grepl("pfs.parquet", fname_lower))      return(c("Engine - Raw Factors", "JKP's Raw Monthly Portfolios. We process this to create Long-Short factors."))
  
  # 3. FUNDAMENTALS
  if (grepl("annual", fname_lower))           return(c("Deep Dive - Fundamentals", "Yearly Balance Sheet & Income Statement data (Compustat)."))
  if (grepl("quarterly", fname_lower))        return(c("Deep Dive - Fundamentals", "Quarterly Earnings data. Useful for earnings surprise factors."))
  
  # 4. BENCHMARKS & VALIDATION
  if (grepl("aggregated", fname_lower))       return(c("Validation - Benchmark", "JKP's Official pre-calculated factors. We compare our manual construction against this."))
  if (grepl("\\[usa\\]", fname_lower))        return(c("Validation - Benchmark", "Original CSV benchmarks downloaded from JKP website for audit."))
  
  # 5. RISK & MARKET
  if (grepl("ap_factors", fname_lower))       return(c("Risk - Benchmarks", "Asset Pricing Factors (Mkt-RF, SMB, HML, RF). Essential for CAPM/Alpha calculation."))
  if (grepl("market_returns", fname_lower))   return(c("Risk - Market", "Overall Market Return (VW/EW). Used for Beta calculation."))
  
  # 6. METADATA & CLASSIFICATION
  if (grepl("industry_gics", fname_lower))    return(c("Metadata - Sectors", "GICS Sector codes for every stock. Used for Sector Neutrality checks."))
  if (grepl("nyse_cutoffs", fname_lower))     return(c("Metadata - Size", "Market Cap breakpoints (Small vs Large)."))
  if (grepl("details", fname_lower))          return(c("Metadata - Dictionary", "The Rosetta Stone. Maps code names (be_me) to human names."))
  if (grepl("clusters", fname_lower))         return(c("Metadata - Themes", "Mapping of individual factors into broad themes (Value, Momentum, etc.)."))
  
  # 7. EXTERNAL / LEGACY
  if (grepl("sqlite", fname_lower))           return(c("External - TidyFinance", "Local SQL database (likely from Tidy Finance book). Reference only."))
  if (grepl("17_industry", fname_lower))      return(c("External - FamaFrench", "Fama-French 17 Industry Portfolio returns."))
  
  return(c("Unassigned", "General Data File"))
}

# --- FILE PROCESSOR ---
get_file_info <- function(filepath) {
  ext <- tolower(file_ext(filepath))
  fname <- basename(filepath)
  
  # Get Smart Classification
  cls <- classify_file(fname, filepath)
  role <- cls[1]
  desc <- cls[2]
  
  # Get Size
  fsize <- format(object.size(file.info(filepath)$size), units="auto")
  
  # Get Variables & Rows
  cols <- NA
  nrows <- NA
  
  tryCatch({
    if (ext == "parquet") {
      ds <- open_dataset(filepath)
      cols <- paste(names(ds), collapse = ", ")
      # Only count rows if file is small enough (<200MB) to be fast, else say "Large"
      if(file.info(filepath)$size < 200 * 1024^2) {
         nrows <- tryCatch(ds %>% count() %>% collect() %>% as.numeric(), error = function(e) "Lazy")
      } else {
         nrows <- "Huge (Lazy)"
      }
      
    } else if (ext == "csv") {
      df <- read_csv(filepath, n_max = 1, show_col_types = FALSE)
      cols <- paste(names(df), collapse = ", ")
      nrows <- "CSV"
      
    } else if (ext == "xlsx") {
      df <- read_excel(filepath, sheet = 1, n_max = 1)
      cols <- paste(names(df), collapse = ", ")
      nrows <- "Excel"
      
    } else if (ext == "sqlite") {
      con <- dbConnect(SQLite(), filepath)
      cols <- paste("Tables:", paste(dbListTables(con), collapse=", "))
      nrows <- "SQL DB"
      dbDisconnect(con)
    }
  }, error = function(e) { cols <<- paste("Error:", e$message) })
  
  tibble(
    `File Name` = fname,
    `Project Role` = role,
    `Description` = desc,
    `Variables` = cols,
    `Row Count` = as.character(nrows),
    `Size` = fsize,
    `Path` = gsub(paste0(getwd(), "/"), "", filepath)
  )
}

# --- MAIN EXECUTION ---
cat("Scanning Data Directory:", DATA_DIR, "...\n")
all_files <- dir_ls(DATA_DIR, recurse = TRUE, type = "file")
all_files <- all_files[!grepl("DS_Store", all_files)] # Remove system files

doc_table <- map_dfr(all_files, get_file_info) %>%
  arrange(`Project Role`, `File Name`)

# --- EXPORT TO EXCEL ---
wb <- createWorkbook()
addWorksheet(wb, "Data Dictionary")
writeData(wb, "Data Dictionary", doc_table, withFilter = TRUE)

# Styling
headerStyle <- createStyle(fontSize = 12, fontColour = "white", fgFill = "#2c3e50", halign = "center", textDecoration = "bold")
addStyle(wb, "Data Dictionary", headerStyle, rows = 1, cols = 1:ncol(doc_table))
setColWidths(wb, "Data Dictionary", cols = 1:ncol(doc_table), widths = "auto")
setColWidths(wb, "Data Dictionary", cols = 4, widths = 60) # Wide Variables column

saveWorkbook(wb, OUTPUT_FILE, overwrite = TRUE)

cat("\n✅ SUCCESS! Dictionary created at:\n", OUTPUT_FILE, "\n")
print(doc_table %>% select(`File Name`, `Project Role`))
