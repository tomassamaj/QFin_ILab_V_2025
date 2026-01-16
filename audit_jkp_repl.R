# ==============================================================================
# JKP DATA AUDIT: FORENSIC STRUCTURE CHECK
# Purpose: Diagnose why the dataset is 7.5M rows (should be ~100k for US Monthly)
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, arrow, lubridate, skimr)

# SET WORKING DIRECTORY
setwd("/Users/farkastallos/Library/CloudStorage/OneDrive-WUWien/00_WU/01_2_YEAR/07_ILab_ZZ/ILab_Code")
REPL_FILE  <- "pfs.parquet"

# 1. LOAD RAW DATA
cat("--- 1. LOADING RAW FILE ---\n")
if(!file.exists(REPL_FILE)) stop("pfs.parquet not found!")

# Load without filtering first to see the full scope
raw_data <- read_parquet(REPL_FILE)

cat("Rows:", format(nrow(raw_data), big.mark=","), "\n")
cat("Columns:", ncol(raw_data), "\n")
cat("Column Names:\n")
print(names(raw_data))

# 2. FREQUENCY CHECK (Is it Monthly or Daily?)
cat("\n--- 2. FREQUENCY AUDIT ---\n")
# Check the first few dates
date_col <- if("eom" %in% names(raw_data)) "eom" else "date"
cat("Date Column used:", date_col, "\n")

# Calculate days between observations for a single factor/country
freq_check <- raw_data %>%
  arrange(!!sym(date_col)) %>%
  slice(1:100) %>%
  pull(!!sym(date_col))

cat("First 5 Dates:", as.character(head(freq_check, 5)), "\n")
cat("Date Range:", as.character(min(raw_data[[date_col]])), "to", as.character(max(raw_data[[date_col]])), "\n")

# 3. COUNTRY CHECK (The likely culprit of the 7.5M rows)
cat("\n--- 3. COUNTRY DISTRIBUTION ---\n")
if("country" %in% names(raw_data)) {
  country_counts <- raw_data %>%
    count(country) %>%
    arrange(desc(n))
  
  cat("Total Unique Countries:", nrow(country_counts), "\n")
  print(head(country_counts, 10))
  
  if(nrow(country_counts) > 1) {
    cat("⚠️ WARNING: Dataset contains MULTIPLE countries. You must filter for 'USA' before comparing!\n")
  }
} else {
  cat("⚠️ WARNING: No 'country' column found. Is this a global file?\n")
}

# 4. FACTOR CHECK
cat("\n--- 4. FACTOR CHECK ---\n")
unique_factors <- unique(raw_data$characteristic)
cat("Unique Characteristics:", length(unique_factors), "\n")
print(head(unique_factors, 10))

# 5. DUPLICATION / GRANULARITY CHECK
# If we filter for USA, One Date, One Characteristic... do we get 1 row?
cat("\n--- 5. GRANULARITY TEST (The 'One Row' Rule) ---\n")

test_date <- max(raw_data[[date_col]])
test_char <- unique_factors[1]

# Try to drill down
granular_test <- raw_data %>%
  filter(!!sym(date_col) == test_date, characteristic == test_char)

if("country" %in% names(raw_data)) {
  granular_test <- granular_test %>% filter(country == "USA")
}

cat("Drilling down to: Date=", as.character(test_date), "| Char=", test_char, "| Country=USA\n")
cat("Resulting Rows:", nrow(granular_test), "\n")

if(nrow(granular_test) > 1) {
  cat("⚠️ CRITICAL FLAG: Found", nrow(granular_test), "rows for a single month/factor/country.\n")
  print(granular_test)
  cat("Hypothesis: Are there different weighting schemes? (e.g., ew, vw_cap) stacked in rows?\n")
} else {
  cat("✅ Granularity looks correct (1 row per factor/date).\n")
}

# 6. RETURN VALUE CHECK
cat("\n--- 6. RETURN VALUE DISTRIBUTION ---\n")
# Which column holds the return?
possible_ret_cols <- names(raw_data)[str_detect(names(raw_data), "ret")]
cat("Potential Return Columns:", paste(possible_ret_cols, collapse=", "), "\n")

# Summary of the first return column found
if(length(possible_ret_cols) > 0) {
  ret_col <- possible_ret_cols[1]
  cat("Summarizing:", ret_col, "\n")
  print(summary(raw_data[[ret_col]]))
  
  # Check for "Percent vs Decimal" issue
  if(max(raw_data[[ret_col]], na.rm=T) > 50) {
    cat("⚠️ WARNING: Max return > 50. Are these percentages (e.g. 5.0 for 5%)? JKP usually uses decimals (0.05).\n")
  }
}

cat("\n--- AUDIT COMPLETE ---\n")