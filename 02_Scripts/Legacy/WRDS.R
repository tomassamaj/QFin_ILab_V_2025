# --- 0. Load Required Packages ---
# Ensure these packages are installed: install.packages(c("RPostgres", "httr", "readxl", "tidyverse", "data.table"))
library(RPostgres)
library(httr)
library(readxl)
library(tidyverse)
library(data.table) # For fwrite in the RAM-limited section

# Retrieve username and password from environment variables
wrds_username <- Sys.getenv("WRDS_USER")
wrds_password <- Sys.getenv("WRDS_PASSWORD")

# Check if variables were found
if (wrds_username == "" || wrds_password == "") {
  stop("WRDS_USER or WRDS_PASSWORD environment variable not set.")
}


# --- 1. Connect to WRDS ---
# Replace 'USERNAME' and 'PASSWORD' with your actual WRDS credentials
# Note: You might need to approve a Duo push notification after running this.
wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  dbname = 'wrds',
                  sslmode = 'require',
                  user = wrds_username,
                  password = wrds_password)

# --- 2. Define Your Desired Data Subset ---

# Helper function to download Excel files from GitHub (requires GITHUB_PAT env variable if private)
github_excel <- function(link) {
  temp_file <- tempfile(fileext = ".xlsx")
  # Use authenticate only if needed (e.g., for private repos)
  # req <- GET(link, authenticate(Sys.getenv("GITHUB_PAT"), ""), write_disk(path = temp_file))
  req <- GET(link, write_disk(path = temp_file)) # Simpler for public repos
  if (status_code(req) != 200) {
    stop("Failed to download file from GitHub. Status code: ", status_code(req))
  }
  data <- readxl::read_excel(temp_file)
  unlink(temp_file)
  return(data)
}

# --- a) Select Countries (e.g., Developed Markets) ---
print("Fetching country classification...")
country_link <- "https://github.com/bkelly-lab/ReplicationCrisis/raw/master/GlobalFactors/Country%20Classification.xlsx"
countries <- github_excel(country_link)

# Extract ISO codes for developed countries
countries_rel <- countries %>%
  filter(msci_development == "developed") %>%
  pull(excntry)

print("Selected countries (Developed):")
print(countries_rel)

# --- b) Select Characteristics (e.g., JKP 153 Factors) ---
print("Fetching factor details...")
chars_link <- "https://github.com/bkelly-lab/ReplicationCrisis/raw/master/GlobalFactors/Factor%20Details.xlsx"
chars <- github_excel(chars_link)

# Extract the relevant characteristic abbreviations used by JKP
chars_rel <- chars %>%
  filter(!is.na(abr_jkp)) %>%
  pull(abr_jkp)

print(paste("Selected", length(chars_rel), "characteristics."))

# --- c) Define Standard JKP Filters ---
jkp_filters <- "common=1 and exch_main=1 and primary_sec=1 and obs_main=1"
print("Using standard JKP filters:")
print(jkp_filters)

# --- 3. Extract Data ---

# Helper function to fetch data safely from WRDS
wrds_fetch <- function(wrds_conn, sql_query, n = -1) {
  print("Sending SQL query to WRDS...")
  res <- dbSendQuery(wrds_conn, sql_query)
  print("Fetching data...")
  data <- dbFetch(res, n = n)
  dbClearResult(res)
  print("Data fetched successfully.")
  return(as_tibble(data)) # Return as a tibble
}

# --- Option A: Extract Data for Many Countries (Requires Sufficient RAM) ---
# This might take ~25 minutes and require >12GB RAM for developed countries & 153 factors

print("Constructing SQL query for multiple developed countries...")
sql_query_multi_country <- paste0(
  # Select identifying columns and the desired characteristics
  "SELECT id, eom, excntry, gvkey, permno, ",
  paste0(chars_rel, collapse = ", "), # Collapse characteristic names with commas
  # Specify the table
  " FROM contrib.global_factor",
  # Apply standard JKP filters and country filter
  " WHERE ", jkp_filters, " and ",
  "excntry IN (", paste0("'", countries_rel, "'", collapse = ", "), ")" # Create IN ('USA', 'JPN', ...) list
  , ";" # End of SQL query
)

# --- Uncomment the following lines to run the multi-country download ---
# print("Executing multi-country query (this may take a while)...")
# jkp_data_developed <- wrds_fetch(wrds, sql_query_multi_country)
# print("Multi-country data download complete.")
# print("Dimensions of downloaded data:")
# print(dim(jkp_data_developed))
# print("Head of downloaded data:")
# print(head(jkp_data_developed))
# ---

# --- Option B: Extract Data Country-by-Country (Lower RAM Usage) ---
# This downloads data for each country and saves it as a separate CSV file.

print("--- Starting Country-by-Country Download (Example for first 3 countries) ---")
# Adjust the loop range or remove slicing `[1:3]` to download for all selected countries
# for (country in countries_rel[1:3]) { # Example: Loop through the first 3 countries
for (country in countries_rel) { # Loop through ALL selected countries
  
  print(paste("Constructing SQL query for:", country))
  sql_query_single_country <- paste0(
    "SELECT id, eom, excntry, gvkey, permno, ",
    paste0(chars_rel, collapse = ", "),
    " FROM contrib.global_factor",
    " WHERE ", jkp_filters, " and ",
    "excntry = '", country, "'", # Filter for the specific country
    ";"
  )
  
  # Get data for the current country
  print(paste("Fetching data for:", country))
  country_data <- wrds_fetch(wrds, sql_query_single_country)
  
  # Define filename and save using fwrite for speed
  filename <- paste0("jkp_data_", country, ".csv")
  print(paste("Saving data to:", filename))
  fwrite(country_data, filename)
  print(paste("Finished processing:", country))
  
  # Optional: Clean up memory if needed within the loop
  # rm(country_data)
  # gc()
}
print("--- Country-by-Country Download Complete ---")


# --- 4. Disconnect from WRDS ---
# It's good practice to disconnect when finished
dbDisconnect(wrds)
print("Disconnected from WRDS.")

# --- Notes ---
# - To get data for ALL countries, modify Option A's WHERE clause:
#   Remove the "and excntry IN (...)" part.
# - To get ALL columns (characteristics), modify the SELECT clause:
#   Change "SELECT id, eom, ..., column_list" to "SELECT *".
# - If you encounter issues, consider checking the JKP GitHub repo issues page.