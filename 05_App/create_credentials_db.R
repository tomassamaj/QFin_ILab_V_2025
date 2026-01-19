# --- 1. Install necessary packages ---
# You only need to run this install part once
if (!require("shinymanager")) install.packages("shinymanager")
if (!require("RSQLite")) install.packages("RSQLite")

library(shinymanager)
library(RSQLite)

# --- 2. Define your credentials in plain text ---
# (Set your real password for Ilab_member here)
credentials_data <- data.frame(
  user = c("admin", "Ilab_member"),
  password = c("Ilab_Momentum_12_1", "Ilab_member_12_1"),
  is_admin = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

# --- 3. Define a passphrase for your database ---
# *** UPDATED to your new passphrase ***
db_passphrase <- "Ilab_member_12_1" 

# --- 4. Create the SQLite database ---
create_db(
  credentials_data = credentials_data,
  sqlite_path = "credentials.sqlite", # This creates the file in your current folder
  passphrase = db_passphrase
)

print("--- 'credentials.sqlite' database created successfully! ---")
print("You can now upload this file with your app.")
