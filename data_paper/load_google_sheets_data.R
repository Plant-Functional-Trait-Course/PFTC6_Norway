# Load data from Google Sheets for PFTC6 authorship
# This script downloads the data from the Google Sheets and prepares it for processing

library(googlesheets4)
library(dplyr)
library(readr)

# Function to load and clean the Google Sheets data
load_authorship_data <- function() {
  # Google Sheets URL
  sheet_url <- "https://docs.google.com/spreadsheets/d/1Z42Gl-SJCQzn1C9VwfZGHkiL1gKCuLfYXXHd7mjHSbg/edit?gid=1667760889#gid=1667760889"
  
  # Read the data from the "Data paper authorship" tab
  # Note: You may need to authenticate with Google Sheets first
  # gs4_auth() # Uncomment this line if you need to authenticate
  
  tryCatch({
    # Read the sheet
    raw_data <- read_sheet(sheet_url, sheet = "Data paper authorship")
    
    # Clean and prepare the data
    cleaned_data <- raw_data %>%
      # Select relevant columns based on the structure you described
      select(
        LastName = A,           # Column A: Last name
        FirstName = B,          # Column B: First name  
        MiddleNames = C,        # Column C: Middle names
        Address1 = O,           # Column O: Primary address
        OtherAddress = P,       # Column P: Other address
        OtherAddress2 = Q       # Column Q: Additional address
      ) %>%
      # Remove rows with missing names
      filter(!is.na(LastName) & !is.na(FirstName)) %>%
      # Clean up whitespace
      mutate(across(everything(), ~str_trim(.)))
    
    return(cleaned_data)
    
  }, error = function(e) {
    cat("Error loading data from Google Sheets:", e$message, "\n")
    cat("You may need to authenticate with Google Sheets first.\n")
    cat("Run: gs4_auth() to authenticate.\n")
    return(NULL)
  })
}

# Alternative function to load from a local CSV file
# (Use this if you export the Google Sheets data to CSV)
load_authorship_data_from_csv <- function(file_path) {
  raw_data <- read_csv(file_path)
  
  # Clean and prepare the data
  cleaned_data <- raw_data %>%
    # Select relevant columns - adjust column names as needed
    select(
      LastName = `A`,           # Column A: Last name
      FirstName = `B`,          # Column B: First name  
      MiddleNames = `C`,        # Column C: Middle names
      Address1 = `O`,           # Column O: Primary address
      OtherAddress = `P`,       # Column P: Other address
      OtherAddress2 = `Q`       # Column Q: Additional address
    ) %>%
    # Remove rows with missing names
    filter(!is.na(LastName) & !is.na(FirstName)) %>%
    # Clean up whitespace
    mutate(across(everything(), ~str_trim(.)))
  
  return(cleaned_data)
}

# Function to save the cleaned data for later use
save_cleaned_data <- function(data, file_path = "data_paper/cleaned_authorship_data.csv") {
  write_csv(data, file_path)
  cat("Cleaned data saved to:", file_path, "\n")
}

# Main execution
if (FALSE) {  # Set to TRUE to run this section
  # Try to load from Google Sheets
  authorship_data <- load_authorship_data()
  
  if (is.null(authorship_data)) {
    cat("Could not load from Google Sheets. Please check authentication or use CSV method.\n")
  } else {
    # Save cleaned data
    save_cleaned_data(authorship_data)
    
    # Show preview
    cat("Data loaded successfully. Preview:\n")
    print(head(authorship_data))
    cat("\nTotal authors:", nrow(authorship_data), "\n")
  }
}

# Instructions for use:
cat("To use this script:\n")
cat("1. Install required packages: install.packages(c('googlesheets4', 'dplyr', 'readr'))\n")
cat("2. Authenticate with Google Sheets: gs4_auth()\n")
cat("3. Run: authorship_data <- load_authorship_data()\n")
cat("4. Or export Google Sheets to CSV and use: authorship_data <- load_authorship_data_from_csv('your_file.csv')\n") 