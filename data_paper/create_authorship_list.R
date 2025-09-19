# Create Authorship List for PFTC6 Data Paper
# This script generates a properly formatted authorship list with numbered affiliations

# Load required libraries
library(dplyr)
library(stringr)
library(readxl)
library(quarto)

# Source the functions from the other scripts
source("data_paper/generate_authorship_list.R")

# Function to load data from Excel file
load_from_excel <- function(file_path = "clean_data/PFTC6 papers authorship info.xlsx") {
  cat("Loading data from Excel file:", file_path, "\n")
  
  tryCatch({
    # Read the Excel file
    sheet_names <- excel_sheets(file_path)
    cat("Available sheets:", paste(sheet_names, collapse = ", "), "\n")
    
    # Find the authorship sheet
    authorship_sheet <- NULL
    for (sheet in sheet_names) {
      if (grepl("authorship|author", tolower(sheet))) {
        authorship_sheet <- sheet
        break
      }
    }
    
    if (is.null(authorship_sheet)) {
      authorship_sheet <- sheet_names[1]
    }
    
    cat("Using sheet:", authorship_sheet, "\n")
    
    # Read the sheet, skip the first 4 rows (headers)
    raw_data <- read_excel(file_path, sheet = authorship_sheet, skip = 4)
    
    # Clean and prepare the data
    cleaned_data <- raw_data %>%
      select(
        FullName = `Name as it should appear`,
        LastName = `(alphabetic within groups for now)`,
        MiddleInitials = `...3`,
        Address1 = `Address 1 , including POSTAL CODE`,
        OtherAddress = `Other Address...16`,
        OtherAddress2 = `Other Address...17`
      ) %>%
      filter(!is.na(LastName) & !is.na(FullName)) %>%
      mutate(across(everything(), ~str_trim(.)))
    
    cat("Successfully loaded", nrow(cleaned_data), "authors from Excel file.\n")
    return(cleaned_data)
    
  }, error = function(e) {
    cat("Error loading from Excel file:", e$message, "\n")
    return(NULL)
  })
}

# Function to create Quarto file for Word output
generate_quarto_word <- function(authorship, affiliations, output_qmd = "data_paper/authorship_list.qmd", output_docx = "data_paper/authorship_list.docx") {
  # Convert numbers to Unicode superscript characters
  superscript_map <- c("0" = "⁰", "1" = "¹", "2" = "²", "3" = "³", "4" = "⁴", "5" = "⁵", "6" = "⁶", "7" = "⁷", "8" = "⁸", "9" = "⁹")
  
  # Convert numbers to superscript in authorship string
  authorship_sup <- authorship
  for (i in 1:length(superscript_map)) {
    authorship_sup <- gsub(names(superscript_map)[i], superscript_map[i], authorship_sup, fixed = TRUE)
  }
  
  # Now convert commas between numbers to superscript using HTML tags
  authorship_sup <- gsub("([⁰¹²³⁴⁵⁶⁷⁸⁹]+),([⁰¹²³⁴⁵⁶⁷⁸⁹]+)", "<sup>\\1,\\2</sup>", authorship_sup)
  authorship_sup <- gsub("([⁰¹²³⁴⁵⁶⁷⁸⁹]+),", "<sup>\\1,</sup>", authorship_sup)
  authorship_sup <- gsub(",([⁰¹²³⁴⁵⁶⁷⁸⁹]+)", "<sup>,\\1</sup>", authorship_sup)

  # Build affiliations block
  affil_block <- paste0(seq_len(nrow(affiliations)), ". ", affiliations$Affiliation, collapse = "\n")

  # Write Quarto file
  qmd <- paste0(
    "---\ntitle: 'PFTC6 Authorship List'\nformat: docx\n---\n\n",
    "**Authors:**  ", authorship_sup, "\n\n",
    "**Affiliations:**\n\n",
    affil_block, "\n"
  )
  writeLines(qmd, output_qmd, useBytes = TRUE)
  
  # Render to docx
  out_filename <- basename(output_docx)
  quarto::quarto_render(output_qmd, output_file = out_filename)
  
  # Move the file if needed
  rendered_path <- file.path(dirname(output_qmd), out_filename)
  if (rendered_path != output_docx) file.rename(rendered_path, output_docx)
  cat("Word file generated at:", output_docx, "\n")
}

# Main function to create authorship list
create_authorship_list <- function(file_path = "clean_data/PFTC6 papers authorship info.xlsx") {
  # Load data from Excel
  data <- load_from_excel(file_path)
  if (is.null(data)) {
    cat("Failed to load data from Excel file.\n")
    return(NULL)
  }
  
  # Generate authorship list
  cat("Generating authorship list...\n")
  result <- generate_authorship_list(data, custom_order = TRUE)
  
  # Print results
  cat("\n" , rep("=", 50), "\n", sep = "")
  cat("AUTHORS:\n")
  cat(result$authorship, "\n\n")
  
  cat("AFFILIATIONS:\n")
  for (i in 1:nrow(result$affiliations)) {
    cat(result$affiliations$Number[i], ". ", result$affiliations$Affiliation[i], "\n", sep = "")
  }
  cat(rep("=", 50), "\n\n")
  
  # Save results
  writeLines(result$authorship, "data_paper/authorship_list.txt")
  write.csv(result$affiliations, "data_paper/affiliations_list.csv", row.names = FALSE)
  
  # Save detailed author data (convert list columns to character)
  authors_data_export <- result$authors_data %>%
    mutate(
      affil_numbers = sapply(affil_numbers, function(x) paste(x, collapse=",")),
      other_affil_numbers = sapply(other_affil_numbers, function(x) paste(x, collapse=",")),
      other2_affil_numbers = sapply(other2_affil_numbers, function(x) paste(x, collapse=",")),
      all_affil_numbers = sapply(all_affil_numbers, function(x) paste(x, collapse=","))
    )
  write.csv(authors_data_export, "data_paper/detailed_authors_data.csv", row.names = FALSE)
  
  cat("Results saved to:\n")
  cat("- data_paper/authorship_list.txt (authorship list)\n")
  cat("- data_paper/affiliations_list.csv (numbered affiliations)\n")
  cat("- data_paper/detailed_authors_data.csv (detailed author information)\n")
  
  return(result)
}

# Main execution
result <- create_authorship_list()

# Generate Word file via Quarto
if (!is.null(result)) {
  generate_quarto_word(result$authorship, result$affiliations)
} 