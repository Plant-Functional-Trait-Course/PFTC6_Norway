# Test script for authorship list generation
# This demonstrates the new "Last name, First initial" format and custom ordering

# Load required libraries
library(dplyr)
library(stringr)

# Source the main function
source("data_paper/generate_authorship_list.R")

# Create test data
test_data <- data.frame(
  LastName = c("Vandvik", "Halbritter", "Macias-Fauria", "Maitner", "Michaletz", "Telford"),
  FirstName = c("Vigdis", "Aud", "Marc", "Brian", "Sean", "Richard"),
  MiddleNames = c("", "H", "", "Salvin", "Thomas", "James"),
  Address1 = c("Department of Biological Sciences, University of Bergen, POBOX 7801 N-5020 Bergen, Norway",
               "Department of Biological Sciences, University of Bergen, POBOX 7801 N-5020 Bergen, Norway",
               "Scott Polar Research Institute, University of Cambridge, Cambridge CB2 1ER, United Kingdom",
               "Department of Ecology and Evolutionary Biology, University of Arizona, Tucson, AZ, USA",
               "Department of Biology, University of New Mexico, Albuquerque, NM, USA",
               "Department of Biology, University of Bergen, Bergen, Norway"),
  OtherAddress = c("Bjerknes Centre for Climate Research, University of Bergen, Bergen, Norway",
                   "Bjerknes Centre for Climate Research, University of Bergen, Bergen, Norway",
                   "", "", "", ""),
  OtherAddress2 = c("", "", "", "", "", "")
)

# Test with custom ordering
cat("Testing authorship list generation with custom ordering...\n\n")

result <- generate_authorship_list(test_data, custom_order = TRUE)

cat("AUTHORS (with custom ordering):\n")
cat(result$authorship, "\n\n")

cat("AFFILIATIONS:\n")
for (i in 1:nrow(result$affiliations)) {
  cat(result$affiliations$Number[i], ". ", result$affiliations$Affiliation[i], "\n", sep = "")
}

cat("\n" , rep("=", 50), "\n", sep = "")

# Test with alphabetical ordering for comparison
cat("Testing with alphabetical ordering for comparison...\n\n")

result_alpha <- generate_authorship_list(test_data, custom_order = FALSE)

cat("AUTHORS (alphabetical):\n")
cat(result_alpha$authorship, "\n\n")

# Show the formatted names for verification
cat("Formatted names in custom order:\n")
for (i in 1:nrow(result$authors_data)) {
  cat(i, ". ", result$authors_data$formatted_name[i], "\n", sep = "")
}

cat("\nTest completed successfully!\n") 