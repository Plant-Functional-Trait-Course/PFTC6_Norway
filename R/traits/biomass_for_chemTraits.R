### Checking biomass dry weight for chemical trait analysis

# Load libraries
library(tidyverse)
library(readxl)
library(googlesheets4)

# import data
raw_dry_mass <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "DryMass")

# From google sheets
raw_dry_mass_google <- read_sheet("https://docs.google.com/spreadsheets/d/1ncqbniu0NUzCfcNe2fOl6M2Yj-BeOEXcYlPZHwNiIbk/edit#gid=0",
                   sheet = "DryMass",
                   col_names = TRUE)
# Minimum dry mass in g
minDW = 0.03


# Check each value if enough
raw_dry_mass_min <- raw_dry_mass %>%
  mutate(minimum_chemTraitDW = if_else(dry_mass >= minDW, T, F))


# Read the cleaned data file or add this code snippet to the cleaning file after cleaning. For now just using the clean_trait_data.R script and having the values loaded and cleaned from there.
clean_traits2_DW <- left_join(clean_traits2, raw_dry_mass_min, by = "ID")

# Check if there is too little material if the sample can be merged with another sample from the same site, treatment, and plot
clean_traits2_DW <- clean_traits2_DW %>%
  mutate(merge = case_when())



# # Wet and dry mass do not make sense for these species
# mutate(dry_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, dry_mass_g),
#        wet_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, wet_mass_g),
#        leaf_area_cm2 = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, leaf_area_cm2)) |>
