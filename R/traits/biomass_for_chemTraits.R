### Checking biomass dry weight for chemical trait analysis

# Load libraries
library(tidyverse)
library(readxl)

# import data
# Here I have saved the cleaned trait data into two files and then I recombine them in R again to calculate how many samples need to be merged
dry_mass <- read_excel(path = "clean_data/traits/PFTC6_Norway_traits_DW_clean_2022.xlsx")
cleaned_traits <- read_excel(path = "clean_data/traits/PFTC6_Norway_traits_clean_2022.xlsx")

# Minimum dry mass needed in g
minDW = 0.03

# Check for each value if enough material
dry_mass_min <- dry_mass %>%
  mutate(minimum_chemTraitDW = if_else(dry_mass >= minDW, T, F))

# combine DW data and cleaned trait data
clean_traits_DW <- left_join(cleaned_traits, dry_mass_min, by = "ID")

# Check if there is too little material if the sample can be merged with another sample from the same project, site, treatment, and plot

# Small samples
# Total DW when all combined
DW_small.1 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==F) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(sumS = sum(dry_mass))
# number of samples below the threshold
DW_small.2 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==F) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(amountS = n())

# How much material and how many samples are above the threshold
DW_big.1 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==T) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(sumB = sum(dry_mass))
DW_big.2 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==T) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(amountB = n())

# Combine and determine max theoretical amount of samples after merging (merges_amount) and total samples incl. where enough (total_wMerge)
DW_both <- left_join(DW_big.1, DW_big.2, by = c("project", "siteID", "experiment", "taxon")) %>%
  left_join(.,DW_small.1, by = c("project", "siteID", "experiment", "taxon")) %>%
  left_join(.,DW_small.2, by = c("project", "siteID", "experiment", "taxon")) %>%
  add_column(merges_amount = .$sumS / minDW) %>%
  mutate(merges_amount = trunc(merges_amount)) %>%
  mutate(total_wMerge = amountB + if_else(is.na(merges_amount), 0, merges_amount))
view(DW_both)
