### Checking biomass dry weight for chemical trait analysis

# Load libraries
library(tidyverse)
library(readxl)
library(writexl)

# Functions
# total DW if combined within groups
DWfilterTotal <- function(DataSet, logical){
  DataSet %>%
    filter(minimum_chemTraitDW == logical) %>%
    group_by(project, siteID, experiment, taxon) %>%
    summarise(sumDW = sum(dry_mass))
}
# Total count of DW's within groups
DWfilterCount <- function(DataSet, logical){
  DataSet %>%
    filter(minimum_chemTraitDW == logical) %>%
    group_by(project, siteID, experiment, taxon) %>%
    summarise(amount = n())
}

# import data
dry_mass <- read_excel(path = "clean_data/traits/PFTC6_Norway_traits_DW_clean_2022.xlsx")

cleaned_traits <- read_excel(path = "clean_data/traits/PFTC6_Norway_traits_clean_2022.xlsx")


# Minimum dry mass in g
minDW = 0.03

# Check each value if enough
dry_mass_min <- dry_mass %>%
  mutate(minimum_chemTraitDW = if_else(dry_mass >= minDW, T, F))


# Read the cleaned data file or add this code snippet to the cleaning file after cleaning. For now just using the clean_trait_data.R script and having the values loaded and cleaned from there.
clean_traits_DW <- left_join(cleaned_traits, dry_mass_min, by = "ID")

# Check if there is too little material if the sample can be merged with another sample from the same site, treatment, and plot

DW_small.1 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==F) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(sumS = sum(dry_mass))
DW_small.2 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==F) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(amountS = n())
DW_small <- left_join(DW_small.1,DW_small.2, by =c("project", "siteID", "experiment", "taxon"))
DW_small <- DW_small %>%
  add_column(merges_amount = DW_small$sumS / minDW) %>%
  mutate(merges_amount = trunc(merges_amount))

# Not needed.
DW_big.1 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==T) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(sumB = sum(dry_mass))
DW_big.2 <- clean_traits_DW %>%
  filter(minimum_chemTraitDW==T) %>%
  group_by(project, siteID, experiment, taxon) %>%
  summarise(amountB = n())
DW_big <- left_join(DW_big.1,DW_big.2, by =c("project", "siteID", "experiment", "taxon"))

DW_both <- left_join(DW_big,DW_small, by =c("project", "siteID", "experiment", "taxon"))
DW_both <- DW_both %>%
  mutate(total_wMerge = amountB + if_else(is.na(merges_amount), 0, merges_amount))



DW_small.1_test <- DWfilterTotal(clean_traits_DW, F)
DW_small.2_test <- DWfilterCount(clean_traits_DW, F)
DW_big.1_test <- DWfilterTotal(clean_traits_DW, T)
DW_big.2_test <- DWfilterCount(clean_traits_DW, T)
DW_both_test <- left_join(DW_big.1_test, DW_big.2_test, by = c("project", "siteID", "experiment", "taxon")) %>%
  left_join(.,DW_small.1_test, by = c("project", "siteID", "experiment", "taxon")) %>%
  left_join(.,DW_small.2_test, by = c("project", "siteID", "experiment", "taxon")) %>%
  rename("sumB" = sumDW.x,
         "amountB" = amount.x,
         "sumS" = sumDW.y,
         "amountS" = amount.y) %>%
  add_column(merges_amount = .$sumS / minDW) %>%
  mutate(merges_amount = trunc(merges_amount)) %>%
  mutate(total_wMerge = amountB + if_else(is.na(merges_amount), 0, merges_amount))
