### Clean trait data

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(validate)
library(PFTCFunctions)
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


# download raw trait data from OSF
get_file(node = "pk4bg",
         file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
         path = "raw_data/traits/",
         remote_path = "RawData/Traits")

# import data
raw_traits <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "Data")

raw_dry_mass <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "DryMass")

raw_leaf_area <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_area_2022.xlsx")



# Data curation

raw_traits |>

  # fix day, siteID, project and experiment
  mutate(day = ,
         sitID = ,
         ...) |>

  # check new_taxon column and add names to taxon
  mutate(taxon = case_when()) |>

  # fix numeric values (traits)
  mutate(leaf_thickness_2_mm = ) |>

  # join leaf area and dry mass data
  left_join(leaf_area, by = "ID") |>
  left_join(dry_mass, by = "ID") |>

  # calculate average thickness, sla and ldmc
  # rename
  rename(wet_mass_total_g = wet_mass_g,
         leaf_area_total_cm2 = leaf_area_cm2,
         dry_mass_total_g = dry_mass_g,
         nr_leaves = bulk_nr_leaves) |>

  # Calculate average leaf thickness
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / nr_leaves,
         dry_mass_g = dry_mass_total_g / nr_leaves,
         leaf_area_cm2 = leaf_area_total_cm2 / nr_leaves) |>

  # Wet and dry mass do not make sense for these species
  mutate(dry_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, dry_mass_g),
         wet_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, wet_mass_g),
         leaf_area_cm2 = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, leaf_area_cm2)) |>

  # Calculate SLA and LDMC (replace with wet mass for now)
  mutate(sla_cm2_g = leaf_area_cm2 / wet_mass_g,
         ldmc = dry_mass_g / wet_mass_g)



