### Checking biomass dry weight for chemical trait analysis

# Load libraries
source("R/load_libraries.R")

# import data
traits <- read_csv(file = "clean_data/PFTC6_clean_leaf_traits_2022.csv")

# Minimum dry mass needed for CNP analysis in g
# 1.5 - 1.7 mg for CN + isotopes
# 3 sub-samples of 3.5 to 4 mg for phosphorus (12 mg)
minDW_cn = 0.004
minDW_cnp = 0.014

# Function to get the number of batches per plot and species
get_chem_trait_batch <- function(x, minDW, i = 1){
  if(length(x) == 0) return()
  # get cumsum of mass
  cx <- cumsum(x)
  # if not enough mass give NA
  if(max(cx) < minDW) return(rep(NA, times = length(x)))
  # put all the leaves in the same batch unitl minDW is reached
  batch <- which.max(cx > minDW)

  # recursive function (= calling own function again on the rest of the data)
  c(rep(i, times = batch), get_chem_trait_batch(x[(1:length(x)) > batch], minDW, i = i + 1))

}


# Check for each value if enough material
wide_traits <- traits %>%
  select(ID:species, dry_mass_total_g, nr_leaves_wm, nr_leaves_dm, trait, value, comment, flag) |>
  pivot_wider(names_from = trait, values_from = value) |>
  # remove leaves where dry mass is missin
  filter(!is.na(dry_mass_total_g))

# Only cn analysis (not used)
# cn_traits <- wide_traits |>
#   # group by plot and species and arrange by mass
#   arrange(project, siteID, blockID, warming, grazing, Nlevel, species, -dry_mass_g) |>
#   group_by(project, siteID, blockID, warming, grazing, Nlevel, species) |>
#   # get cumsum and
#   mutate(cum_mass = cumsum(dry_mass_g),
#          batchNR = get_chem_trait_batch(x = dry_mass_g, minDW = minDW_cn)) |>
#   group_by(project, siteID, blockID, warming, grazing, Nlevel, species, batchNR) |>
#   mutate(chemID = if_else(!is.na(batchNR), str_c(ID, collapse = "_"), NA_character_)) |>
#   select(ID:species, dry_mass_g, cum_mass, batchNR, chemID, everything())

# cnp analysis
cnp_traits <- wide_traits |>
  # group by plot and species and arrange by mass
  arrange(project, siteID, blockID, warming, grazing, Namount_kg_ha_y, species, -dry_mass_total_g) |>
  group_by(project, siteID, blockID, warming, grazing, Namount_kg_ha_y, species) |>
  # get cumsum and
  mutate(cum_mass = cumsum(dry_mass_total_g),
         batchNR = get_chem_trait_batch(x = dry_mass_total_g, minDW = minDW_cnp)) |>
  group_by(project, siteID, blockID, warming, grazing, Namount_kg_ha_y, species, batchNR) |>
  mutate(chemID = if_else(!is.na(batchNR), str_c(ID, collapse = "_"), NA_character_)) |>
  select(ID:species, dry_mass_total_g, cum_mass, batchNR, chemID, everything())

# # for 3D because we want Namount not Nlevel
# cnp_traits_3D <- wide_traits |>
#   # group by plot and species and arrange by mass
#   arrange(project, siteID, blockID, warming, grazing, Namount_kg_ha_y, species, -dry_mass_g) |>
#   group_by(project, siteID, blockID, warming, grazing, Namount_kg_ha_y, species) |>
#   # get cumsum and
#   mutate(cum_mass = cumsum(dry_mass_g),
#          batchNR = get_chem_trait_batch(x = dry_mass_g, minDW = minDW_cnp)) |>
#   group_by(project, siteID, blockID, warming, grazing, Nlevel, species, batchNR) |>
#   mutate(chemID = if_else(!is.na(batchNR), str_c(ID, collapse = "_"), NA_character_)) |>
#   select(ID:species, dry_mass_g, cum_mass, batchNR, chemID, everything())
#

# select 3 for 3D and 6 for incline
cnp_traits |>
  select(project, gradient, siteID, blockID, turfID, warming, grazing, Namount_kg_ha_y, individual_nr, ID, species, batchNR, chemID, dry_mass_total_g, cum_mass) |> ungroup() |>
  filter(project == "3D",
         Namount_kg_ha_y == 0,
         grazing == "C",
         !is.na(batchNR)) |>
  ungroup() |>
  group_by(project, siteID, blockID, turfID, warming, grazing, Namount_kg_ha_y, species) |>
  # three individuals per site, species and treatment
  filter(batchNR <= 3) |>
  ungroup() |>
  arrange(siteID, blockID, warming, grazing, Namount_kg_ha_y, turfID, species) |>
  write_csv("PFTC6_3D_cnp_traits_warmingTreatment_5-may-2023.csv")

cnp_traits |>
  ungroup() |>
  select(project, siteID, blockID, warming, individual_nr, ID, species, batchNR, chemID, dry_mass_total_g, cum_mass) |>
  filter(project == "Incline",
         !is.na(batchNR)) |>
  group_by(project, siteID, blockID, warming, species) |>
  # six individuals per site, species and treatment
  filter(batchNR <= 6) |>
  ungroup() |>
  arrange(siteID, blockID, warming, species, batchNR) |>
  write_csv("PFTC6_Incline_cnp_traits_5-may-2023.csv")

Incline2 <- read_csv("PFTC6_Incline_cnp_traits2_2-may-2023.csv")

Incline2 |> distinct(chemID)
Incline5 |> distinct(chemID)

Incline2 |> distinct(chemID) |> anti_join(Incline5 |> distinct(chemID)) |> write_csv("PFTC6_Incline_changing_chemID.csv")
Incline5 |> distinct(chemID) |> anti_join(Incline2 |> distinct(chemID)) |> write_csv("PFTC6_Incline_new_chemID.csv")
