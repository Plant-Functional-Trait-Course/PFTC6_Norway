### Checking biomass dry weight for chemical trait analysis

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

# Minimum dry mass needed for CNP analysis in g
# 1.5 - 1.7 mg for CN + isotopes
# 3 sub-samples of 3.5 to 4 mg for phosphorus (12 mg)
# minDW_cn = 0.004

traits <- read_csv("clean_data/PFTC6_clean_leaf_traits_2022.csv")
# Check for each value if enough material
# wide_traits <- leaf_traits_clean %>%
#   select(ID:species, trait, value, comment, problem, flag) |>
#   pivot_wider(names_from = trait, values_from = value) |>
#   # remove leaves where dry mass is missing
#   filter(!is.na(dry_mass_total_g))

get_list_chem_traits <- function(leaf_traits_clean){

  wide_traits <- leaf_traits_clean_full |>
    select(ID, date, project, gradient, siteID, elevation_m_asl, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, individual_nr, species = taxon,
           dry_mass_total_g, comment, problem, flag)

  # Minimum dry mass needed for CNP analysis in g
  # 1.5 - 1.7 mg for CN + isotopes
  # 3 sub-samples of 3.5 to 4 mg for phosphorus (12 mg)
  minDW_cnp = 0.014

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


}


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

