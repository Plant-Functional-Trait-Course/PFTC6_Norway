# chemical traits plan

chem_trait_plan <- list(

  # make ThreeD list
  tar_target(
    name = leaf_area_download,
    command = cnp_traits |>
      select(project, gradient, siteID, blockID, turfID, warming, grazing, Namount_kg_ha_y, individual_nr, ID, species, batchNR, chemID, dry_mass_total_g, cum_mass) |>
      ungroup() |>
      filter(project == "3D",
             Namount_kg_ha_y == 0,
             grazing == "C",
             !is.na(batchNR)) |>
      ungroup() |>
      group_by(project, siteID, blockID, turfID, warming, grazing, Namount_kg_ha_y, species) |>
      # three individuals per site, species and treatment
      filter(batchNR <= 3) |>
      ungroup() |>
      arrange(siteID, blockID, warming, grazing, Namount_kg_ha_y, turfID, species)
  ),

  write_csv("PFTC6_3D_cnp_traits_warmingTreatment_5-may-2023.csv")

)

# select 3 for 3D and 6 for incline


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
