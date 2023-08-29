#### Make species list ####

community_plan <- list(

  # download data
  tar_target(
    name = funcab_taxon_download,
    command = get_file(node = "4c5v2",
                       file = "FunCaB_taxon_table.csv",
                       path = "clean_data",
                       remote_path = "3_Plant_composition"),
    format = "file"
  ),

  tar_target(
    name = taxon_table,
    command = {

      missing_sp <- tribble(
        ~species_name, ~functional_group, ~family,
        "Alchemilla sp",   "forb", "Rosaceae",
        "Antennaria alpina",   "forb", "Asteraceae",
        "Deschampsia alpina",   "graminoid", "Poaceae",
        "Empetrum nigrum",   "woody", "Ericaceae",
        "Euphrasia stricta",   "forb", "Orobanchaceae",
        "Galeopsis tetrahit",   "forb", "Lamiaceae",
        "Pyrola norvegica",   "forb", "Ericaceae",
        "Pyrola sp",   "forb", "Ericaceae",
        "Taraxacum sp",   "forb", "Asteraceae",
        "Carex sp",   "graminoid", "Cyperaceae"
      )

      read_csv(funcab_taxon_download) |>
        select(-c(species:seedlings)) |>
        distinct() |>
        bind_rows(missing_sp)

    }

  ),

  # make
  tar_target(
    name = traits_taxon_table,
    command = leaf_traits_clean |>
      distinct(project, gradient, species) |>
      filter(!is.na(project)) |>
      left_join(taxon_table, by = c("species" = "species_name")) |>
      mutate(project = paste(project, gradient, sep = "_"),
             project = recode(project, `3D_gradient` = "Gradient", Incline_NA = "Incline", "3D_NA" = "ThreeD"),
             presence = "x") |>
      select(-gradient) |>
      pivot_wider(names_from = project, values_from = presence) |>
      arrange(functional_group, species) |>
      select(functional_group, family, species, ThreeD, Gradient, Incline)

  ),

  tar_target(
    name = traits_taxon_table_output,
    command =  save_csv(traits_taxon_table,
                        name = "PFTC6_traits_taxon_table_2022.csv"),
    format = "file"
  )

)




