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
        "Carex sp",   "graminoid", "Cyperaceae",
        "Dryas octopetala",   "forb", "Rosaceae",
        "Empetrum rubrum",   "woody", "Ericaceae",
        "Phegopteris connectilis",   "xx", "xx",
        "Urtica dioica",   "forb", "Rosaceae",
        "Cladonia rangiferina",   "lichen", "xx",
        "Kindbergia praelonga",   "xx", "xx"

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
    command = {

      # leaf flux species
      leaf_flux <- tibble(project = "ii leaf flux",
             species = c("Alchemilla alpina",
                         "Achillea millefolium",
                         "Agrostis capillaris",
                         "Vaccinium vitis-idaea"))

      # remote sensing species
      remote_sensing <- tibble(project = "vi multispectral imagery",
                          species = c("Achillea millefolium",
                                      "Dryas octopetala",
      "Luzula multiflora",
      "Trifolium pratense",
      "Aconitum septentrionale",
      "Empetrum rubrum",
      "Nardus stricta",
      "Trifolium repens",
      "Agrostis capillaris",
      "Festuca ovina",
      "Phegopteris connectilis",
      "Urtica dioica",
      "Alchemilla alpina",
      "Festuca rubra",
      "Poa pratensis",
      "Vaccinium myrtillus",
      "Anthoxanthum odoratum",
      "Galium boreale",
      "Ranunculus acris",
      "Vaccinium uliginosum",
      "Avenella flexuosa",
      "Galium verum",
      "Rumex acetosa",
      "Vaccinium vitis-idaea",
      "Carex leporina",
      "Hypericum maculatum",
      "Rumex acetosella",
      "Cladonia rangiferina",
      "Juniperus communis",
      "Salix herbacea",
      "Deschampsia cespitosa",
      "Kindbergia praelonga",
      "Silene acaulis"))


      leaf_traits_clean |>
        distinct(project, gradient, species) |>
        filter(!is.na(project)) |>
        bind_rows(leaf_flux, remote_sensing) |>
        left_join(taxon_table, by = c("species" = "species_name")) |>
        mutate(project = paste(project, gradient, sep = "_"),
               project = recode(project, `3D_gradient` = "i traits gradient", Incline_NA = "i traits warming", "3D_NA" = "i traits global change"),
               presence = "x") |>
        select(-gradient) |>
        pivot_wider(names_from = project, values_from = presence) |>
        arrange(functional_group, species) |>
        select(functional_group, family, species, `i traits gradient`:`i traits global change`, `ii leaf flux` = `ii leaf flux_NA`, `vi multispectral imagery` = `vi multispectral imagery_NA`)

    }

  ),

  tar_target(
    name = traits_taxon_table_output,
    command =  save_csv(traits_taxon_table,
                        name = "PFTC6_traits_taxon_table2_2022.csv"),
    format = "file"
  )

)




