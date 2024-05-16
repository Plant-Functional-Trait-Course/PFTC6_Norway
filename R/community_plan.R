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
        "Phegopteris connectilis",   "fern", "Thelypteridaceae",
        "Urtica dioica",   "forb", "Rosaceae",
        "Deschampsia flexuosa",   "graminoid", "Poaceae",
        "Vaccinium myrtillus",   "woody", "Ericaceae",
        "Cladonia rangiferina",   "lichen", "Cladoniaceae",
        "Kindbergia praelonga",   "bryophyte", "Brachytheciaceae"

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
      leaf_flux <- tibble(project = "ii Leaf assimilation-temperature responses",
             species = c("Alchemilla alpina",
                         "Achillea millefolium",
                         "Vaccinium vitis-idaea"))

      leaf_hyperspect <- tibble(project = "iii Leaf hyperspectral imagery",
        species = c("Achillea millefolium",
        "Agrostis capillaris",
        "Alchemilla alpina",
        "Deschampsia flexuosa",
        "Empetrum rubrum",
        "Festuca rubra",
        "Hypericum maculatum",
        "Kindbergia praelonga",
        "Rumex acetosa",
        "Vaccinium myrtillus",
        "Vaccinium vitis-idaea",
        "Veronica alpina")
      )

      # remote sensing species
      remote_sensing <- tibble(project = "vi airborne multispectral imagery",
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
        bind_rows(leaf_flux, leaf_hyperspect) |>
        left_join(taxon_table, by = c("species" = "species_name")) |>
        mutate(project = paste(project, gradient, sep = "_"),
               project = recode(project, `3D_gradient` = "i Traits gradient", Incline_NA = "i Traits warming", "3D_NA" = "i Traits global change"),
               presence = "x") |>
        select(-gradient) |>
        pivot_wider(names_from = project, values_from = presence) |>
        arrange(functional_group, species) |>
        select(functional_group, family, species, `i Traits gradient`, `i Traits warming`, `i Traits global change`, `ii Leaf assimilation-temperature responses` = `ii Leaf assimilation-temperature responses_NA`, `iii Leaf hyperspectral imagery_NA` = `iii Leaf hyperspectral imagery_NA`)

    }

  ),

  tar_target(
    name = traits_taxon_table_output,
    command =  save_csv(traits_taxon_table,
                        name = "PFTC6_taxon_table_2022.csv"),
    format = "file"
  )

)




