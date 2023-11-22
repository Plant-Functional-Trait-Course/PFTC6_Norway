#### Download, import and clean trait data ####

trait_plan <- list(

  # download data
  #leaf area
  tar_target(
    name = leaf_area_download,
    command = get_file(node = "fcbw4",
                       file = "PFTC6_leaf_area_2022.csv",
                       path = "raw_data/traits",
                       remote_path = "raw_data/trait_raw_data"),
    format = "file"
  ),

  tar_target(
    name = leaf_area_corrected_download,
    command = get_file(node = "fcbw4",
                       file = "PFTC6_leaf_area_corrected_2022.csv",
                       path = "raw_data/traits",
                       remote_path = "raw_data/trait_raw_data"),
    format = "file"
  ),

  tar_target(
    name = leaf_area_corrected_2_download,
    command = get_file(node = "fcbw4",
                       file = "PFTC6_leaf_area_corrected_2_2022.csv",
                       path = "raw_data/traits",
                       remote_path = "raw_data/trait_raw_data"),
    format = "file"
  ),

  tar_target(
    name = leaf_area_cropped_download,
    command = get_file(node = "fcbw4",
                       file = "PFTC6_leaf_area_cropping_2022.csv",
                       path = "raw_data/traits",
                       remote_path = "raw_data/trait_raw_data"),
    format = "file"
  ),

  # leaf traits
  tar_target(
    name = comments_download,
    command = get_file(node = "fcbw4",
                       file = "comments_trait_data.csv",
                       path = "raw_data/traits",
                       remote_path = "raw_data/trait_raw_data"),
    format = "file"
  ),

  tar_target(
    name = traits_download,
    command = get_file(node = "fcbw4",
                       file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
                       path = "raw_data/traits/",
                       remote_path = "raw_data/trait_raw_data"),
    format = "file"
  ),


  # import data
  # dry mass
  tar_target(
    name = raw_dry_mass,
    command = read_excel(path = traits_download, sheet = "DryMass")
  ),

  # leaf area
  tar_target(
    name = raw_leaf_area,
    command = read_csv(file = leaf_area_download)
  ),

  tar_target(
    name = raw_leaf_area_corrected,
    command = read_csv(file = leaf_area_corrected_download) |>
      # remove corrections for herbivory
      filter(!grepl("Coskun", dir))
  ),

  tar_target(
    name = raw_leaf_area_corrected_2,
    command = read_csv(file = leaf_area_corrected_2_download) |>
      # remove corrections for herbivory
      filter(!grepl("Coskun", dir))
  ),

  tar_target(
    name = raw_leaf_area_cropped,
    command = read_csv(file = leaf_area_cropped_download) |>
      # remove scans that also needed painting
      filter(!ID %in% c("AEE2091_edited", "AFT5418_edited", "ANN5578_edited", "CRU9872_edited", "DYL5087_edited"))
  ),

  tar_target(
    name = scanning_checks,
    command = read_excel(path = traits_download, sheet = "ScanningChecks")
  ),


  # leaf traits
  # comments about scanning
  tar_target(
    name = traits_comments,
    command = read_csv(file = comments_download)
  ),


  tar_target(
    name = raw_traits,
    command = read_excel(path = traits_download, sheet = "Data", na = c("", "NA"))
  ),

  # clean data
  # dry mass
  tar_target(
    name = dry_mass_clean,
    command = clean_dry_mass(raw_dry_mass)
  ),

  # leaf area
  tar_target(
    name = leaf_area_clean,
    command = clean_leaf_area(raw_leaf_area, raw_leaf_area_corrected, raw_leaf_area_corrected_2, raw_leaf_area_cropped, scanning_checks)
  ),

  # leaf traits
  tar_target(
    name = leaf_traits_clean_full,
    command = clean_traits(raw_traits, traits_comments, dry_mass_clean, leaf_area_clean)
  ),

  tar_target(
    name = leaf_traits_clean,
    command = clean_traits_pretty(leaf_traits_clean_full)
  ),

  # Make 3D data
  tar_target(
    name = leaf_traits_3D,
    command = leaf_traits_clean |>
      filter(project == "3D") |>
      select(ID, date, gradient, siteID, elevation_m_asl, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, individual_nr, species, trait, value, origSiteID, destSiteID, comment, problem, flag)
  ),

  tar_target(
    name = leaf_traits_3D_output,
    command = save_csv(leaf_traits_3D,
                       name = "PFTC6_ThreeD_clean_leaf_traits_2022.csv"),
    format = "file"
  ),

  # Make Incline data
  tar_target(
    name = leaf_traits_Incline,
    command = leaf_traits_clean |>
      filter(project == "Incline") |>
      select(ID, date, siteID, elevation_m_asl, blockID, warming, individual_nr, species, trait, value, flowering, comment, problem, flag)
  ),

  tar_target(
    name = leaf_traits_Incline_output,
    command =  save_csv(leaf_traits_Incline,
                        name = "PFTC6_Incline_clean_leaf_traits_2022.csv"),
    format = "file"
  )

)
