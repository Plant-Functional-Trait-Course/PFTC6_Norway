# download clean data
# community, climate
# PFTC6 datasets traits, and fluxes
# This code only downloads the data from specific repositories, but does not import them.

download_clean_data_plan <- list(

  # vegetation
  # VCG plant community composition
  tar_target(
    name = vcg_community,
    command =  get_file(node = "",
                        file = "seedclim.sqlite",
                        path = "clean_data",
                        remote_path = "..."),
    format = "file"
  ),

  # ThreeD plant community composition
  tar_target(
    name = threeD_community,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_cover_2019-2022.csv",
                        path = "clean_data",
                        remote_path = "Vegetation"),
    format = "file"
  ),

  # Incline plant community composition
  tar_target(
    name = incline_community,
    command =  get_file(node = "pk4bg",
                        file = "...",
                        path = "clean_data",
                        remote_path = "..."),
    format = "file"
  ),

  # clinate
  # VCG climate data
  tar_target(
    name = vcg_climate,
    command =  get_file(node = "",
                        file = "",
                        path = "clean_data",
                        remote_path = "..."),
    format = "file"
  )

  # traits
  # ThreeD traits
  # tar_target(
  #   name = threed_traits,
  #   command =  get_file(node = "fcbw4",
  #                       file = "PFTC6_ThreeD_clean_leaf_traits_2022.csv",
  #                       path = "data",
  #                       remote_path = "trait_data"),
  #   format = "file"
  # ),

  # Incline traits
  # tar_target(
  #   name = incline_traits,
  #   command =  get_file(node = "fcbw4",
  #                       file = "PFTC6_Incline_clean_leaf_traits_2022.csv",
  #                       path = "data",
  #                       remote_path = "trait_data"),
  #   format = "file"
  # ),

)
