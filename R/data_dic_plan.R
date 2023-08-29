# data dictionary plan

data_dic_plan <- list(

  # attribute table
  tar_target(
    name = description_table_download,
    command = get_file(node = "fcbw4",
                       file = "PFTC6_data_description.xlsx",
                       path = "raw_data",
                       remote_path = "raw_data/trait_raw_data"),
    format = "file"
  ),

  tar_target(
    name = description_table,
    command = read_excel(path = description_table_download) |>
      mutate(TableID = as.character(TableID))
  ),


  # traits 3D
  tar_target(
    name = trait_dic_3D,
    command = make_data_dictionary(data = leaf_traits_3D,
                                   description_table = description_table,
                                   table_ID = "ThreeD")
  ),

  # traits Incline
  tar_target(
    name = trait_dic_I,
    command = make_data_dictionary(data = leaf_traits_Incline,
                                   description_table = description_table,
                                   table_ID = "Incline")
  ),

  # merge data dictionaries
  tar_target(
    name = data_dic,
    command = write_xlsx(list(trait_3D = trait_dic_3D,
                              trait_I = trait_dic_I),
    path = "clean_data/PFTC6_data_dictionary.xlsx")),
    format = "file"

)
