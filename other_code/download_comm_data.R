# download relevant data on community, climate and traits from PFTC6 and VCG projects
# This code only downloads the data from specific repositories, but does not import them.

# vegetation
# VCG plant community composition
# this is a ziped file and needs unzipping before use
get_file(node = "npfa9",
         file = "seedclim.2020.4.15.zip",
         path = "clean_data",
         remote_path = "3_Community_data")

# ThreeD plant community composition
get_file(node = "pk4bg",
         file = "Three-D_clean_cover_2019-2022.csv",
         path = "clean_data",
         remote_path = "Vegetation")

# INCLINE plant community composition
get_file(node = "zhk3m",
         file = "INCLINE_community_species_cover_fixed.csv",
         path = "clean_data",
         remote_path = "Community")

# climate
# VCG climate data
get_file(node = "npfa9",
         file = "VCG_clean_temperature_2009-2022.csv",
         path = "clean_data",
         remote_path = "8_Environmental_data")

# PFTC6 traits
# ThreeD traits
get_file(node = "fcbw4",
         file = "PFTC6_ThreeD_clean_leaf_traits_2022.csv",
         path = "clean_data",
         remote_path = "i_trait_data")

# Incline traits
get_file(node = "fcbw4",
         file = "PFTC6_Incline_clean_leaf_traits_2022.csv",
         path = "clean_data",
         remote_path = "i_trait_data")
