# DRY MASS AND LEAF AREA

# import dra mass data
raw_dry_mass <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "DryMass")

dry_mass <- raw_dry_mass |>
  # remove duplicates
  distinct()

# visualize
#dry_mass |> ggplot(aes(dry_mass)) + geom_histogram()



# leaf area
raw_leaf_area <- read_csv("raw_data/traits/leaf_area.csv")
scanning_checks <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "ScanningChecks")
# NEED TO IMPORT RESCANNED LEAF AREAS !!!

