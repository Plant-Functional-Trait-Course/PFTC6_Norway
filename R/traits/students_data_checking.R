####################################
#### CODE TO CHECK SPREADSHEETS ####
####################################

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(validate)
library(PFTCFunctions)
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


#### Read in spreadsheet ####

# download raw trait data from OSF
get_file(node = "pk4bg",
         file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
         path = "raw_data/traits/",
         remote_path = "RawData/Traits")

raw_traits <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "Data")


### get all valid IDs (seed for PFTC6 is 49)
uniqueIDs <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)


# Set rules
# rules
rules <- validator(

  # check variable types
  is.character(ID),

  is.numeric(individual_nr),

  # matching lists
  ID %in% c(leaf_ID$hashcode) # unique ID
)

out <- confront(raw_traits, rules)
summary(out)

plot(out)




# plot data

ggplot(raw_traits, aes(x = wet_mass_g, y = leaf_area)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() +
  scale_y_log10()


ggplot(raw_traits, aes(x = leaf_thickness_1_mm, y = leaf_thickness_3_mm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() +
  scale_y_log10()
