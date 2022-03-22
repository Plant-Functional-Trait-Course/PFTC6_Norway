# Make data dictionaries

# load libraries
source(file = "R/load_packages.R")

# load clean data
source("R/data_dic/download_clean_data.R")

# data dictionary function
source("R/data_dic/make_data_dictionary.R")

# read in data description table
description_table <- read_excel("R/data_dic/data_description.xlsx") %>%
  mutate(TableID = as.character(TableID))

#************************************************************************
#************************************************************************
### 1 BIOMASS

biomass <- read_csv("data/biomass/FunCaB_clean_biomass_2015-2021.csv")


biomass_dic <- make_data_dictionary(data = biomass,
                                      description_table = description_table,
                                      table_ID = NA_character_) %>%
  mutate(`Variable range or levels` = if_else(`Variable name` == "remark", NA_character_, `Variable range or levels`))


#************************************************************************
### 2 SPECIES BIOMASS

biomass_sp <- read_csv("data/biomass/FunCaB_clean_species_biomass_2016.csv")


biomass_sp_dic <- make_data_dictionary(data = biomass_sp,
                                    description_table = description_table,
                                    table_ID = NA_character_)


#************************************************************************
####3 MICROCLIMATE DATA ####
# Temperature
soil_temperature <- read_csv(file = "data/climate/FunCaB_clean_soiltemperature_2015-2016.csv")

soil_temperature_dic <- make_data_dictionary(data = soil_temperature,
                                             description_table = description_table,
                                             table_ID = NA_character_) %>%
  mutate(`Variable range or levels` = if_else(`Variable name` == "comments", NA_character_, `Variable range or levels`))



# Soilmoisture
soilmoisture <- read_csv("data/climate/FunCaB_clean_soilMoisture_2015-2019.csv")

soilmoisture_dic <- make_data_dictionary(data = soilmoisture,
                                         description_table = description_table,
                                         table_ID = NA_character_)

#************************************************************************

### 5 COMMUNITY DATA

community <- read_csv("data/community/FunCaB_clean_composition_2015-2019.csv")


community_dic <- make_data_dictionary(data = community,
                                      description_table = description_table,
                                      table_ID = NA_character_) %>%
  mutate(`Variable range or levels` = if_else(`Variable name` == "species", "Ach.mil - Vio.tri or NA", `Variable range or levels`))


#************************************************************************

#### 6 SEEDLING RECRUITMENT DATA ####


recruitment <- read_csv("data/recruitment/FunCaB_clean_recruitment_2018-2019.csv")


recruitment_dic <- make_data_dictionary(data = recruitment,
                                      description_table = description_table,
                                      table_ID = "recruitment")



#************************************************************************

#### 7 CARBON FLUX DATA ####

cflux <- read_csv("data/cflux/FunCaB_clean_Cflux_2015-2017.csv")

cflux_dic <- make_data_dictionary(data = cflux,
                                  description_table = description_table,
                                  table_ID = "cflux") %>%
  mutate(`Variable range or levels` = if_else(`Variable name` %in% c("comment", "flag"), NA_character_, `Variable range or levels`))


#************************************************************************

#### 8 REFLECTANCE ####

reflectance <- read_csv("data/reflectance/FunCaB_clean_reflectance_2019_2021.csv")

reflectance_dic <- make_data_dictionary(data = reflectance,
                                  description_table = description_table,
                                  table_ID = "reflectance") %>%
  mutate(`Variable range or levels` = if_else(`Variable name` == "time", "08:00 - 19:55", `Variable range or levels`),
         `Variable type` = if_else(`Variable name` == "time", "time", `Variable type`)) %>%
  mutate(`Variable range or levels` = if_else(`Variable name` == "notes", NA_character_, `Variable range or levels`))



#************************************************************************

##merge all dics together to one xlsx, with each parameter as a single sheet

write_xlsx(list(biomass_removal = biomass_dic,
                biomass_sp = biomass_sp_dic,
                soil_temperature = soil_temperature_dic,
                soil_moisture = soilmoisture_dic,
                plant_community = community_dic,
                seedling_recruitment = recruitment_dic,
                cflux = cflux_dic,
                reflectance = reflectance_dic),
           path = "R/data_dic/data_dictionary.xlsx")



