# Make data dictionaries

# load libraries
library(tidyverse)
library(readxl)
library(writexl)

# load clean data
source("R/data_dic/download_clean_data.R")


# read in data description table
description_table <- read_excel("R/data_dic/data_description.xlsx") %>%
  mutate(TableID = as.character(TableID))

#************************************************************************
#************************************************************************
### Traits

incline_traits <- read_csv("clean_data/PFTC6_Incline_clean_leaf_traits_2022.csv")


trait_dic_I <- make_data_dictionary(data = incline_traits,
                                   description_table = description_table,
                                   table_ID = "Incline")


threeD_traits <- read_csv("clean_data/PFTC6_ThreeD_clean_leaf_traits_2022.csv")

trait_dic_3D <- make_data_dictionary(data = threeD_traits,
                                   description_table = description_table,
                                   table_ID = "ThreeD")

#************************************************************************

##merge all dics together to one xlsx, with each parameter as a single sheet

write_xlsx(list(trait_I = trait_dic_I,
                trait_3D = trait_dic_3D),
           path = "R/data_dic/data_dictionary.xlsx")



