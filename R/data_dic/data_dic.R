# Make data dictionaries

# load libraries
library(tidyverse)
library(readxl)
library(writexl)

# load clean data
source("R/data_dic/download_clean_data.R")

# data dictionary function
source("R/data_dic/make_data_dictionary.R")

# read in data description table
description_table <- read_excel("R/data_dic/data_description.xlsx") %>%
  mutate(TableID = as.character(TableID))

#************************************************************************
#************************************************************************
### 1 Dataset 1

data_1 <- read_csv("clean_data/...")


data_1_dic <- make_data_dictionary(data = data_1,
                                      description_table = description_table,
                                      table_ID = NA_character_)


#************************************************************************
### 2 Dataset 2

data_2 <- read_csv("clean_data/...")


data_2_dic <- make_data_dictionary(data = data_2,
                                    description_table = description_table,
                                    table_ID = NA_character_)


#************************************************************************

### Add all data sets


#************************************************************************

##merge all dics together to one xlsx, with each parameter as a single sheet

write_xlsx(list(data_1 = data_1_dic,
                data_2 = data_2_dic,
                ...
                ),
           path = "R/data_dic/data_dictionary.xlsx")



