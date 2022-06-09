library("baRcodeR")
library("PFTCFunctions")
library("tidyverse")
library(readxl)

# get all codes for Peru
#all_codes <- get_PFTC_envelope_codes(seed = 1)

load("R/Rdatagathering/envelope_codes.Rdata", verbose = TRUE)

# load cleaned trait data
#traits_2018_Peru_cleaned <- read_csv(file = "traits/data/PFTC3.7_Traits_2018_Peru_cleaned.csv", col_names = TRUE)

# anti_join
# unusedIDs <- all_codes %>%
#   anti_join(traits_2018_Peru_cleaned, by = c("hashcode" = "ID")) %>%
#   slice(1:4000) %>%
#   as_tibble()
# #write.csv(unusedIDs, file = "LeafIDs_Peru_2020.csv", row.names = FALSE)
#
#
# # only for now...
# unusedIDs2 <- unusedIDs[1:4000,] %>% as.data.frame()

# Function to make and print labels on PDF
# The magic combination for these lables: Avery 4778
#https://www.lyreco.com/webshop/NONO/etiketter-avery-45-7-x-21-2-mm-hvit-eske-c3a0-960-stk-product-000000000002760191.html
custom_create_PDF(Labels = all_codes$hashcode, name = "Norway_2022_myLabels",
                  type = "linear", Fsz = 14, Across = TRUE,
                  trunc = TRUE, numrow = 12, numcol = 4,
                  page_width = 8.3, page_height = 11.7, width_margin = 0.2,
                  height_margin = 0.7, label_width = 1.811, label_height = 0.5)



# Barcodes for PFTC5 Peru
all_codes <- get_PFTC_envelope_codes(seed = 6)

# check if they overlap with pftc3
PFTC3 <- get_PFTC_envelope_codes(seed = 1)
all_codes %>% inner_join(PFTC3) # no overlap

all_codes <- all_codes %>% slice(1:4800)
#all_codes <- all_codes %>% slice(1:48)
custom_create_PDF(Labels = all_codes$hashcode, name = "traits/Peru2020_myLabels",
                  type = "linear", Fsz = 14, Across = TRUE,
                  trunc = TRUE, numrow = 12, numcol = 4,
                  page_width = 8.3, page_height = 11.7, width_margin = 0.2,
                  height_margin = 0.67, label_width = 1.811, label_height = 0.5)
