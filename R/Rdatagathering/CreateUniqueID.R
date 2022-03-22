library("tidyverse")
library("writexl")

source("R/Rdatagathering/envelope_codes.R")

# Make List with ID
dat <- all_codes %>%
  select(hashcode) %>%
  rename(ID = hashcode)
write_xlsx(dat, path = "UniqueID_PFTC6_Norway.xlsx", col_names = TRUE)

