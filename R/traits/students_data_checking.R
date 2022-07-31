####################################
#### CODE TO CHECK SPREADSHEETS ####
####################################

# Load libraries
library(tidyverse)
library(lubridate)
library(validate)
library(PFTCFunctions)
library(googlesheets4)


#### Read in spreadsheet ####
### get all valid IDs (seed for PFTC6 is 49)
uniqueIDs <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)

### Read google sheet
#Install the package if you haven't
#install.packages("googlesheets4")

#Read google sheets data into R
gs4_deauth()
raw_traits <- read_sheet("https://docs.google.com/spreadsheets/d/1ncqbniu0NUzCfcNe2fOl6M2Yj-BeOEXcYlPZHwNiIbk/edit#gid=0")


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
