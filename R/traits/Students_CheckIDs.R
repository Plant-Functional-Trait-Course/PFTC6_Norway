####################################
#### CODE TO CHECK SPREADSHEETS ####
####################################

#### Source to load libraries, data lists and functions ####
source("traits/Rdatagathering/CheckSpreadsheet.R")


#### Read in spreadsheet ####
### CHANGE PATH TO THE SPREADSHEET !!!
traits <- read_excel(path = "traits/data/TraitSpreadsheet_Template.xlsx")


# Check spreadsheet
CheckSpreadsheet(dat = traits)


# Draw some plots
MakeSomePlots(traits)

