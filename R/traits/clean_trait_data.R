### Clean trait data

### Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(validate)
#library(PFTCFunctions)
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")
#devtools::install_version("TNRS")
library(TNRS) # match taxa names
#remotes::install_github("audhalbritter/dataDocumentation")
library(dataDocumentation)

# download raw trait data from OSF
get_file(node = "pk4bg",
         file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
         path = "raw_data/traits/",
         remote_path = "RawData/Traits")


# import data

raw_traits <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "Data")

raw_dry_mass <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "DryMass")

#raw_leaf_area <- read.csv("raw_data/traits/leaf_area.csv")


### Data cleaning ----

# Remove rows with just NA
clean_traits <- raw_traits %>%
  filter(if_any(everything(), ~ !is.na(.)))

# Remove Seans data
clean_traits <- clean_traits %>%
  filter(is.na(project)|project!="Sean") #

# Current combinations of experiment, siteID and day
clean_traits %>%
  mutate(siteID = if_else(siteID == "vik", "Vik", siteID)) %>%
  select(siteID,experiment,day,project,elevation_m_asl) %>%
  unique()

# Incline sites are Ulv, Gud, Skj
# SO thats an easy correction if we assume siteID is correct?
# It is not always correct
# But luckily - only Incline added elevation to their write up?
# SO convert based on this first

clean_traits <- clean_traits %>%
  mutate(siteID = if_else(siteID == "vik", "Vik", siteID),
         siteID = if_else(project == "Incline" & siteID == "Hog", "Ulv", siteID))

# Fix days, month, year, date
# Sort out project and siteID
# Add elevation
# Fix experiment column when obviously wrong
# Clean leaf thickness data which is wrong

clean_traits2 <- clean_traits %>%
  mutate(day = case_when( siteID == "Ulv" & project == "Incline" ~ 24,
                          siteID == "Hog" & project == "3D" ~ 24,
                          siteID == "Vik" & project == "3D" ~ 26,
                          siteID == "Gud" & project == "Incline" ~ 27,
                          siteID == "Lia" & project == "3D" ~ 28,
                          siteID == "Skj" & project == "Incline" ~ 30,
                          siteID == "Joa" & project == "3D" ~ 1,
                          TRUE ~ day),
         siteID = case_when(day == 24 & project == "Incline" ~ "Ulv",
                            day == 24 & project == "3D" ~ "Hog",
                            day == 26 & project == "3D" ~ "Vik",
                            day == 27 & project == "Incline" ~ "Gud",
                            day == 28 & project == "3D" ~ "Lia",
                            day == 30 & project == "Incline" ~ "Skj",
                            day == 1 & project == "3D" ~ "Joa",
                            TRUE ~ siteID),
         project = case_when(day == 24 & siteID == "Ulv" ~ "Incline",
                             day == 24 & siteID == "Hog" ~ "3D",
                             day == 26 & siteID == "Vik" ~ "3D",
                             day == 27 & siteID == "Gud" ~ "Incline",
                             day == 28 & siteID == "Lia" ~ "3D",
                             day == 30 & siteID == "Skj" ~ "Incline",
                             day == 1 & siteID == "Joa" ~ "3D",
                             TRUE ~ project),
         year = 2022,
         month = if_else(day == 1, 8, 7),
         date = make_date(year, month, day),
         elevation_m_asl = case_when(siteID == "Ulv" ~  1208,
                                     siteID == "Hog" ~ 700,
                                     siteID == "Vik" ~ 469,
                                     siteID == "Gud" ~ 1213,
                                     siteID == "Lia" ~ 1290,
                                     siteID == "Skj" ~ 1088,
                                     siteID == "Joa" ~ 920),
         experiment = ifelse(experiment == "NA", NA_character_, experiment),
         leaf_thickness_1_mm = if_else(ID == "IKY0250", 0.207, leaf_thickness_1_mm), # fix unit errors
         leaf_thickness_1_mm = if_else(ID == "DEV8302", 0.155, leaf_thickness_1_mm),
         leaf_thickness_2_mm = if_else(ID == "CZW4480", "0.153", leaf_thickness_2_mm), # make correct string prior to conversion
         leaf_thickness_2_mm = if_else(ID == "DDI9716", "0.223", leaf_thickness_2_mm),
         leaf_thickness_2_mm = if_else(ID == "DEX5838", "0.185", leaf_thickness_2_mm),
         leaf_thickness_3_mm = if_else(ID == "CHV2350", 0.198, leaf_thickness_3_mm),
         leaf_thickness_2_mm = as.numeric(leaf_thickness_2_mm),
         plant_height = ifelse(project == "Incline", plant_height/10,plant_height), # All incline plants were measured in mm, so convert to cm
         plant_height = ifelse(plant_height > 59, plant_height/10, plant_height)) %>% # fixing obviously missed decimals
  filter(wet_mass_g < 10) # Remove two impossible wet mass values

# Fix day and project which didn't change
# Remove Joa which must be Seans as comment is rest

clean_traits2 <- clean_traits2 %>%
  mutate(day = ifelse(ID == "EDH3100", 27, day),
         project = ifelse(ID == "EDH3100", "Incline", project)) %>%
  filter(ID != "CGE9632")

# Check experiments column
# Experiment should all be C or OTC for Incline
clean_traits2 %>%
  select(siteID,experiment,day,project,elevation_m_asl) %>%
  unique()

# Have 4 entries without/ wrong experiment for incline
# And missing site IDs

missing_incline_exp <- clean_traits2 %>%
  filter((siteID == "Ulv" | siteID == "Gud" |siteID == "Skj" )) %>%
  filter(is.na(experiment) | experiment =="N")

missing_siteID <-clean_traits2 %>%
  filter(is.na(siteID))


# Check for the incline experiments which are missing
clean_traits2 %>%
  filter( siteID == "Gud") %>%
  filter(taxon == "Campanula rotundifolia") %>%
  group_by(siteID,taxon,plotID,experiment,individual_nr) %>%
  tally()

# This is likely OTC as OTC in plot 3.0 is also missing individual nr 1 - which is should have, so change both later on
clean_traits2 %>%
  filter( siteID == "Ulv") %>%
  filter(taxon == "Anthoxanthum odoratum") %>%
  group_by(siteID,taxon,plotID,experiment,individual_nr) %>%
  tally()

# Don't think this next one can be inferred from individual number - could be either
clean_traits2 %>%
  filter( siteID == "Skj") %>%
  filter(taxon == "Bistorta vivipara") %>%
  group_by(siteID,taxon,plotID,experiment,individual_nr) %>%
  tally()

# This next one has to be OTC
clean_traits2 %>%
  filter( siteID == "Skj") %>%
  filter(taxon == "Thalictrum alpinum") %>%
  group_by(siteID,taxon,plotID,experiment,individual_nr) %>%
  tally()

# Now Site IDs

# This one has to be 3D
clean_traits2 %>%
  filter(day == 24) %>%
  filter(taxon == "Carex vaginata") %>%
  group_by(siteID,taxon,plotID,experiment,individual_nr,project) %>%
  tally()

# Fix what the experiments and Site IDs likely are

clean_traits2 <- clean_traits2 %>%
  mutate(experiment = ifelse(ID == "DJD3630", "C", experiment),
         experiment = ifelse(ID == "AGP9286", "OTC", experiment),
         individual_nr = ifelse(ID == "AGP9286", 1, individual_nr),
         experiment = ifelse(ID == "GOX6736", "OTC", experiment),
         siteID = ifelse(ID == "CEW4486", "Hog", siteID),
         project = ifelse(ID == "CEW4486", "3D", project),
         elevation_m_asl = ifelse(ID == "CEW4486", 700, elevation_m_asl),
         siteID = ifelse(ID == "APJ1597", "Hog", siteID),
         project = ifelse(ID == "APJ1597", "3D", project),
         elevation_m_asl = ifelse(ID == "APJ1597", 700, elevation_m_asl),
         siteID = ifelse(ID == "BNK8495" & project =="Incline", "Ulv", siteID),
         elevation_m_asl = ifelse(ID == "BNK8495" & project =="Incline", 1208, elevation_m_asl),
         day = ifelse(ID == "BNK8495" & project =="Incline", 24, day))

# Now check for missing plot IDs/Individual Number
unique(clean_traits2$plotID)
# Make plot ID which is "NA" NA
# N or C needs to have a number ID
# if not should have a code
# incline should be numbers

clean_traits2 <- clean_traits2 %>%
  mutate(plotID = case_when(plotID =="B2" ~ "2.0",
                            plotID =="B3" ~ "3.0",
                            plotID =="BL5" ~ "5.0",
                            plotID =="NA"~ NA_character_,
                            TRUE ~ plotID))

clean_traits2 %>%
  filter(is.na(plotID))
# Comments have the plot ID for some of them - incorporate these and cross reference with Aud's metadata package
# to make sure they're correct - as some are missing info

# Use dataDocumentation package
meta_data_3D <- create_threed_meta_data()
# this can correct some of the plotID issues

clean_traits2 <- clean_traits2 %>%
  mutate(plotID = case_when(remark == "1 85 WN1C 167" ~ "1-85 WN1C 162", # corrected to metadata
                            remark == "5-57AN8C 57" ~ "8-57AN8C 57", # says 5-57 but no 5-57 in metadata so changed to 8
                            remark == "Plot ID on envelope is 10-75-AN3N-79" ~ "10-79 AN2N 79", # has to be this one
                            remark == "Plot ID: WN3CN 112" ~ "4-32 WN3N 112",
                            remark == "Plot ID: 7-109AN3C 109" ~ "4-109AN3C 109", # corrected number
                            remark == "Plot ID: 4-105AN3C 109" ~ "4-109AN3C 109",
                            TRUE ~ plotID),
         plotID = ifelse(ID == "CIO0085","2.0",plotID))

# Think these 5 can't be corrected without looking at the envelopes
clean_traits2 %>%
  filter(is.na(plotID))

# Individual numbers
miss_ind_nr <- clean_traits2 %>%
  filter(is.na(individual_nr))

# Correct where Joshua noted

clean_traits2 <- clean_traits2 %>%
  mutate(individual_nr = ifelse(ID == "EJR3255",2,individual_nr),
         individual_nr = ifelse(ID == "DSE5104",3,individual_nr))

# Way to find them could be to group them by plot etc. and see what's missing
check_ind_nr <- clean_traits2 %>%
  filter(siteID %in% miss_ind_nr$siteID) %>%
  filter(plotID %in% miss_ind_nr$plotID) %>%
  filter(taxon %in% miss_ind_nr$taxon) %>%
  arrange(siteID,taxon,plotID,individual_nr)
# honestly not sure how important having these is though - I think some were missed from first day of 3D work in Hog


# Can use Aud's package for precise experiment info for 3D when needed

### Clean duplicates ----

# Check for duplicate barcodes and make sure data is different

duplicated_IDs <- clean_traits2[duplicated(clean_traits2$ID), ]

duplicate_subset <- clean_traits2[clean_traits2$ID %in% duplicated_IDs$ID, ]

# Code to check if they are true duplicates

duplicate_subset <- duplicate_subset %>%
  group_by(ID) %>%
  mutate(true_dupe_wet_mass = as.integer(n_distinct(wet_mass_g) == 1),
         true_dupe_wet_mass = ifelse(true_dupe_wet_mass==1, "Yes","No"))

# Remove true duplicates then manually check if the entries have two leaves in the leaf scan files
# (need to remove only one of these duplicates later - using slice)

duplicate_subset_check <- duplicate_subset %>%
  filter(true_dupe_wet_mass != "Yes")

unique(duplicate_subset_check$ID)

# AQK5961 - only one leaf scan - looks like correct species (grass anyway)
# CTQ9841 - only one leaf scan (is a grass)
# BNN7822 - One leaf scan in 2022/07/27 folder - looks like Potentilla erecta
#  Other leaf scan in 22/07/25 they are the same scan
# But duplicate in data is Agrostis... - check envelope - agrostis got the wrong code?
# BNK8495 - only one leaf scan - looks like Potentilla erecta not leontodon as other dupe says
# Two ACM3709 - both are the same scan, is Alchemilla alpina not Nardus as other dupe says
# FUY4409 - one scan is Alchemilla alpina - not Bistorta vivipara as the other dupe says
# HRT6861 - one scan is Trifolium repens not leontodon
# IGM2553 - one scan looks like Anthoxanthum not Avenella

# So two of these are the same species with different measurements, the rest have wrong species
# associated with the scan


# Remove all duplicates from main dataset - to let others be re-added
clean_traits3 <- clean_traits2 %>%
  filter(!ID %in% duplicate_subset$ID)

# Remove one row from the duplicates so they are unique
# This deals with the true duplicates

duplicate_subset <- duplicate_subset %>%
  unique() %>%
  filter(leaf_thickness_3_mm!=0.281)# because this value differs slightly and messes with unique

# Rebind the two together

clean_traits3 <- bind_rows(clean_traits3,duplicate_subset)

# so the duplicate data has been removed but some different plants with the same ID remain

### Checking for duplicates in dry mass

# Download from google doc
library(googlesheets4)
dry_mass_so_far <-  read_sheet("https://docs.google.com/spreadsheets/d/1ncqbniu0NUzCfcNe2fOl6M2Yj-BeOEXcYlPZHwNiIbk/edit#gid=410051704", sheet=2)

str(dry_mass_so_far)

dry_mass_so_far[duplicated(dry_mass_so_far$ID), ]
# One duplicate entry so far - seems to be a true duplicate

## Dataframe with all envelopes to check...

duplicate_subset2 <- duplicate_subset %>%
  filter(true_dupe_wet_mass !="Yes") %>%
  mutate(problem = "duplicate_ID")

miss_plotID <- clean_traits2 %>%
  filter(is.na(plotID)) %>%
  mutate(problem = "miss_plotID")

miss_expt_incline <- clean_traits2 %>%
  filter((siteID == "Ulv" | siteID == "Gud" |siteID == "Skj" )) %>%
  filter(is.na(experiment) | experiment =="N")

miss_expt_incline <- miss_expt_incline %>%
  mutate(problem = "miss_experiment")

data_to_check <- rbind(duplicate_subset2,miss_plotID,miss_expt_incline)
write.csv(data_to_check,"PTFC_envelopes_to_check.csv")

### Clean taxa names ----

# first get in data from new_taxon col

clean_traits3$taxon <- ifelse(is.na(clean_traits3$taxon), clean_traits3$new_taxon, clean_traits3$taxon)
species <- unique(clean_traits3$taxon)
species <- TNRS(species)

# Now fix names

clean_traits3 <- clean_traits3 %>%
  mutate(remark = ifelse(taxon == "Festuca officinalis","was F. officinalis, changed to F. ovina, should most likely be correct",remark)) %>%
  mutate(taxon=str_replace_all(taxon, c("Salix herbaceae"="Salix herbacea",
                                        "Astragulus alpinus"="Astragalus alpinus",
                                        "Oxyna diggna"="Oxyria digyna",
                                        "Alchemilla spp"="Alchemilla sp",
                                        "Achemilla sp"="Alchemilla sp",
                                        "Achemilla sp."="Alchemilla sp",
                                        "Alchemilla sp."="Alchemilla sp",
                                        "Carex sp."="Carex sp",
                                        "Festuca officinalis"="Festuca ovina", # most likely F. ovina
                                        "Astralagulus sp."="Astragalus alpinus",
                                        "Geranium sylvatica"="Geranium sylvaticum"
  ))) %>%
  select(-new_taxon)

####### join leaf area and dry mass data

clean_traits3 <- left_join(clean_traits3, raw_leaf_area, by = "ID")

# Fix leaf area columns
clean_traits3 <- clean_traits3 %>%
  select(-X) %>%
  rename(number_leaf_fragments_scanned = n,
         wet_mass_total_g = wet_mass_g,
         leaf_area_total_cm2 = leaf_area,
         nr_leaves = bulk_nr_leaves) %>%
  mutate(nr_leaves = ifelse(is.na(nr_leaves) & !is.na(leaf_thickness_1_mm), 1, nr_leaves)) %>%
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate average leaf thickness
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / nr_leaves,
         leaf_area_cm2 = leaf_area_total_cm2 / nr_leaves) %>%

  # # Wet and dry mass do not make sense for these species
  # mutate(dry_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, dry_mass_g),
  #        wet_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, wet_mass_g),
  #        leaf_area_cm2 = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, leaf_area_cm2)) |>

  # Calculate SLA and LDMC (replace with wet mass for now)
  mutate(sla_cm2_g = leaf_area_cm2 / wet_mass_g)

# Some scans not there

#_______________________________________________________________________________
# NA removal and outlier detection by Joshua

clean_traits3$false <- clean_traits3$plant_height/clean_traits3$wet_mass_g # a ratio of 1 or 0.1 points to a mistaken height value
# height values drawn from Incline group fieldwork sheets
clean_traits3[clean_traits3$ID == "BOU0176", "plant_height"] <- 8.1
clean_traits3[clean_traits3$ID == "BDQ5475", "plant_height"] <- 5.3
clean_traits3[clean_traits3$ID == "EDH3100", "plant_height"] <- 14.6

# bulk number of leaves counted from leaf scans
clean_traits3[clean_traits3$ID == "CHW9026", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "BHS3927", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "AQA6446", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "AIT4560", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "AIX4648", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "APO5292", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "AJB8204", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "CBX5036", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "APQ8072", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "BQG7481", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "BFI7437", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "CDE0818", "bulk_nr_leaves"] <- 2
clean_traits3[clean_traits3$ID == "CON9328", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "DAK3110", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "CZY5489", "bulk_nr_leaves"] <- 3
clean_traits3[clean_traits3$ID == "GPW1350", "bulk_nr_leaves"] <- 1
clean_traits3[clean_traits3$ID == "AQK5961", "bulk_nr_leaves"] <- 1

# there is a note that mistakenly says that leaf thickness should be 0.0325
clean_traits3[clean_traits3$ID == "HWK3847", "leaf_thickness_3_mm"] <- 0.325

# lines of code that generate the variance of leaf thickness values to find outliers
thickness <-clean_traits3 %>% select(c(ID,leaf_thickness_1_mm,leaf_thickness_2_mm,leaf_thickness_3_mm)) %>%
  pivot_longer(cols = c(leaf_thickness_1_mm,leaf_thickness_2_mm,leaf_thickness_3_mm), names_to = "measurement", values_to = "value")
thickness <- aggregate(log(thickness$value), list(thickness$ID), FUN = var)
thickness <- rename(thickness, ID = Group.1, thickness_var = x)
clean_traits3 <- left_join(clean_traits3, thickness, by = "ID")

# outlier values of ~ 1 magnitude have been changed to better reflect other measured values of the sample
clean_traits3[clean_traits3$ID == "ICR7173", "leaf_thickness_2_mm"] <- 0.091
clean_traits3[clean_traits3$ID == "FVU2190", "leaf_thickness_2_mm"] <- 0.038
clean_traits3[clean_traits3$ID == "ESD7613", "leaf_thickness_1_mm"] <- 0.243
clean_traits3[clean_traits3$ID == "ESV6901", "leaf_thickness_1_mm"] <- 0.247
clean_traits3[clean_traits3$ID == "CJN0746", "leaf_thickness_2_mm"] <- 1.000
clean_traits3[clean_traits3$ID == "CWC6486", "leaf_thickness_1_mm"] <- 0.265
clean_traits3[clean_traits3$ID == "CCO1039", "leaf_thickness_1_mm"] <- 1.000
clean_traits3[clean_traits3$ID == "EPR5076", "leaf_thickness_1_mm"] <- 0.236
clean_traits3[clean_traits3$ID == "CXJ5628", "leaf_thickness_1_mm"] <- 0.098
clean_traits3[clean_traits3$ID == "EQE2168", "leaf_thickness_1_mm"] <- 0.257
clean_traits3[clean_traits3$ID == "HMZ3031", "leaf_thickness_1_mm"] <- 0.219
clean_traits3[clean_traits3$ID == "GTE6076", "leaf_thickness_1_mm"] <- 0.170

# function to count decimal places, as thickness measurements should only have a precision of .001 not .0001
count_decimals = function(x) {
  if (length(x) == 0) return(numeric())
  x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  x_int = floor(x) %>% abs() %>% nchar()
  x_nchr = x_nchr - 1 - x_int
  x_nchr[x_nchr < 0] = 0
  x_nchr}

clean_traits3$dec_1 <- NA
clean_traits3$dec_2 <- NA
clean_traits3$dec_3 <- NA
for(i in 1:nrow(clean_traits3)) {clean_traits3$dec_1[i] <- count_decimals(clean_traits3$leaf_thickness_1_mm[i])}
for(i in 1:nrow(clean_traits3)) {clean_traits3$dec_2[i] <- count_decimals(clean_traits3$leaf_thickness_2_mm[i])}
for(i in 1:nrow(clean_traits3)) {clean_traits3$dec_3[i] <- count_decimals(clean_traits3$leaf_thickness_3_mm[i])}


rm(thickness)
clean_traits3 <- select(clean_traits3, -c(false, thickness_var, dec_1, dec_2, dec_3))

#_______________________________________________________________________________
