### Clean trait data

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(validate)
library(PFTCFunctions)
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")
#devtools::install_version("TNRS")
library(TNRS) # match taxa names

# download raw trait data from OSF
get_file(node = "pk4bg",
         file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
         path = "raw_data/traits/",
         remote_path = "RawData/Traits")

# import data
raw_traits <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "Data")

raw_dry_mass <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "DryMass")

raw_leaf_area <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_area_2022.xlsx")


### Data cleaning

# Remove rows with just NA
clean_traits <- raw_traits %>%
                filter(if_any(everything(), ~ !is.na(.)))

# Remove Seans data

clean_traits <- clean_traits %>%
  filter(is.na(project)|project!="Sean") #


# Fix days, month, year, date
# Sort out project and siteID
# add elevation
# fix experiment column when obviously wrong
# clean leaf thickness data which is wrong

  clean_traits <- clean_traits %>%
    mutate(siteID = if_else(siteID == "vik", "Vik", siteID),
           day = case_when( siteID == "Ulv" & project == "Incline" ~ 24,
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
           elevation = case_when(siteID == "Ulv" ~  1208,
                                 siteID == "Hog" ~ 700,
                                 siteID == "Vik" ~ 469,
                                 siteID == "Gud" ~ 1213,
                                 siteID == "Lia" ~ 1290,
                                 siteID == "Skj" ~ 1088,
                                 siteID == "Joa" ~ 920),
           experiment = ifelse(experiment == "NA", NA, experiment),
           experiment = ifelse(experiment == "N" & project == "Incline", NA, experiment),
           leaf_thickness_1_mm = if_else(ID == "IKY0250", 0.207, leaf_thickness_1_mm),
           leaf_thickness_1_mm = if_else(ID == "DEV8302", 0.155, leaf_thickness_1_mm),
           leaf_thickness_2_mm = if_else(ID == "CZW4480", "0.153", leaf_thickness_2_mm),
           leaf_thickness_2_mm = if_else(ID == "DDI9716", "0.223", leaf_thickness_2_mm),
           leaf_thickness_2_mm = if_else(ID == "DEX5838", "0.185", leaf_thickness_2_mm),
           leaf_thickness_3_mm = if_else(ID == "CHV2350", 0.198, leaf_thickness_3_mm),
           leaf_thickness_2_mm = as.numeric(leaf_thickness_2_mm))


# Fix day and project which didnt change

clean_traits <- clean_traits %>%
    mutate(day = ifelse(ID == "EDH3100", 27, day),
           project = ifelse(ID == "EDH3100", "Incline", project))

### Clean taxa names


# first get in data from new_taxon col

clean_traits$taxon <- ifelse(is.na(clean_traits$taxon), clean_traits$new_taxon, clean_traits$taxon)
species <- unique(clean_traits$taxon)
species <- TNRS(species)

# Now fix names

clean_traits <- clean_traits %>%
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


  # join leaf area and dry mass data
  left_join(leaf_area, by = "ID") |>
  left_join(dry_mass, by = "ID") |>

  # calculate average thickness, sla and ldmc
  # rename
  rename(wet_mass_total_g = wet_mass_g,
         leaf_area_total_cm2 = leaf_area_cm2,
         dry_mass_total_g = dry_mass_g,
         nr_leaves = bulk_nr_leaves) |>

  # Calculate average leaf thickness
  mutate(leaf_thickness_ave_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE)) %>%
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / nr_leaves,
         dry_mass_g = dry_mass_total_g / nr_leaves,
         leaf_area_cm2 = leaf_area_total_cm2 / nr_leaves) |>

  # Wet and dry mass do not make sense for these species
  mutate(dry_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, dry_mass_g),
         wet_mass_g = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, wet_mass_g),
         leaf_area_cm2 = ifelse(genus %in% c("Baccharis", "Lycopodiella", "Lycopodium", "Hypericum"), NA_real_, leaf_area_cm2)) |>

  # Calculate SLA and LDMC (replace with wet mass for now)
  mutate(sla_cm2_g = leaf_area_cm2 / wet_mass_g,
         ldmc = dry_mass_g / wet_mass_g)


  #check for duplicate barcodes and make sure data is different

  dups<-clean_traits[duplicated(clean_traits$ID), ]

  dupID<-as.vector(dups$ID)

  dup2x<-  clean_traits[clean_traits$ID %in% dupID, ]


  #List of ID codes where the data is true duplicates:
  real_dups<-tibble(ID= c( "AFE7141", "ALZ2013", "APD9921", "BMT1443", "DUH2615",
                           "EFN3512", "GKL3008", "HLT2732"))

  #List of ID codes where the data is true duplicates:

  not_real_dups<-tibble(ID= c("ACM3709", "AQK5961", "BNK8495", "BNN7822", "CTQ9841",
                              "FUY4409", "HRT6861", "IGM2553"))

