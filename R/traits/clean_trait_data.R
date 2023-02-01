### Clean trait data

#load libraries
source("R/load_libraries.R")

# download raw trait data from OSF
get_file(node = "pk4bg",
         file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
         path = "raw_data/traits/",
         remote_path = "RawData/Traits")


# import trait data
raw_traits <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "Data", na = c("", "NA"))


### Data cleaning ----

### Check IDs
# get list of valid IDs
valid_codes <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)
#
# raw_traits |>
#   anti_join(valid_codes, by = c("ID" = "hashcode"))

# Check combinations of experiment, siteID and day
# clean_traits %>%
#   select(siteID,experiment,day,project,elevation_m_asl) %>%
#   unique() |>
#   arrange(project, siteID, day, experiment) |>
#   print(n = Inf)

# Incline sites are Ulv, Gud, Skj
# SO thats an easy correction if we assume siteID is correct?
# It is not always correct
# But luckily - only Incline added elevation to their write up?
# SO convert based on this first

# leaf thickness
# lines of code that generate the variance of leaf thickness values to find outliers
# thickness <-clean_traits %>% select(c(ID,leaf_thickness_1_mm,leaf_thickness_2_mm,leaf_thickness_3_mm)) %>%
#   pivot_longer(cols = c(leaf_thickness_1_mm,leaf_thickness_2_mm,leaf_thickness_3_mm), names_to = "measurement", values_to = "value")
# thickness <- aggregate(log(thickness$value), list(thickness$ID), FUN = var)
# thickness <- rename(thickness, ID = Group.1, thickness_var = x)
# clean_traits <- left_join(clean_traits, thickness, by = "ID")


# function to count decimal places, as thickness measurements should only have a precision of .001 not .0001
# count_decimals = function(x) {
#   if (length(x) == 0) return(numeric())
#   x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
#   x_int = floor(x) %>% abs() %>% nchar()
#   x_nchr = x_nchr - 1 - x_int
#   x_nchr[x_nchr < 0] = 0
#   x_nchr}
#
# clean_traits$dec_1 <- NA
# clean_traits$dec_2 <- NA
# clean_traits$dec_3 <- NA
# for(i in 1:nrow(clean_traits)) {clean_traits$dec_1[i] <- count_decimals(clean_traits$leaf_thickness_1_mm[i])}
# for(i in 1:nrow(clean_traits)) {clean_traits$dec_2[i] <- count_decimals(clean_traits$leaf_thickness_2_mm[i])}
# for(i in 1:nrow(clean_traits)) {clean_traits$dec_3[i] <- count_decimals(clean_traits$leaf_thickness_3_mm[i])}


clean_traits <- raw_traits %>%
  # remove unused column
  select(-length_cm) |>
  # Remove rows with just NA
  filter(if_any(everything(), ~ !is.na(.))) |>

  # fix ID
  mutate(ID = case_when(ID == "COZ7391" ~ "COY7391",
                        ID == "CUZ1951" ~ "CUY1951",
                        ID == "BKV1781" ~ "BJV1781",
                        ID == "1848950.0" ~ "APR6962",
                        TRUE ~ ID)) |>

  # fix project
  mutate(project = case_when(ID %in% c("AGP9286", "HRQ1892", "BPC8034") ~ "3D",
                             ID %in% c("CJC4018", "CSV2097", "EDH3100") ~ "Incline",
                             ID %in% c("CGE9632", "IBW5191") ~ "Sean",
                             TRUE ~ project)) |>

  # Remove Seans data
  filter(is.na(project)|project !="Sean") |>

  # fix siteID
  mutate(siteID = if_else(siteID == "vik", "Vik", siteID),
         siteID = if_else(ID == "BIE2833", "Ulv", siteID),
         siteID = if_else(ID == "BPC8034", "Hog", siteID)) |>

  # Fix days and siteID
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
                             TRUE ~ project)) |>

  # Fix month, year, date
  mutate(year = 2022,
         month = if_else(day == 1, 8, 7),
         date = make_date(year, month, day),

         # Add elevation
         elevation_m_asl = case_when(siteID == "Ulv" ~  1208,
                                     siteID == "Hog" ~ 700,
                                     siteID == "Vik" ~ 469,
                                     siteID == "Gud" ~ 1213,
                                     siteID == "Lia" ~ 1290,
                                     siteID == "Skj" ~ 1088,
                                     siteID == "Joa" ~ 920)) |>

  # Fix experiment column when obviously wrong
  mutate(experiment = ifelse(experiment == "NA", NA_character_, experiment),
         # checked with field sheets
         experiment = if_else(ID == "DJD3630", "OTC", experiment)) |>

  # Clean leaf thickness data which is wrong (LT > 150)
  mutate( leaf_thickness_1_mm = if_else(ID == "IKY0250", 0.207, leaf_thickness_1_mm), # fix unit errors
          leaf_thickness_1_mm = if_else(ID == "DEV8302", 0.155, leaf_thickness_1_mm),
          leaf_thickness_2_mm = if_else(ID == "DDI9716", 0.223, leaf_thickness_2_mm),
          leaf_thickness_2_mm = if_else(ID == "DEX5838", 0.185, leaf_thickness_2_mm),
          leaf_thickness_3_mm = if_else(ID == "CHV2350", 0.198, leaf_thickness_3_mm),
          # there is a note that mistakenly says that leaf thickness should be 0.0325
          leaf_thickness_3_mm = if_else(ID == "HWK3847", 0.325, leaf_thickness_3_mm),
          leaf_thickness_1_mm = if_else(ID == "CHW9026", 0.206, leaf_thickness_1_mm),
          leaf_thickness_2_mm = if_else(ID == "CHW9026", 0.215, leaf_thickness_2_mm),
          leaf_thickness_3_mm = if_else(ID == "CHW9026", 0.21, leaf_thickness_3_mm),
          # outlier values of ~ 1 magnitude have been changed to better reflect other measured values of the sample
          leaf_thickness_2_mm = if_else(ID == "ICR7173", 0.091, leaf_thickness_2_mm),
          leaf_thickness_2_mm = if_else(ID == "FVU2190", 0.038, leaf_thickness_2_mm),
          leaf_thickness_1_mm = if_else(ID == "ESD7613", 0.243, leaf_thickness_1_mm),
          leaf_thickness_1_mm = if_else(ID == "ESV6901", 0.247, leaf_thickness_1_mm),
          leaf_thickness_1_mm = if_else(ID == "CJN0746", 0.099, leaf_thickness_1_mm),
          leaf_thickness_3_mm = if_else(ID == "CJN0746", 0.099, leaf_thickness_3_mm),
          leaf_thickness_1_mm = if_else(ID == "CWC6486", 0.265, leaf_thickness_1_mm),
          leaf_thickness_2_mm = if_else(ID == "CCO1039", 0.099, leaf_thickness_2_mm),
          leaf_thickness_3_mm = if_else(ID == "CCO1039", 0.094, leaf_thickness_3_mm),
          leaf_thickness_1_mm = if_else(ID == "EPR5076", 0.236, leaf_thickness_1_mm),
          leaf_thickness_1_mm = if_else(ID == "CXJ5628", 0.098, leaf_thickness_1_mm),
          leaf_thickness_1_mm = if_else(ID == "EQE2168", 0.257, leaf_thickness_1_mm),
          leaf_thickness_1_mm = if_else(ID == "HMZ3031", 0.219, leaf_thickness_1_mm),
          leaf_thickness_1_mm = if_else(ID == "GTE6076", 0.170, leaf_thickness_1_mm),
          leaf_thickness_1_mm = if_else(ID == "BFU6275", 0.197, leaf_thickness_1_mm),
          leaf_thickness_2_mm = if_else(ID == "BFU6275", 0.172, leaf_thickness_2_mm),
          leaf_thickness_1_mm = if_else(ID == "ANV4280", 0.249, leaf_thickness_1_mm),
          leaf_thickness_3_mm = if_else(ID == "CJC4018", 0.091, leaf_thickness_3_mm),
          leaf_thickness_3_mm = if_else(ID == "GSD6144", 0.183, leaf_thickness_3_mm),
          leaf_thickness_1_mm = if_else(ID == "CXL2586", 0.073, leaf_thickness_1_mm)
          ) |>

  # fix plant height
  # fix wrong value .14.8, which gets converted to NA in data import
  mutate(plant_height = case_when(ID == "EYC5540" ~ 14.8,
                                  # height values drawn from Incline group fieldwork sheets
                                  ID == "BOU0176" ~ 8.1,
                                  ID == "BDQ5475" ~ 5.3,
                                  ID == "EDH3100" ~ 14.6,
                                  # missing heights and extracting from envelopes
                                  ID == "CHW9026" ~ 12.7,
                                  ID == "BHS3927" ~ 9.4,
                                  ID == "AQA6446" ~ 15.2,
                                  ID == "AIT4560" ~ 3.1,
                                  ID == "AIX4648" ~ 13.7,
                                  ID == "APO5292" ~ 11,
                                  ID == "AJB8204" ~ 8.5,
                                  ID == "CBX5036" ~ 14.3,
                                  ID == "APQ8072" ~ 15,
                                  ID == "BQG7481" ~ 6,
                                  ID == "BFI7437" ~ 37,
                                  ID == "AQK5961" ~ 17.5,
                                  ID == "CDE0818" ~ 12.9,
                                  ID == "CON9328" ~ 6.7,
                                  ID == "DAK3110" ~ 11.1,
                                  ID == "CZY5489" ~ 33.9,
                                  ID == "GPW1350" ~ 35,
                                  ID == "GDI1096" ~ 7.4,
                                  ID == "CWQ2942" ~ 12.8,
                                  ID == "CUC2352" ~ 17.3,
                                  TRUE ~ plant_height),
         # All incline plants were measured in mm, so convert to cm
         plant_height = ifelse(project == "Incline", plant_height/10, plant_height),
         # fix some leaves that were measured in mm
         plant_height = ifelse(plant_height > 59, plant_height/10, plant_height)) |>

  # Wet mass
  mutate(wet_mass_g = case_when(ID == "CHW9026" ~ 0.0698,
                                ID == "GDI1096" ~ 0.0422,
                                ID == "CWQ2942" ~ 0.1312,
                                ID == "CUC2352" ~ 0.1232,
                                ID == "BOF6747" ~ 0.0733,
                                ID == "EIZ1694" ~ 0.0126,
                                ID == "GCU2533" ~ 0.0441,
                                ID == "GTA1209" ~ 0.0174,
                                ID == "GIZ0068" ~ 0.0663,
                                ID == "GYZ8905" ~ 0.3574,
                                ID == "ERM6771" ~ 0.0669,
                                ID == "DVI2460" ~ 1.3371,
                                TRUE ~ wet_mass_g))



# Check experiments column
# Experiment should all be C or OTC for Incline
# clean_traits %>%
#   select(siteID,experiment,day,project,elevation_m_asl) %>%
#   arrange(project, experiment, siteID) |>
#   unique()

# Have 2 entries without experiment or siteID (the rest are wrong species and can be deleted)
# clean_traits %>%
#   filter(is.na(siteID) | is.na(experiment) & project == "Incline") |> as.data.frame()

# Check for the incline experiments which are missing
### PROBABLY OTC 1
# clean_traits %>%
#   filter( siteID == "Gud") %>%
#   filter(taxon == "Campanula rotundifolia") %>%
#   group_by(siteID,taxon,plotID,experiment,individual_nr) %>%
#   tally()


### Duplicates, missing plot IDs and Individual Number

#unique(clean_traits$plotID)
# Make plot ID which is "NA" NA
# N or C needs to have a number ID
# if not should have a code
# incline should be numbers

# clean_traits %>%
#   filter(is.na(plotID))
# Comments have the plot ID for some of them - incorporate these and cross reference with Aud's metadata package
# to make sure they're correct - as some are missing info

# fix almost duplicate
# clean_traits |>
#   group_by(ID) |>
#   mutate(n = n()) |> filter(n > 1) |> View()

## Check individual numbers
# na's
# clean_traits %>%
#   filter(is.na(individual_nr)) |>
#   as.data.frame()

# duplicate ind nr
# clean_traits |>
#   group_by(siteID, taxon, project, experiment, plotID, individual_nr) |>
#   mutate(n = n()) |> filter(n > 1) |>
#   select(ID, siteID, taxon, experiment, plotID, individual_nr) |>
#   arrange(siteID, taxon, experiment, plotID, individual_nr)

# Use dataDocumentation package
meta_data_3D <- create_threed_meta_data() |>
  select(turfID, warming, grazing, Nlevel, Namount_kg_ha_y)
# this can correct some of the plotID issues


clean_traits <- clean_traits %>%
  ## PlotID
  mutate(plotID = case_when(plotID =="B2" ~ "2.0",
                            plotID =="B3" ~ "3.0",
                            plotID =="BL5" ~ "5.0",
                            ID == "GAO0614" ~ "6.0",
                            plotID =="NA"~ NA_character_,
                            TRUE ~ plotID)) |>
  mutate(plotID = case_when(remark == "1 85 WN1C 167" ~ "1-85 WN1C 162", # corrected to metadata
                            remark == "5-57AN8C 57" ~ "8-57 AN8C 57", # says 5-57 but no 5-57 in metadata so changed to 8
                            remark == "Plot ID on envelope is 10-75-AN3N-79" ~ "10-79 AN2N 79", # has to be this one
                            remark == "Plot ID: WN3CN 112" ~ "4-32 WN3N 112",
                            remark == "Plot ID: 7-109AN3C 109" ~ "4-109 AN3C 109", # corrected number
                            remark == "Plot ID: 4-105AN3C 109" ~ "4-109 AN3C 109",
                            TRUE ~ plotID),
         plotID = ifelse(ID == "CIO0085","2.0",plotID)) |>
  # remove wrong plants with no trait measurements
  mutate(remark = tolower(remark)) |>
  filter(!grepl("wrong species", remark)) |>

  ## DUPLICATES
  # remove real duplicates
  distinct() |>
  # remove almost real duplicates
  filter(!(ID == "CTQ9841" & remark == "delete, duplicate with wrong values" & !is.na(remark))) |>
  filter(!(ID == "APD9921" & leaf_thickness_3_mm == "0.281")) |>
  filter(!(ID == "AFE7141" & is.na(bulk_nr_leaves))) |>
  filter(!(ID == "DUH2615" & individual_nr == 1)) |>

  ## Long and ugly list to correct ind nr
  mutate(individual_nr = case_when(ID == "APQ8072" ~ 5,
                                   ID == "BEI6014" ~ 4,
                                   ID == "CTM2082" ~ 1,
                                   ID == "BMT1443" ~ 2,
                                   ID == "AGG5788" ~ 4,
                                   ID == "CRS5764" ~ 4,
                                   ID == "CQD6658" ~ 5,
                                   ID == "CYH5232" ~ 3,
                                   ID == "CPL3980" ~ 6,
                                   ID == "CUR3983" ~ 3,
                                   ID == "DZC2489" ~ 4,
                                   ID == "DMR2654" ~ 5,
                                   ID == "DHR0146" ~ 6,
                                   ID == "DSM5404" ~ 5,
                                   ID == "DIJ8969" ~ 6,
                                   ID == "DSI7474" ~ 7,
                                   ID == "EHV4649" ~ 5,
                                   ID == "EHE3636" ~ 4,
                                   ID == "DTQ6176" ~ 4,
                                   ID == "DLV4976" ~ 4,
                                   ID == "DLB9991" ~ 5,
                                   ID == "EKY3708" ~ 5,
                                   ID == "EKX6404" ~ 6,
                                   ID == "EIP7539" ~ 4,
                                   ID == "DIK4930" ~ 2,
                                   ID == "BQW6041" ~ 2,
                                   ID == "EJL3434" ~ 4,
                                   ID == "EDS0348" ~ 5,
                                   ID == "DVZ2676" ~ 6,
                                   ID == "EDU0323" ~ 3,
                                   ID == "DWJ1146" ~ 4,
                                   ID == "EJF0038" ~ 5,
                                   ID == "DLF9268" ~ 6,
                                   ID == "DHE6369" ~ 4,
                                   ID == "BRK2754" ~ 6,
                                   ID == "EGK3307" ~ 2,
                                   ID == "BRY2565" ~ 2,
                                   ID == "DSC4388" ~ 4,
                                   ID == "DKT9944" ~ 5,
                                   ID == "BRQ6790" ~ 2,
                                   ID == "DSZ2667" ~ 4,
                                   ID == "EQT1079" ~ 6,
                                   ID == "HMG5583" ~ 2,
                                   ID == "GDI1096" ~ 6,
                                   ID == "HSU7673" ~ 4,
                                   ID == "ERO0676" ~ 2,
                                   ID == "IFE5070" ~ 4,
                                   ID == "IKL3299" ~ 6,
                                   ID == "HLH7856" ~ 3,
                                   ID == "GFG1251" ~ 2,
                                   ID == "EOW0861" ~ 5,
                                   ID == "IIP7429" ~ 4,
                                   ID == "GWO3443" ~ 2,
                                   ID == "GTC8484" ~ 1,
                                   ID == "HCE4621" ~ 3,
                                   ID == "EQG2060" ~ 4,
                                   ID == "DJQ2908" ~ 1,
                                   ID == "CUV4540" ~ 2,
                                   ID == "EJR3255" ~ 2,
                                   ID == "DSE5104" ~ 3,
                                   ID == "GQW0555" ~ 3,
                                   ID == "INJ6310" ~ 1,
                                   TRUE ~ individual_nr))

### Clean taxa names ----

# use TNRS package to find correct names
# species <- unique(clean_traits$taxon)
# tnrs_species_check <- TNRS(species)

clean_traits <- clean_traits |>
  # first get in data from new_taxon col
  mutate(taxon = if_else(is.na(taxon), new_taxon, taxon)) |>
  # Now fix species names
  mutate(remark = if_else(taxon == "Festuca officinalis", "Taxon was Festuca officinalis, changed to F. ovina after checking scan", remark)) %>%
  mutate(taxon = str_replace_all(taxon,
                                 c("Salix herbaceae" = "Salix herbacea",
                                   "Astragulus alpinus" = "Astragalus alpinus",
                                   "Oxyna diggna" = "Oxyria digyna",
                                   "Alchemilla spp" = "Alchemilla sp",
                                   "Achemilla sp" = "Alchemilla sp",
                                   "Achemilla sp." = "Alchemilla sp",
                                   "Alchemilla sp." = "Alchemilla sp",
                                   "Carex sp." = "Carex sp",
                                   "Festuca officinalis" = "Festuca ovina",
                                   "Astralagulus sp." = "Astragalus alpinus",
                                   "Geranium sylvatica" = "Geranium sylvaticum"))) %>%
  select(-new_taxon) |>
  # fix wrong species in data
  mutate(taxon = if_else(ID == "HIW1048", "Carex bigelowii", taxon))







#### JOIN DRY MASS AND LEAF AREA ####
### source dry mass and leaf area
source("R/traits/clean_dry_mass_and_area.R")

# Seans leaves
# Sean <- run lines 65-85 with == "Sean"
# dry_mass |> anti_join(clean_traits, by = "ID") |>
#   anti_join(Sean, "ID")
# no leaves that do not match

# leaf_area |> anti_join(clean_traits, by = "ID") |>
#   anti_join(Sean, by = "ID")
# out, out_2, EDJ2892, EGN0308_2, EOP1516, GAL3376

clean_traits2 <- clean_traits |>
  # join dry mass and area
  left_join(dry_mass, by = "ID") |>
  left_join(leaf_area, by = "ID") |>

  # fix wet mass that is 10 times too large
  mutate(ratio = leaf_area/wet_mass_g,
         wet_mass_g = if_else(ratio < 10, wet_mass_g/10, wet_mass_g),
         comment_dm = if_else(ratio < 10, paste0(comment_dm, "_", "10 x too large, corrected wet mass"), comment_dm)) |>

  # remove wrong species
  filter(!grepl("Wrong Species|wrong species|WRONG SPECIES", remark)) |>

  # FIX COMMENTS
  # missing data
  mutate(comment_dm = if_else(is.na(dry_mass_g), paste0(comment_dm, "_dry mass missing"), comment_dm),
         comment_dm = if_else(is.na(wet_mass_g), paste0(comment_dm, "_wet mass missing"), comment_dm),
         comment_dm = if_else(is.na(plant_height), paste0(comment_dm, "_height missing"), comment_dm),
         comment_area = if_else(is.na(leaf_area), paste0(comment_area, "_area missing"), comment_area),
         # missing info that cannot be solved
         # DLL3549, EVU9278, EXQ7925
         comment_area = if_else(is.na(plotID), "missing plotID", comment_area),
         comment_area = if_else(ID == "EVU9278", "missing plotID and ind_nr", comment_area)) |>
  rename(remark_dry_mass = remark_dry_weighing) |>
  # add comment to scans that did not work
  mutate(comment_area = if_else(ID %in% c("ACZ3726", "AGG5788", "BFQ1216", "BFR1655", "BPQ4029", "DBV4120", "DTS3934", "GYL1269", "HLM0330", "IFW2666", "IHS9389", "ILK6566"), "scan corrupt_area missing", comment_area)) |>

  # other problems
  mutate(comment_area = case_when(grepl("fungus", remark) ~ "damage_spots",
                                  grepl("frozen!", remarks_2) ~ "damage_frost",
                                  ID %in% c("EIZ1694", "CZR5069") ~ "wet mass when dry_wet mass < expected",
                                  comment_area == "NA_area missing" ~ "area missing",
                                  ID == "DVA0594" ~ "including sheath_mass area > expected",
                                  ID == "ADG7762" ~ "including sheath_mass area > expected",
                                  TRUE ~ comment_area),

         comment_dm = case_when(ID %in% c("GWT1967", "GUH1653") ~ "flattened for thickness measure_thickness",
                                ID %in% c("DVQ3484", "FXQ1526") ~ "thickness measured when dry_thickness",
                                ID %in% c("GTC8484") ~ "sterile brachlets",
                                comment_dm == "NA_dry mass missing" ~ "dry mass missing",
                                comment_dm == "height missing_NA" ~ "height missing",
                                comment_dm == "NA_height missing" ~ "height missing",
                                comment_dm == "NA_wet mass missing" ~ "wet mass missing",
                                TRUE ~ comment_dm)) |>

  # merge trait comments
  mutate(comment = paste0(comment_dm, "_", comment_area),
         comment = str_remove(comment, "NA_NA_|NA_|_NA|NA")) |>

  # merge remark and remarks_2
  mutate(remark = if_else(!is.na(remarks_2), paste0(remark, "_", remarks_2), remark)) |>
  select(-remarks_2, -comment_dm, -comment_area) |>

  # Make flags
  mutate(flag = case_when(
    # not reliable
    grepl("damage|stem leaves|sterile brachlets", comment) ~ "unrealiable data",

    # specifics not reliable
    grepl("area < expected", comment) ~ "unrealibale area",
    grepl("dry mass < expected", comment) ~ "unrealibale dry mass",
    grepl("wet mass < expected", comment) ~ "unrealibale wet mass",
    grepl("wet mass > expected", comment) ~ "unrealibale wet mass",
    grepl("area_mass < expected|mass area > expected", comment) ~ "unrealibale mass and area",
    grepl("flattened for thickness measure|thickness measured when dry", comment) ~ "unrealibale thickness",
    grepl("leaf area from one leaf was matched to leaf with missing area", comment) ~ "potentially unrealibale area",

    # missing
    comment == "missing plotID and ind_nr" ~ "missing plotID and ind_nr",
    comment == "missing plotID" ~ "missing plotID",
    comment == "height missing" ~ "missing height",
    comment == "wet mass missing" ~ "missing wet mass",
    comment == "dry mass missing" ~ "missing dry mass",
    comment == "area missing" ~ "missing area",
    comment == "scan corrupt_area missing" ~ "missing area",
    comment == "thickness missing" ~ "missing thickness",

    # multiple issues
    comment == "height missing_mid_overlapping_folded_leaves_area < expected" ~ "missing height_unrealibale area",
    comment == "low_overlapping_folded_leaves_damage_area < expected" ~ "unrealibale data",
    comment == "part of petiole gone, dry mass < expected_area missing" ~ "unrealibale dry mass_missing area",
    comment == "petiole missing, area_mass < expected_damage_herbivory" ~ "unrealiable data",
    comment == "petiole missing, area_mass < expected_low_overlapping_folded_leaves_area < expected" ~ "unrealiable mass and area",
    comment == "petiole missing, area_mass < expected_mid_overlapping_folded_leaves_area < expected" ~ "unrealiable mass and area")) |>

  # add flowering from remarks
  mutate(flowering = if_else(is.na(flowering) & grepl("flower|Flower", remark), "flower", flowering)) |>

  # sean leaf
  filter(!ID == "HUI3674") |>

  # Fix leaf area columns
  rename(number_leaf_fragments_scanned = n,
         wet_mass_total_g = wet_mass_g,
         dry_mass_total_g = dry_mass_g,
         leaf_area_total_cm2 = leaf_area,
         nr_leaves_wm = bulk_nr_leaves) |>

  # FIX NR OF LEAVES
  # replace nr leave if NA
  mutate(nr_leaves_wm = ifelse(is.na(nr_leaves_wm), number_leaf_fragments_scanned, nr_leaves_wm)) |>
  # make variable for nr of leaves for dry mass (for lost leaves during trait wheel)
  mutate(nr_leaves_dm = if_else(is.na(nr_leaves_dm), nr_leaves_wm, nr_leaves_dm)) |>

# bulk number of leaves counted from leaf scans
  mutate(nr_leaves_wm = case_when(ID == "CHW9026" ~ 1,
                               ID == "BHS3927" ~ 1,
                               ID == "AQA6446" ~ 1,
                               ID == "AIT4560" ~ 1,
                               ID == "AIX4648" ~ 1,
                               ID == "APO5292" ~ 1,
                               ID == "AJB8204" ~ 1,
                               ID == "CBX5036" ~ 1,
                               ID == "APQ8072" ~ 1,
                               ID == "BQG7481" ~ 1,
                               ID == "BFI7437" ~ 1,
                               ID == "AQK5961" ~ 1,
                               ID == "CDE0818" ~ 2,
                               ID == "DAK3110" ~ 1,
                               ID == "CZY5489" ~ 3,
                               ID == "GPW1350" ~ 1,
                               ID == "CZR5069" ~ 3,
                               TRUE ~ nr_leaves_wm)) %>%




  # Calculate nr of thickness measurements and average leaf thickness
  rowwise() %>%
  mutate(nr_thickness = sum(!is.na(leaf_thickness_1_mm), !is.na(leaf_thickness_2_mm), !is.na(leaf_thickness_3_mm), na.rm = TRUE)) |>
  ungroup() %>%
  mutate(leaf_thickness_mm = rowMeans(select(., matches("leaf_thickness_\\d_mm")), na.rm = TRUE),
        # add comment if missing
        comment = if_else(is.na(leaf_thickness_mm), "thickness missing", comment))  |>
  # Calculate values on the leaf level (mostly bulk samples)
  mutate(wet_mass_g = wet_mass_total_g / nr_leaves_wm,
         dry_mass_g = dry_mass_total_g / nr_leaves_dm,
         leaf_area_cm2 = leaf_area_total_cm2 / nr_leaves_wm)  |>
  # Calculate SLA and LDMC (replace with wet mass for now)
  mutate(sla_cm2_g = leaf_area_cm2 / dry_mass_g,
         ldmc = dry_mass_g / wet_mass_g) |>

  # STREAMLINE TERMINOLOGY WITH VCG, ThreeD and INCLINE
  # make full siteID
  mutate(siteID = recode(siteID,
                         # old name (replace) = valid name (do not change)
                         'Gud' = "Gudmedalen",
                         'Ulv' = "Ulvehaugen",
                         'Skj' = "Skjelingahaugen",
                         'Hog' = "Hogsete",
                         'Lia' = "Liahovden",
                         'Vik' = "Vikesland",
                         'Joa' = "Joasete")) |>



  # join ThreeD metadata and fix terms
  # plotID should be split into blockID and turfID for ThreeD
  # experiment: ThreeD does not have experiment, but has warming, grazing and Nlevel
  # plotID should be blockID for gradient + add 3 letters of siteID
  # experiment should be grazing for gradient
  mutate(blockID = if_else(project == "3D" & nchar(plotID) > 3, str_remove(plotID, "\\-.*"), NA_character_),
         blockID = if_else(project == "3D" & nchar(plotID) < 6, plotID, blockID),
         blockID = str_remove(blockID, "\\.0"),
         turfID = if_else(project == "3D" & nchar(plotID) > 3, str_extract(plotID, "\\-.*"), NA_character_),
         turfID = gsub("-", "", turfID)) |>
  left_join(meta_data_3D, by = "turfID") |>

  # gradient
  # add warming, grazing and Nlevel for the gradient
  # make variable that defines all the plots belonging to the gradient
  mutate(warming = if_else(project == "3D" & is.na(turfID), "A", warming),
         grazing = if_else(project == "3D" & is.na(turfID), experiment, grazing),
         Nlevel = if_else(project == "3D" & is.na(turfID), 0, Nlevel),
         Namount_kg_ha_y = if_else(project == "3D" & is.na(turfID), 0, Namount_kg_ha_y),
         gradient = if_else(project == "3D" & warming == "A" & Nlevel < 4, "gradient", NA_character_)) |>

  # Incline terminology
  # make experiment column OTC with W and C
  mutate(OTC = if_else(project == "Incline", experiment, NA_character_),
         OTC = recode(OTC, "OTC" = "W"),
         plotID = if_else(project == "Incline", str_remove(plotID, "\\.0"), plotID),
         blockID = if_else(project == "Incline" & !is.na(plotID), paste(substr(siteID, 1, 3), plotID, sep = "_"), blockID)) |>
  rename(plant_height_cm = plant_height) |>

  # make table long
  pivot_longer(cols = c(plant_height_cm, wet_mass_g, dry_mass_g, leaf_area_cm2, leaf_thickness_mm, ldmc, sla_cm2_g), names_to = "trait", values_to = "value") |>

  #log transform size and area traits
  mutate(value_trans = if_else(
    trait %in% c("plant_height_cm", "wet_mass_g", "dry_mass_g", "leaf_area_cm2", "leaf_thickness_mm"),
    true = suppressWarnings(log(value)),# suppress warnings from log(-value) in isotopes (these are calculated but not kept)
    false = value),
  trait_trans = recode(trait,
    "plant_height_cm" = "plant_height_log_cm",
    "wet_mass_g" = "wet_mass_log_g",
    "dry_mass_g" = "dry_mass_g_log",
    "leaf_area_cm2" = "leaf_area_log_cm2",
    "leaf_thickness_mm" = "leaf_thickness_log_mm")) |>
  # select and sort
  select(ID, date, project, gradient, siteID, elevation_m_asl, blockID, turfID, OTC, warming, grazing, Nlevel, Namount_kg_ha_y, individual_nr, species = taxon,
         trait, value, trait_trans, value_trans,
         comment, flag,
         # useful variables
         flowering, nr_leaves_wm, nr_leaves_dm, number_leaf_fragments_scanned, nr_thickness,
         # remove 3 x thickness once data is properly checked
         leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm,
         wet_mass_total_g, dry_mass_total_g, leaf_area_total_cm2,
         remark, remark_dry_mass, scanning_comment)

write_csv(clean_traits, file = "clean_data/PFTC6_clean_leaf_traits_2022.csv")


# Problems that cannot be fixed
# 85 have no area, 12 with corrupt scan
# leaves that cannot be matched:
# out.jpeg from day 27, 1 leaf Pot.ere
# out.jpeg from day 28, 1 leaf Alc.alp
# out.jpeg from day 2, 3 leaves Alc.alp
# EOP1516, a grass, 2 leaves, probably Agrostis
# GAL3376, Sib.pro, 2 leaves


#_______________________________________________________________________________




### TO DO:
### PROBLEMS

# a bit uncertain if match is correct
# 5  3323 raw_data/traits/pftc6_leaf_scans/2022-07-30 out       2    16.1 -> 2 leaves ach millefolium -> can be BGW5255, but seems to be a bit off


# outliers
# 1 x thickness
clean_traits2 |>
  ggplot(aes(x = leaf_thickness_1_mm, y = leaf_thickness_2_mm, colour = siteID)) +
  geom_point()

clean_traits2 |> filter(leaf_thickness_1_mm < 0.5 & leaf_thickness_2_mm > 0.75) |> as.data.frame()
# CKE1363 LT2 and LT3 might be a bit high
clean_traits2 |> filter(taxon == "Festuca rubra") |> ggplot(aes(x = leaf_thickness_mm)) + geom_histogram()

# sla huge
clean_traits2 |>
  ggplot(aes(x = dry_mass_g, y = sla_cm2_g, shape = siteID, colour = sla_cm2_g > 500)) +
  geom_point()
clean_traits2 |> filter(sla_cm2_g > 1000) |> select(ID:plotID, dry_mass_g, wet_mass_g, ldmc, leaf_area_cm2, sla_cm2_g, comment, flag)

clean_traits2 |>
  ggplot(aes(x = dry_mass_g, y = ldmc, colour = sla_cm2_g > 1000)) +
  geom_point()


#_______________________________________________________________________________

#### CHECKING DATA, OUTLIERS ETC. ####

# some outliers for area vs mass
# looks good
clean_traits2 |>
  ggplot(aes(x = leaf_area_cm2, y = dry_mass_g, colour = siteID)) +
  geom_point()

clean_traits2 |>
  ggplot(aes(x = leaf_area_cm2, y = wet_mass_g, colour = siteID)) +
  geom_point()

clean_traits2 |>
  ggplot(aes(x = dry_mass_g, y = wet_mass_g, colour = taxon)) +
  geom_point() +
  theme(legend.position = "none")

# 1 strange value
clean_traits2 |>
  ggplot(aes(x = leaf_thickness_1_mm, y = leaf_thickness_2_mm, colour = siteID)) +
  geom_point()

# looks good!
clean_traits2 |>
  ggplot(aes(x = leaf_thickness_2_mm, y = leaf_thickness_3_mm, colour = siteID)) +
  geom_point()

clean_traits2 |>
  ggplot(aes(x = leaf_thickness_1_mm, y = leaf_thickness_3_mm, colour = siteID)) +
  geom_point()


# looks good
clean_traits2 |>
  ggplot(aes(x = dry_mass_g, y = ldmc, shape = siteID, colour = ldmc > 1)) +
  geom_point()

clean_traits2 |>
  ggplot(aes(x = dry_mass_g, y = sla_cm2_g, shape = siteID, colour = sla_cm2_g > 500)) +
  geom_point()
clean_traits2 |> filter(sla_cm2_g > 1000) |> select(ID:plotID, dry_mass_g, wet_mass_g, ldmc, leaf_area_cm2, sla_cm2_g, comment, flag)


data <- clean_traits
# Libraries
library(viridis)
library(forcats)

#### PLANT HEIGHT ####
#Histogram of plant height
#First, for all data combined
hist(data$plant_height,
     xlab = "Plant height (cm)",
     main = "Check for utliers",
     breaks = sqrt(nrow(data)))

# Now plot per species and site
# Get unique site names to loop over (different plot per site)
sites <- unique(data$siteID)
# Create empty list to save plots
site_plots <- list()
# Run loop to plot all sites
for (site_ in sites) {
  site_plots[[site_]] = ggplot(data %>% filter(siteID == site_),
    aes(x=plant_height, color=taxon, fill=taxon))+
    geom_histogram(alpha = 0.6, binwidth = 0.5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_minimal() +
    theme(
      panel.border = element_rect(fill = NA, color = "grey80"),
      panel.grid = element_blank(),
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("Plant height (cm)") +
    ylab("Frequency") +
    ggtitle(site_) +
    facet_wrap(~taxon, scales = "free")
}
# Select which plot (site) to view
sites
site_plots[1] #Hogsete

# Now with a focus on species
sp_data <- data %>%
  ggplot( aes(x=plant_height, color=siteID, fill=siteID)) +
  geom_histogram(alpha = 0.6, binwidth = 0.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, color = "grey80"),
    panel.grid = element_blank(),
    legend.position = "right",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Plant height (cm)") +
  ylab("Frequency") +
  ggtitle("") +
  facet_wrap(~taxon, scales = "free")
sp_data #Pretty cool! We can start seeing some differences between sites

# Can also plot species individually
# Get unique species names to loop over (different plot per species)
species <- unique(data$taxon)
# Create empty list to save plots
species_plots <- list()
# Run loop to plot all species
for (species_ in species) {
  species_plots[[species_]] = ggplot(data %>%
                                       filter(taxon == species_),
    aes(x=plant_height, color=siteID, fill=siteID))+
    geom_histogram(alpha = 0.6, binwidth = 0.5) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    theme_minimal() +
    theme(
      panel.border = element_rect(fill = NA, color = "grey80"),
      panel.grid = element_blank(),
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab("Plant height (cm)") +
    ylab("Frequency") +
    ggtitle(species_) +
    facet_wrap(~taxon, scales = "free")
}
# Select which plot (species) to view
species
species_plots[1] #Agrostis capillaris
