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

clean_traits <- raw_traits %>%
  # remove unused column
  select(-length_cm) |>
  # Remove rows with just NA
  filter(if_any(everything(), ~ !is.na(.))) |>

  # fix project
  mutate(project = case_when(ID %in% c("AGP9286", "HRQ1892") ~ "3D",
                             ID %in% c("CJC4018", "CSV2097", "EDH3100") ~ "Incline",
                             ID %in% c("CGE9632", "IBW5191") ~ "Sean",
                             TRUE ~ project)) |>

  # Remove Seans data
  filter(is.na(project)|project!="Sean") |>

  # fix siteID
  mutate(siteID = if_else(siteID == "vik", "Vik", siteID),
         siteID = if_else(ID == "BIE2833", "Ulv", siteID)) |>

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
  mutate(experiment = ifelse(experiment == "NA", NA_character_, experiment)) |>

  # Clean leaf thickness data which is wrong (LT > 150)
  mutate( leaf_thickness_1_mm = if_else(ID == "IKY0250", 0.207, leaf_thickness_1_mm), # fix unit errors
          leaf_thickness_1_mm = if_else(ID == "DEV8302", 0.155, leaf_thickness_1_mm),
          leaf_thickness_2_mm = if_else(ID == "DDI9716", 0.223, leaf_thickness_2_mm),
          leaf_thickness_2_mm = if_else(ID == "DEX5838", 0.185, leaf_thickness_2_mm),
          leaf_thickness_3_mm = if_else(ID == "CHV2350", 0.198, leaf_thickness_3_mm)) |>

  # fix plant height
  # fix wrong value .14.8, which gets converted to NA in data import
  mutate(plant_height = if_else(ID == "EYC5540", 14.8, plant_height),
         # All incline plants were measured in mm, so convert to cm
         plant_height = ifelse(project == "Incline", plant_height/10, plant_height),
         # fix some leaves that were measured in mm
         plant_height = ifelse(plant_height > 59, plant_height/10, plant_height),
         # fix typos
         plant_height = case_when(ID == "GDI1096" ~ 0.0422,
                                  ID == "CWQ2942" ~ 0.1312,
                                  ID == "CUC2352" ~ 0.1232))

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
meta_data_3D <- create_threed_meta_data()
# this can correct some of the plotID issues


clean_traits <- clean_traits %>%

  ## PlotID
  mutate(plotID = case_when(plotID =="B2" ~ "2.0",
                            plotID =="B3" ~ "3.0",
                            plotID =="BL5" ~ "5.0",
                            plotID =="NA"~ NA_character_,
                            TRUE ~ plotID)) |>
  mutate(plotID = case_when(remark == "1 85 WN1C 167" ~ "1-85 WN1C 162", # corrected to metadata
                            remark == "5-57AN8C 57" ~ "8-57AN8C 57", # says 5-57 but no 5-57 in metadata so changed to 8
                            remark == "Plot ID on envelope is 10-75-AN3N-79" ~ "10-79 AN2N 79", # has to be this one
                            remark == "Plot ID: WN3CN 112" ~ "4-32 WN3N 112",
                            remark == "Plot ID: 7-109AN3C 109" ~ "4-109AN3C 109", # corrected number
                            remark == "Plot ID: 4-105AN3C 109" ~ "4-109AN3C 109",
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

# first get in data from new_taxon col

clean_traits <- clean_traits |>
  mutate(taxon = if_else(is.na(taxon), new_taxon, taxon))

# use TNRS package to find correct names
# species <- unique(clean_traits$taxon)
# tnrs_species_check <- TNRS(species)

# Now fix species names
clean_traits <- clean_traits %>%
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
  select(-new_taxon)


### PROBLEMS
# DJD3630 missing experiment -> probably C, need to fix plotID -> need to find out if 4 inds in here
clean_traits |>
  filter(siteID == "Gud", taxon == "Campanula rotundifolia") |>
  arrange(plotID, experiment, individual_nr) |>
  as.data.frame()
# DLL3549: missing plotID, maybe 6 -> find out if this one exists
clean_traits |>
  filter(siteID == "Gud", taxon == "Poa alpina", experiment == "OTC") |>
  arrange(plotID, individual_nr) |>
  as.data.frame()
#EVU9278 missing plotID and ind nr, could be 1-3 WN1C 85, because is missing ind 3
clean_traits |>
  filter(siteID == "Joa", taxon == "Sibbaldia procumbens") |>
  arrange(plotID, individual_nr) |>
  as.data.frame()
#EXQ7925 difficult, could be one of those 1-88 AN1N 88, 4-29 WN3C 106
clean_traits |>
  filter(siteID == "Joa", taxon == "Festuca rubra") |>
  arrange(plotID, individual_nr) |> View()
# HIU3378 dry mass exists, so probably not entered
# Can use Aud's package for precise experiment info for 3D when needed




### source dry mass and leaf area
source("R/traits/clean_dry_mass_and_area.R")


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



# Checking for outliers ####
data <- clean_traits3
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

