### CHECK DATA IN SPREADSHEET

# Load libraries
library(tidyverse)
library(lubridate)
library(validate)
library(gridExtra)
library(PFTCFunctions)
library(googlesheets4)


### get all valid IDs (seed for PFTC6 is 49)
uniqueIDs <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)

### Read google sheet
#Install the package if you haven't
#install.packages("googlesheets4")

#Read google sheets data into R
gs4_deauth()
raw_traits <- read_sheet("https://docs.google.com/spreadsheets/d/1ncqbniu0NUzCfcNe2fOl6M2Yj-BeOEXcYlPZHwNiIbk/edit#gid=0")

dd <- readxl::read_excel(path = "raw_data/PFTC6_Norway_Leaf_traits_2022.xlsx")
dd |> distinct(plotID)

# for mulitiple excel files
#myfiles <- dir(path = paste0("raw_data"), pattern = "xlsx", recursive = TRUE, full.names = TRUE)
#mdat <- map_df(myfiles, function(n) read_excel(path = n, col_names = TRUE))


# Set rules
# rules
rules <- validator(

  # check variable types
  is.character(ID),
  is.character(siteID),
  is.character(genus),
  is.character(species),
  is.character(project),
  is.character(experiment),
  is.character(plotID),
  is.character(remark),

  is.numeric(day),
  is.numeric(elevation_m_asl),
  is.numeric(individual_nr),
  is.numeric(leaf_nr),
  is.numeric(plant_height_cm),
  is.numeric(bulk_nr_leaves),
  is.numeric(cut_cm),
  is.numeric(wet_mass_g),
  is.numeric(leaf_thickness_1_mm),
  is.numeric(leaf_thickness_2_mm),
  is.numeric(leaf_thickness_3_mm),

  # matching lists
  ID %in% c(leaf_ID$hashcode), # unique ID
  day %in% c(24, 25, 26, 27, 28, 29, 30, 31, 1, 2, 3, 4),
  siteID %in% c("Vik", "Hog", "Joa", "Lia", "Ulv", "Lav", "Gud", "SkJ"),
  elevation_m_asl %in% c(474, 700, 920, 1320, 1200),
  projcet %in% c("3D", "Incline", "Sean", "Drones"),
  experiment %in% c("gradient", "GC", "C", "control", "OTC"),
  #plotID %in% c(),
  individual_nr %in% c(1:10),
  leaf_nr %in% c(1:10)
  )

out <- confront(raw_traits, rules)
summary(out)

plot(out)



### Uniqueness

unique_rule <- validator(is_unique(ID, siteID, genus, species, project, experiment, plotID, individual_nr, leaf_nr))
out <- confront(raw_traits, unique_rule)
summary(out)
violating(raw_traits, out)



#### Function to plot some figures ####

MakeSomePlots <- function(dat){

  dat <- dat %>%
    mutate(SLA_cm2_g = Leaf_Area_cm2/Dry_Mass_g) %>%
    mutate(LDMC = Dry_Mass_g/Wet_Mass_g)

  if(all(is.na(dat$Dry_Mass_g))){

    # histogram of Wet_mass_g
    p1 <- ggplot(dat, aes(x = Wet_Mass_g)) +
      geom_histogram()

    # histgram of Leaf_Area_cm2
    p2 <- ggplot(dat, aes(x = Leaf_Area_cm2)) +
      geom_histogram() +
      geom_vline(xintercept = 1, color = "red")

  } else{

    # wet vs dry mass
    p1 <- ggplot(dat, aes(x = Wet_Mass_g, y = Dry_Mass_g)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, colour = "red") +
      scale_x_log10() +
      scale_y_log10()

    # dry vs area
    p2 <- ggplot(dat, aes(x = Dry_Mass_g, y = Leaf_Area_cm2)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, colour = "red") +
      scale_x_log10() +
      scale_y_log10()
  }

  p3 <- ggplot(dat, aes(x = LDMC)) +
    geom_histogram()

  p4 <- ggplot(traits, aes(x = Leaf_Thickness_1_mm, y = Leaf_Thickness_2_mm)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    scale_x_log10() +
    scale_y_log10()

  p5 <- ggplot(traits, aes(x = Leaf_Thickness_1_mm, y = Leaf_Thickness_3_mm)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    scale_x_log10() +
    scale_y_log10()

  p6 <- ggplot(traits, aes(x = Leaf_Thickness_2_mm, y = Leaf_Thickness_3_mm)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    scale_x_log10() +
    scale_y_log10()

  grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)

}

