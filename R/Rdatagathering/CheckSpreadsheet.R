### CHECK DATA IN SPREADSHEET

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(assertr)
library(gridExtra)
library(PFTCFunctions)


### get all valid IDs (seed for PFTC6 is 49)
uniqueIDs <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)

### Read google sheet
#Install the required package
#install.packages("googlesheets4")

#Load the required library
library(googlesheets4)
#Read google sheets data into R
gs4_deauth()
raw_traits <- read_sheet("https://docs.google.com/spreadsheets/d/1ncqbniu0NUzCfcNe2fOl6M2Yj-BeOEXcYlPZHwNiIbk/edit#gid=0")

# for mulitiple excel files
#myfiles <- dir(path = paste0("raw_data"), pattern = "xlsx", recursive = TRUE, full.names = TRUE)
#mdat <- map_df(myfiles, function(n) read_excel(path = n, col_names = TRUE))


# Checking data set
nr_col <- 20

# data type
characters <- c("ID", "siteID", "genus", "species", "experiment", "plot_ID", "remark")
numerics <- c("day", "elevation_m_asl", "individual_nr", "leaf_nr", "plant_height_cm", "bulk_nr_leaves", "cut_cm", "wet_mass_g", "leaf_thickness_1_mm", "leaf_thickness_2_mm", "leaf_thickness_3_mm")

IDs <- uniqueIDs$hashcode
days <- c(24, 25, 26, 27, 28, 29, 30, 31, 1, 2, 3, 4)
siteIDs <- c("Vik", "Hog", "Joa", "Lia", "Ulv", "Lav", "Gud", "SkJ")
elevations <- c(474, 700, 920, 1320, 1200)
projects<- c("3D", "Incline", "Sean", "Drones")
experiments <- c("gradient", "GC", "control", "OTC")
plots <- c()
individuals <- c(1:10)
leafs <- c(1:10)


#dat <- raw_traits
#### Function to test trait data ####
CheckSpreadsheet <- function(dat){

  out <- dat %>%
    #chain_start() %>%
    #filter(!is.na(ID)) %>%
    verify(ncol(.) == nr_col, error_fun = error_append) %>% # check number of columns

    assert(is.character, characters, error_fun = error_append) %>%  # is a character
    assert(is.numeric, numerics, error_fun = error_append) %>% # is numeric

    assert(function(x) nchar(x)==7, ID) %>% # check length of ID
    assert(in_set(IDs), ID) %>% # check if ID is valid
    chain_end()

    # Check if Variables only contain elements from lists defined above
    assert(in_set(days), day, error_fun = error_report) %>%
    assert(in_set(site.list), Site, error_fun = error_report) %>%
    #assert(in_set(genus.list), Genus, error_fun = error_report) %>%
    #assert(in_set(species.list), Species, error_fun = error_report) %>%
    assert(in_set(elevation.list), Elevation, error_fun = error_report) %>%
    #assert(in_set(project.list), Project, error_fun = error_report) %>%
    assert(in_set(experiment.list), Experiment, error_fun = error_report) %>%
    assert(in_set(plot.list), Plot, error_fun = error_report) %>%
    assert(in_set(individual.list), Individual_nr, error_fun = error_report) %>%
    assert(in_set(leaf.list), Leaf_nr, error_fun = error_report) %>%

    # check values
    verify(Wet_Mass_g > Dry_Mass_g, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Wet_Mass_g, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Dry_Mass_g, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Leaf_Area_cm2, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Leaf_Thickness_1_mm, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Leaf_Thickness_2_mm, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Leaf_Thickness_3_mm, error_fun = error_report) %>%

  # Observation unique
    group_by(ID) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    assert(in_set(1), n, error_fun = error_report)

}



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

