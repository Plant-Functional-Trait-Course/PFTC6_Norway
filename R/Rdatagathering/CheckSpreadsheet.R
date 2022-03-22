### CHECK DATA IN SPREADSHEET 

# Load libraries
library("tidyverse")
library("lubridate")
library("readxl")
library("assertr")
library("gridExtra")

# get all valid IDs
load("traits/Rdatagathering/envelope_codes.Rdata")
all_codes

# Read in several tables
#myfiles <- dir(path = paste0("traits/data"), pattern = "xlsx", recursive = TRUE, full.names = TRUE)
#mdat <- map_df(myfiles, function(n) read_excel(path = n, col_names = TRUE))


# Data lists
nr.col <- 20
ID.list <- all_codes$hashcode # MAKE AN OPTION TO SLECT CERTAIN ROWS
date.list <- ymd(c("2018-03-23"))
character.list <- c("Site", "Genus", "Species", "Experiment")
numeric.list <- c("Elevation", "Plot", "Individual_nr", "Leaf_nr", "Leaf_Area_cm2", "Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_1_mm", "Leaf_Thickness_2_mm", "Leaf_Thickness_3_mm")
site.list <- c("WAY", "AJA", "PIL", "TRE")
elevation.list <- c(3085, 3450, 3670, 3900)
genus.list <- c()
species.list <- c()
project.list <- c("Local", "Experiment", "Leaf_T")
experiment.list <- c("None", "Burnt", "Unburnt", "Grazed", "Ungrazed")
plot.list <- c(1:5)
individual.list <- c(1:5)
leaf.list <- c(1:5)

# set bounds for WetMass, DryMass, etc
sla.upper <- 500



#### Function to test trait data ####
CheckSpreadsheet <- function(dat){

  out <- dat %>%
    filter(!is.na(ID)) %>% 
    mutate(Date = as.Date(Date)) %>% 
    verify(ncol(.) == nr.col, error_fun = error_report) %>% # check number of columns
    assert(function(x) nchar(x)==7, ID, error_fun = error_report) %>% # check length of ID
    assert(in_set(ID.list), ID, error_fun = error_report) %>% # check if ID is valid
    assert(is.character, character.list, error_fun = error_report) %>%  # is a character
    assert(is.numeric, numeric.list, error_fun = error_report) %>% # is numeric
    # Check if Variables only contain elements from lists defined above
    assert(in_set(date.list), Date, error_fun = error_report) %>% 
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

