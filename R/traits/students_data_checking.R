####################################
#### CODE TO CHECK SPREADSHEETS ####
####################################

# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(validate)
library(PFTCFunctions)
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")


#### Read in spreadsheet ####

# download raw trait data from OSF
get_file(node = "pk4bg",
         file = "PFTC6_Norway_Leaf_traits_2022.xlsx",
         path = "raw_data/traits/",
         remote_path = "RawData/Traits")

raw_traits <- read_excel(path = "raw_data/traits/PFTC6_Norway_Leaf_traits_2022.xlsx", sheet = "Data")


### get all valid IDs (seed for PFTC6 is 49)
uniqueIDs <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)


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




# plot data

ggplot(raw_traits, aes(x = wet_mass_g, y = leaf_area)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() +
  scale_y_log10()


ggplot(raw_traits, aes(x = leaf_thickness_1_mm, y = leaf_thickness_3_mm)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_log10() +
  scale_y_log10()



#_______________________________________________________________________________

#### CHECKING DATA, OUTLIERS ETC. ####

# some outliers for area vs mass
# looks good
clean_traits |>
  ggplot(aes(x = leaf_area_cm2, y = dry_mass_g, colour = siteID)) +
  geom_point()

clean_traits |>
  ggplot(aes(x = leaf_area_cm2, y = wet_mass_g, colour = siteID)) +
  geom_point()

clean_traits |>
  ggplot(aes(x = dry_mass_g, y = wet_mass_g, colour = taxon)) +
  geom_point() +
  theme(legend.position = "none")

# 1 strange value
clean_traits |>
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
clean_traits |>
  ggplot(aes(x = dry_mass_g, y = ldmc, shape = siteID, colour = ldmc > 1)) +
  geom_point()

clean_traits |>
  ggplot(aes(x = dry_mass_g, y = sla_cm2_g, shape = siteID, colour = sla_cm2_g > 600)) +
  geom_point()
clean_traits |> filter(sla_cm2_g > 600) |> select(ID:plotID, dry_mass_g, wet_mass_g, ldmc, leaf_area_cm2, sla_cm2_g, comment, flag) |> View()

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

