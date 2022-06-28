library(dataDownloader)
library(DBI)
library(RSQLite)
library(tidyverse)


# # 3D data
# get_file(node = "pk4bg",
#          file = "THREE-D_Cover_2019_2020.csv",
#          path = "data",
#          remote_path = "Vegetation")
#
# # Høgsete and Vikesland data
# get_file(node = "npfa9",
#          file = "seedclim.2020.4.15.zip",
#          path = "clean_data",
#          remote_path = "3_Community_data")

#### GET COMMUNITY DATA ####
# vcg plant community data
con <- dbConnect(SQLite(), dbname = "clean_data/seedclim.sqlite")

dbListTables(con)

vcg_community <- tbl(con, "turf_community")  |>
  select(-cf, -flag) |>
  left_join(tbl(con, "turfs"), by = "turfID") |>

  # only control plots
  filter(TTtreat %in% c("TTC")) |>
  select(-RTtreat, -GRtreat, -destinationPlotID) |>

  # join plot, block and site IDs
  left_join(tbl(con, "plots"), by = c("originPlotID" = "plotID")) |>
  rename("plotID" = originPlotID) |>
  select(-aspect, -slope) |>
  left_join(tbl(con, "blocks"), by = c("blockID")) |>
  select(-aspect, -slope) |>
  left_join(tbl(con, "sites"), by = c("siteID")) |>
  select(-comment, -norwegian_name, -site_code, -c(biogeographic_zone:precipitation_level)) |>

  # filter 2 sites, and last year
  filter(siteID %in% c("Hogsete", "Vikesland"),
         year == 2019) |>
  left_join(tbl(con, "taxon"), by = "species") |>
  group_by(siteID, elevation, species_name) |>
  summarise(cover = mean(cover)) |>
  rename(species = species_name) |>
  collect()


# 3D
threeD_community <- read_csv("clean_data/THREE-D_Cover_2019-2021.csv") |>
  filter(grazing == "C",
         Nlevel %in% c(1, 2, 3),
         year == 2021) |>
  mutate(destBlockID = as.character(destBlockID),
         elevation = if_else(destSiteID == "Lia", 1290, 920),
         site_warm = paste(destSiteID, warming, "_")) |>
  group_by(siteID = site_warm, species) |>
  summarise(cover = mean(cover))



# incline
seedclim_sp <- tbl(con, "taxon") |> collect()

incline_sp <- read_csv2(file = "clean_data/INCLINE_Species_List_Abbrevs.csv") |>
  select(species = INCLINE_Sp) |>
  mutate(species = str_replace(species, "_", ".")) |>
  left_join(seedclim_sp, by = "species") |>
  filter(!str_detect(species, "cf|CF")) |>
  filter(!str_detect(species, "\\.sp$|\\.sp\\.")) |>
  filter(!species %in% c("Car.sp_den_lyse", "Car.sp_smal", "Pyr.sp_IKKE_rotundifolia", "Fern", "Ranunculus", "Orchid")) |>
  mutate(species_name = if_else(is.na(species_name), species, species_name),
         cover = 1,
         siteID = "Incline") |>
  select(species = species_name, cover, siteID)


incline_community <- read_csv2(file = "clean_data/INCLINE_community_2018_2019_2021.csv")|>
  filter(year == 2021,
         Measure == "cover") |>
  mutate(subPlotID = paste(Site, Block, plot, subPlot, "_")) |>
  select(Site, subPlotID, Ach_mil:Vio_sp) %>%
  mutate(across(c(Ach_mil:Vio_sp), ~as.numeric(.))) |>
  pivot_longer(cols = c(Ach_mil:Vio_sp), names_to = "species", values_to = "cover") |>
  filter(!is.na(cover)) |>
  mutate(species = str_replace(species, "_", ".")) |>
  filter(!str_detect(species, "cf|CF")) |>
  filter(!str_detect(species, "\\.sp$|\\.sp\\.")) |>
  left_join(seedclim_sp, by = "species") |>
  filter(!species %in% c("Car.sp_den_lyse", "Car.sp_smal", "Pyr.sp_IKKE_rotundifolia", "Fern", "Ranunculus", "Orchid")) |>
  mutate(species_name = if_else(is.na(species_name), species, species_name)) |>
  select(siteID = Site, subPlotID, cover, species = species_name) |>
  group_by(siteID, species) |>
  summarise(cover = mean(cover))


####  COMMON ALPINE AND GENERAL SPECIES FOR WHOLE GRADIENT AND ALL SITES ####
community <- bind_rows(
  vcg = vcg_community,
  threeD = threeD_community,
  incline = incline_community,
  .id = "experiment"
) |>
  filter(!is.na(species)) |>
  ungroup() |>
  select(siteID, species, cover) |>
  pivot_wider(names_from = siteID, values_from = cover)


# filter for species that occur in incline
community |>
  filter(
    across(
      .cols = Gudmedalen:Ulvehaugen,
      .fns = ~ !is.na(.x)
    )
  ) |> #writexl::write_xlsx(path = "long_list.xlsx")
  filter(species %in% c(# Incline wishlist (alpine)
                        "Sibbaldia procumbens",
                        "Veronica alpina",
                        "Bistorta vivipara",
                        "Salix herbacea",
                        "Alchemilla alpina",
                        "Agrostis capillaris",
                        "Anthoxanthum odoratum",
                        "Carex bigelowii",
                        "Poa alpina",
                        "Thalictrum alpinum",
                        "Festuca rubra",

                        # other alpines
                        "Saussurea alpina",

                        # expanded sampling (whole elev gradient)
                        "Achillea millefolium",
                        "Agrostis capillaris",
                        "Campanula rotundifolia",
                        "Carex vaginata",
                        "Leontodon autumnalis"
                        )) |> writexl::write_xlsx(path = "short_list.xlsx")


  #writexl::write_xlsx(path = "vcg_threeD_specie_list.xlsx")




#### PLOT LEVEL SPECIES LISTS ####


vcg_plot_community <- tbl(con, "turf_community")  |>
  select(-cf, -flag) |>
  left_join(tbl(con, "turfs"), by = "turfID") |>

  # only control plots
  filter(TTtreat %in% c("TTC")) |>
  select(-RTtreat, -GRtreat, -destinationPlotID) |>

  # join plot, block and site IDs
  left_join(tbl(con, "plots"), by = c("originPlotID" = "plotID")) |>
  rename("plotID" = originPlotID) |>
  select(-aspect, -slope) |>
  left_join(tbl(con, "blocks"), by = c("blockID")) |>
  select(-aspect, -slope) |>
  left_join(tbl(con, "sites"), by = c("siteID")) |>
  select(-comment, -norwegian_name, -site_code, -latitude, -longitude, -c(biogeographic_zone:precipitation_level)) |>

  # filter 2 sites, blocks and last year
  filter(siteID %in% c("Hogsete", "Vikesland"),
         ### CHECK IF THESE ARE OK!!!
         blockID %in% c("Hog1", "Hog2", "Hog3", "Vik3", "Vik4", "Vik5"),
         year == 2019,
         species != "NID.seedling") |>
  left_join(tbl(con, "taxon"), by = "species") |>
  select(siteID, blockID, turfID, species = species_name, cover) |>
  mutate(warming = "ambient",
         grazing = "C",
         Namount_kg_ha_y = 0) |>
  collect()

# vcg_plot_community |>
#   filter(siteID == "Vikesland") |>
#   select(blockID, species, cover) |>
#   pivot_wider(names_from = blockID, values_from = cover)


NitrogenDictionary <- tibble(Nlevel = c(1,6,5,3,10,7,4,8,9,2),
                             Namount_kg_ha_y = c(0, 5, 1, 0, 150, 10, 0.5, 50, 100, 0))

threeD_plot_community <- read_csv("clean_data/THREE-D_Cover_2019-2021.csv") |>
  left_join(NitrogenDictionary, by = "Nlevel") |>
  filter(year == 2021 & grazing == "N" & Namount_kg_ha_y == 0 |
           year == 2021 & grazing == "C" & Namount_kg_ha_y %in% c(0, 5, 10, 50, 150)
         ) |>
  mutate(destBlockID = as.character(destBlockID),
         elevation = if_else(destSiteID == "Lia", 1290, 920)) |>
  select(siteID = destSiteID, blockID = destBlockID, turfID, warming, grazing, Namount_kg_ha_y, species, cover)



bind_rows(
  vcg = vcg_plot_community,
  threeD = threeD_plot_community,
  .id = "experiment"
) |>
  group_by(turfID) |>
  arrange(experiment, siteID, blockID, turfID, -cover) |>
  mutate(cumsum = cumsum(cover)) |>
  filter(cumsum <= 90) |> ungroup() |> count(experiment)



