# Species list for Flora


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
  distinct(species = species_name) |>

  collect()


# 3D
threeD_community <- read_csv("clean_data/THREE-D_Cover_2019-2021.csv") |>
  filter(warming == "A",
         grazing == "C",
         Nlevel %in% c(1, 2, 3),
         year == 2021) |>
  mutate(destBlockID = as.character(destBlockID),
         elevation = if_else(destSiteID == "Lia", 1290, 920)) |>
  distinct(species)


# incline
seedclim_sp <- tbl(con, "taxon") |> collect()

incline_sp <- read_csv2(file = "clean_data/INCLINE_Species_List_Abbrevs.csv") |>
  select(species = INCLINE_Sp) |>
  mutate(species = str_replace(species, "_", ".")) |>
  left_join(seedclim_sp, by = "species") |>
  filter(!str_detect(species, "cf|CF")) |>
  filter(!str_detect(species, "\\.sp$|\\.sp\\.")) |>
  filter(!species %in% c("Car.sp_den_lyse", "Car.sp_smal", "Pyr.sp_IKKE_rotundifolia", "Fern", "Ranunculus", "Orchid")) |>
  mutate(species_name = if_else(is.na(species_name), species, species_name)) |>
  select(species = species_name)



flora_pftc <- bind_rows(
  vcg = vcg_community,
  threeD = threeD_community,
  incline = incline_sp,
  .id = "experiment"
) |>
  filter(!is.na(species)) |>
  mutate(presence = "x") |>
  pivot_wider(names_from = experiment, values_from = presence) |>
  arrange(species)

write_csv(flora_pftc, "flora_pftc.csv")

### FunCaB

flora_funcab <- read_csv("clean_data/FunCaB_taxon_table.csv") |>
  select(functional_group, family, species_name, community) |>
  filter(!is.na(community)) |>
  arrange(functional_group, species_name)

write_csv(flora_funcab, "flora_funcab.csv")
