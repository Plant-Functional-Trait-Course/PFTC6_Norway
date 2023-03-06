###########################
### CALCULATE LEAF AREA ###
###########################


#### LOAD LIBRARY
#devtools::install_github("richardjtelford/LeafArea")
library(LeafArea)


#### Function to calculate leaf area
loop.files <-  function(files){

  file.copy(files, new.folder)
  if(grepl("-NA$", files)){
  newfile <- basename(files)
  file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
  "/", gsub("-NA$", "", newfile)))
  }
  print(files)
  area <- try(run.ij(set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 60, trim.pixel2 = 150, save.image = TRUE))
  # more cropping
  #area <- try(run.ij(set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 200, trim.pixel2 = 0, save.image = TRUE))

  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(dir = dirname(files), ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}

# list.of.files <- dir(path = paste0("raw_data/leaves/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
# new.folder <- "raw_data/Temp/"
# output.folder <- "raw_data/out"
# LeafArea.raw_total <- plyr::ldply(list.of.files, loop.files)

# test run.ij
dd <- run.ij(set.directory = "raw_data/traits/pftc6_leaf_scans/not_working/", distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.1, trim.pixel = 58, trim.pixel2 = 150, save.image = TRUE)
# dd$summary


###########################################################################
#### Calculate leaf area for 2022 data
# make a list of files, temporary folder and output folder
list.of.files <- dir(path = paste0("raw_data/traits/pftc6_leaf_scans/2022-07-24/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "raw_data/traits/Temp/"
output.folder <- "raw_data/traits/output/"


# Run function
LA_24 <- plyr::ldply(list.of.files, loop.files)

LA <- read_csv(file = "raw_data/traits/PFTC6_LA.csv")
LA <- bind_rows(LA, LA_24)
LA <- LA |>
  select(-c(...1, ...2, ...3, ...4, ...5, ...6, ...7, ...8, ...9)) |>
  # remove na's
  filter(!is.na(dir)) |>
  tidylog::filter(!grepl("mask", ID)) |>
  # remove duplicate IAU6402
  distinct()
write_csv(LA, file = "raw_data/traits/PFTC6_LA.csv")

# Check for all the leaves that have "mask". Scan is duplicated and area is calculated many times
# LA |>
#   filter(grepl("mask", ID)) |>
#   print(n = Inf)


# calculate sums
leaf_area <- LA |>
  # extract everything before point
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))

# save data as csv
dim(LeafArea.raw)
write.csv(leaf_area, file = "raw_data/traits/PFTC6_leaf_area_2022.csv")

leaf_area <- read_csv(file = "raw_data/traits/PFTC6_leaf_area_2022.csv")


###########################################################################
#### Calculate corrected leaves for 2022 data
# make a list of files, temporary folder and output folder
list.of.files <- dir(path = paste0("raw_data/traits/pftc6_leaf_scans/corrected_leaves/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "raw_data/traits/Temp/"
output.folder <- "raw_data/traits/output_corrected"

# Run function
LeafArea.raw_total <- plyr::ldply(list.of.files, loop.files)


# calculate sums
leaf_area_corr <- LeafArea.raw_total |>
  # extract everything before point
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))

# save data as csv
write.csv(leaf_area_corr, file = "raw_data/traits/PFTC6_leaf_area_corrected_2022.csv")
write.csv(leaf_area_corr, file = "raw_data/traits/PFTC6_leaf_area_corrected_2_2022.csv")


###########################################################################
#### Calculate leaves that need more cropping (trim.pixel = 200)

# check scans where ruler was included and n i huge
# e.g. EAB and EBT
# needs cropping and reanalysing area
leaf_area |> filter(n > 20) |> print(n = Inf)


# make a list of files, temporary folder and output folder
list.of.files <- dir(path = paste0("raw_data/traits/pftc6_leaf_scans/need_cropping/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "raw_data/traits/Temp/"
output.folder <- "raw_data/traits/output"

# Run function
LA_crop <- plyr::ldply(list.of.files, loop.files)

# calculate sums
leaf_area_corr <- LA_crop |>
  tidylog::filter(!grepl("mask", ID)) |>
  # remove duplicate
  distinct() |>
  # extract everything before point
  mutate(ID = sub("\\..*", "", ID)) |>
  group_by(dir, ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))

# save data as csv
write.csv(leaf_area_corr, file = "raw_data/traits/PFTC6_leaf_area_cropping_2022.csv")
