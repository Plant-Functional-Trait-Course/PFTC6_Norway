###########################
### CALCULATE LEAF AREA ###
###########################


#### LOAD LIBRARY
devtools::install_github("richardjtelford/LeafArea")
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
  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}


# test run.ij
run.ij(set.directory = "raw_data/leaf_scans/pftc6_leaf_scans", distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.1, trim.pixel = 58, trim.pixel2 = 150, save.image = TRUE)



###########################################################################
#### Calculate leaf area for 2022 data
# make a list of files, temporary folder and output folder
list.of.files <- dir(path = paste0("raw_data/leaf_scans/pftc6_leaf_scans/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "raw_data/Temp/"
output.folder <- "raw_data/output"

# Run function
LeafArea.raw_total <- plyr::ldply(list.of.files, loop.files)

# calculate sums
leaf_area <- LeafArea.raw_total |>
  mutate(ID = substr(ID, 1, 7)) |>
  group_by(ID) |>
  summarise(n = n(),
            leaf_area = sum(LeafArea))

# save data as csv
dim(LeafArea.raw)
write.csv(leaf_area, file = "leaf_area.csv")

