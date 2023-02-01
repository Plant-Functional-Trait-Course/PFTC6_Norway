# Package and function to use
#install.packages("exifr")
library(exifr)

# And these are the settings we expect:
# resolution <- 300
# imageHeight <- 3508
# imageWidth <- 2552
# tolerance <- 20

# all scans
exif_data_scans <- exifr::read_exif(
  path = "raw_data/traits/pftc6_leaf_scans/",
  recursive=TRUE,
  tags = c("XResolution", "YResolution", "BitsPerSample", "ImageHeight", "ImageWidth"))

# check if resolution is ok
# problem with one of Coras leaves, and all cropped leaves
exif_data_scans |>
  mutate(ID = basename(SourceFile),
         file = str_remove(SourceFile, "raw_data/traits/pftc6_leaf_scans/"),
         file = str_remove(file, pattern = "\\/.*")) |>
  # group_by(SourceFile) |>
  # count(XResolution, YResolution)
  filter(file == "need_cropping") |>
  select(ID) |> print(n = Inf)


exif_data_scans |>
  ggplot(aes(x = XResolution, y = YResolution)) +
  geom_point()


# check output (all have the same resolution)
exif_data_out <- exifr::read_exif(
  path = "raw_data/traits/output_old/",
  recursive=TRUE,
  tags = c("XResolution", "YResolution", "BitsPerSample", "ImageHeight", "ImageWidth"))


exif_data_out |>
  mutate(ID = basename(SourceFile),
         SourceFile = str_remove(SourceFile, "raw_data/traits/output_old/"),
         SourceFile = str_remove(SourceFile, pattern = "\\/.*")) |>
  ggplot(aes(x = XResolution, y = YResolution)) +
  geom_point()


exifr::read_exif(
  path = "raw_data/traits/pftc6_leaf_scans/need_cropping/",
  recursive=TRUE,
  tags = c("XResolution", "YResolution", "BitsPerSample", "ImageHeight", "ImageWidth")) |>
  print(n = Inf)
