# reporting

# gradient
leaf_traits_3D |>
  filter(gradient == "gradient") |>
  distinct(species) |>
  arrange(species) |>
  print(n = Inf)
# gradient
# 54 species
# n = 8,086 trait observations
# 1171 leaves

# observations per site and treatment
leaf_traits_3D |>
  filter(gradient == "gradient") |>
  count(siteID, grazing) |>
  arrange(siteID)


#ThreeD
leaf_traits_3D |>
  # remove gradient plots
  filter(!siteID == "Hogsete",
         !(siteID == "Vikesland" & warming == "A")) |>
  distinct(species) |>
  arrange(species)
# 55 species
# 11949 trait observations
# 1734

# observations per site and treatment
leaf_traits_3D |>
  # remove gradient plots
  filter(!siteID == "Hogsete",
         !(siteID == "Vikesland" & warming == "A")) |>
  count(siteID, warming, grazing, Nlevel) |>

  # group_by(siteID) |>
  # summarise(mean = mean(n),
  #           se = sd(mean)/sqrt(n))
  arrange(siteID, warming, grazing) |>
  print(n = Inf)

leaf_traits_Incline |>
  distinct(species) |>
  arrange(species) |>
  print(n = Inf)
# 38 sp
# n = 8795
leaf_traits_Incline |>
distinct(ID)
# 1293 leaves

leaf_traits_Incline |>
  count(siteID, warming) |>
  group_by(warming) |>
  summarise(mean(n))

leaf_traits_Incline |>
  group_by(warming) |>
  distinct(species) |>
  count()
