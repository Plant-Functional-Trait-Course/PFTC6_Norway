# CLEAN DRY MASS AND LEAF AREA

#### DRY MASS ####
clean_dry_mass <- function(raw_dry_mass){

  # Fix wrong IDs
  # raw_dry_mass |>
  #   anti_join(valid_codes, by = c("ID" = "hashcode"))
  #
  # valid_codes |> filter(grepl("5466", hashcode))

  # missing data
  missing_dry_mass <- tibble(ID = c("ILK6566", "GJT9323", "GRN8278", "HJH6874", "FZX5276", "ELR5218", "FZN9661", "FWY2485", "GRC5399", "GCL1857", "GAO0614"),
                             dry_mass = c(0.05624, 0.02181, 0.00853, 0.01432, 0.01137, 0.0121, 0.0162, 0.07055, 0.01618, 0.00967, 0.00363))

  dry_mass <- raw_dry_mass |>
    # remove duplicates
    distinct() |>
    # fix IDs
    mutate(ID = case_when(ID == "AAEG4217" ~ "AEG4217",
                          ID == "BMT1143" ~ "BMT1443",
                          ID == "GUP6912" ~ "GUP5912",
                          ID == "IGW2581" ~ "IGW3581",
                          ID == "ETF1136" ~ "ETF1336",
                          ID == "ETE4809" ~ "ETE4807",
                          ID == "ETF5352" ~ "ETD5352",
                          ID == "ESO07833" ~ "ESO7833",
                          ID == "ICC3201" ~ "IIC3201",
                          ID == "BCG2644" ~ "BGC2644",
                          ID == "HKV3142" ~ "HKV3141",
                          ID == "CEK2662" ~ "CEK2622",
                          ID == "EVY2685" ~ "EVZ2685",
                          ID == "FZD5466" ~ "FYD5466",
                          TRUE ~ ID)) |>

    # Fix wrong and missing values
    mutate(dry_mass = case_when(ID == "DZZ8153" ~ 0.02985,
                                ID == "HCG0783" ~ 0.03877,
                                ID == "IKM2831" ~ 0.02374,
                                ID == "HQH8579" ~ 0.02852,
                                ID == "CVI2987" ~ 0.04331,
                                ID == "DVD1041" ~ 0.01917,
                                ID == "EBE4058" ~ 0.02730,
                                ID == "HCE4621" ~ 0.02379,
                                ID == "GDX1137" ~ 0.02954,
                                ID == "BPE5229" ~ 0.0171,
                                ID == "BDR1013" ~ 0.009,
                                ID == "CRT3828" ~ 0.01129,
                                ID == "CKH3753" ~ 0.00848,
                                ID == "DYT2257" ~ 0.01098,
                                ID == "GIR2712" ~ 0.01553,

                                ID == "BNT7204" ~ 0.00099,
                                ID == "DBR2205" ~ 0.0054,
                                ID == "EAX6890" ~ 0.03641,
                                ID == "EFZ2636" ~ 0.00480,
                                ID == "HBO6894" ~ 0.02337,
                                ID == "IJQ9708" ~ 0.02268,
                                ID == "HTH8109" ~ 0.00635,
                                ID == "GDU4741" ~ 0.00959,
                                ID == "GBG7195" ~ 0.0149,
                                ID == "HWP9676" ~ 0.00059,
                                ID == "EVD5117" ~ 0.06803,
                                ID == "AFH6257" ~ NA_real_, # dry mass is very wrong

                                TRUE ~ dry_mass)) |>
    # add missing dry mass
    bind_rows(missing_dry_mass) |>
    rename(dry_mass_g = dry_mass) |>

    # fix comments
    # flowering comment
    mutate(flowering = if_else(grepl("flower|Flower", remark_dry_weighing), "flower", NA_character_)) |>
    # extract important comments
    mutate(comment_dm = case_when(ID %in% c("BKZ6068", "CRI6709", "CBW8814", "BFF7026", "ALY3867", "CDD3672", "BKY7923", "AHL2709", "APF7189", "AEL0483", "AHP2160", "EJY8296", "DOQ7244", "DOI4478", "HGI7887", "HBT8906") ~ "stem leaves",
                                  ID %in% c("BKB0890", "CTZ8507", "CUX5323", "CYJ9702", "DHT4044", "FZR6893", "DOJ2287") ~ "some stem leaves",

                                  # some issues for dry mass
                                  ID %in% c("DUJ7316", "DYF2988") ~ "petiole missing, dry mass < expected",
                                  ID == "BJA1904" ~ "part of petiole gone, dry mass < expected",
                                  ID %in% c("CYF1214", "CYF1214") ~ "missing some petioles, dry mass < expected",
                                  ID == "GLC4523" ~ "Wet mass maybe off due to moss, wet mass > expected",
                                  grepl("may be missing some material", remark_dry_weighing) ~ "Might be missing parts, dry mass < expected",
                                  grepl("one leaf damaged", remark_dry_weighing) ~ "Damage, dry mass < expected",

                                  # damage
                                  grepl("bud", remark_dry_weighing) ~ "damage_herbivory, dry mass < expected",
                                  grepl("tip has come off|Tip missing", remark_dry_weighing) ~ "damage, dry mass < expected",
                                  grepl("froze damage", remark_dry_weighing) ~ "damage, dry mass < expected",

                                  # add comment when leaves were lost
                                  ID %in% c("BPT4152", "AND1627", "BJU6449", "BDV3827", "ABC7502", "CIO0085", "CYJ9702", "EKL8440", "CZR5069", "BRM7672", "DVJ8460", "GTW7354", "DQC4298", "HWP9676", "EZC2758", "INR2799", "EYP6103", "HEP4020", "DVR2576", "IIB8454", "IKE6687", "HNC6175", "CYW3541") ~ "some leaves gone, recalculate dry mass",

                                  # Issues for all traits: petiole missing completely
                                  ID %in% c("CCM5009", "CXO0734", "CJS6825", "DUJ7316", "IFF0615", "CXI4978", "CXM3140", "CUL6951", "DUT0110", "APL1495", "APP3418", "AQI0874") ~ "petiole missing, area_mass < expected"),

           # fix nr of leaves for dry mass
           nr_leaves_dm = case_when(ID == "BPT4152" ~ 4,
                                    ID == "AND1627" ~ 3,
                                    ID == "BJU6449" ~ 7,
                                    ID == "BDV3827" ~ 2,
                                    ID == "ABC7502" ~ 4,
                                    ID == "CIO0085" ~ 9,
                                    ID == "CYJ9702" ~ 4,
                                    ID == "CYW3541" ~ 7,
                                    ID == "EKL8440" ~ 9,
                                    ID == "CZR5069" ~ 3,
                                    ID == "BRM7672" ~ 3,
                                    ID == "DVJ8460" ~ 4,
                                    ID == "GTW7354" ~ 8,
                                    ID == "DQC4298" ~ 4,
                                    ID == "HWP9676" ~ 4,
                                    ID == "EZC2758" ~ 1,
                                    ID == "INR2799" ~ 2,
                                    ID == "EYP6103" ~ 2,
                                    ID == "HEP4020" ~ 19,
                                    ID == "DVR2576" ~ 3,
                                    ID == "IIB8454" ~ 5,
                                    ID == "IKE6687" ~ 1,
                                    ID == "HNC6175" ~ 4,
                                    ID == "CHW9026" ~ 1,
                                    ID == "BHS3927" ~ 1,
                                    # fix Galium problem where some took leaves and others rosettes
                                    ID == "BPD4783" ~ 18,
                                    ID == "BPP6137" ~ 30,
                                    ID == "BPC8034" ~ 30,
                                    ID == "AHI6936" ~ 30,
                                    FALSE ~ NA_real_
           )) |>
    select(-notes)

  # visualize
  #dry_mass |> ggplot(aes(dry_mass)) + geom_histogram()


}

#######################################################################################

#### LEAF AREA ####

clean_leaf_area <- function(raw_leaf_area, raw_leaf_area_corrected, raw_leaf_area_corrected_2, raw_leaf_area_cropped, scanning_checks){

  # # Check with list of scans if IDs are wrong
  # list.of.files <- dir(path = paste0("raw_data/traits/pftc6_leaf_scans/"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
  #
  # scans <- list.of.files |>
  #   as_tibble() |>
  #   mutate(value = str_remove(value, "raw_data/traits/pftc6_leaf_scans//"),
  #          value = str_remove(value, ".jpeg")) |>
  #   separate(value, into = c("day", "ID"), sep = "/")
  #
  # # list of wrong scan codes (needs checking)
  # scans |>
  #   anti_join(valid_codes, by = c("ID" = "hashcode")) |>
  #   print(n = Inf)

  # comments about scanning
  scanning_checks <- scanning_checks |>
    select(ID, scanning_comment = Comment, supporting_scanning_comment = `Supporting Text Comment`) |>
    filter(!is.na(ID)) |>
    # remove duplicate rows with duplicate comments (will cause problem later when joining)
    filter(!(ID == "AGR2370" & scanning_comment == "Mid_overlapping_folded_leaves")) |>
    filter(!(ID == "DUQ1626" & scanning_comment == "Low_overlapping_folded_leaves")) |>
    filter(!(ID == "EAU3304" & is.na(supporting_scanning_comment))) |>
    filter(!(ID == "EGQ8858" & is.na(supporting_scanning_comment))) |>
    filter(!(ID == "DSL6932" & is.na(supporting_scanning_comment))) |>
    distinct() |>
    # remove unnecessary comments
    filter(!supporting_scanning_comment %in% c("leaf area looks fine on masked file"))

  # scanning_checks |>
  #   anti_join(valid_codes, by = c("ID" = "hashcode"))


  # corrected leaves
  corrected_area <- raw_leaf_area_corrected |>
    bind_rows(raw_leaf_area_corrected_2) |>
    bind_rows(raw_leaf_area_cropped) |>
    mutate(area_comment = case_when(str_detect(dir, "Cora") ~ "corrected invisible area",
                                    ID %in% c("AEE2091_edited", "AFT5418-edited", "ANN5578_edited", "CRU9872-edited", "DYL5087-edited") ~ "corrected invisible area",
                                    str_detect(dir, "Susan") ~ "removed foreign object",
                                    str_detect(dir, "cropping") ~ "additional scan cropping"),
           ID = str_remove(ID, "_edited|-edited|_Edited")) |>
    # remove area correction for DVA0594, otherwise mass area ratio is bad
    # ADG7762: do not use correction, removes leaf sheath, but anyway invisible, so needs only comment that mass area ratio might be wrong
    filter(!ID %in% c("DVA0594", "ADG7762")) |>
    select(-dir, -...1) |>
    rename(corrected_area = leaf_area, corrected_n = n)

  # Check ID: no wrong IDs!!!
  # corrected_area |>
  #   anti_join(valid_codes, by = c("ID" = "hashcode"))


  # leaf area
  leaf_area <- raw_leaf_area |>
    select(-...1) |>
    # add day for 2.8
    mutate(dir = if_else(dir == "raw_data/leaves", "raw_data/traits/pftc6_leaf_scans/2022-08-02", dir)) |>
    # remove real duplicates, wrong scans
    filter(!ID %in% c("DRR2343_2",
                      "ERZ6464_2",
                      "EUA4251",
                      "GFC3042_2",
                      "HNX5249_2",
                      "HRV3559_2",
                      "ACU0226",
                      "CWK5780_2",
                      "CWP1903_2",
                      "CWT6852_2",
                      "BQY3246_2",
                      "BZC0444",
                      "CYW3541_2",
                      "DLJ6129",
                      "DMG7383",
                      "FZA6717_2",
                      "GJB5184_2",
                      "DPP2652_2",
                      "GRX7950_2",
                      "IJI8283",
                      "ETH6232_2",
                      "ETM1024_2",
                      "ETN4156_2",
                      "CIB0635_2",
                      "CIB0635_3",
                      "Test1",
                      "Test2",
                      "Test3",
                      "Test4")) |>
    # rename IDs
    mutate(ID = case_when(ID == "EUA4251_2" ~ "EUA4251",
                          ID == "ACU0226_2" ~ "ACU0226",
                          ID == "BZC0444_in_case_we_dont_have_it_elsewhere" ~ "BZC0444",
                          ID == "DLJ6129_2" ~ "DLJ6129",
                          ID == "DMG7383_2" ~ "DMG7383",
                          ID == "IJI8283_2" ~ "IJI8283",
                          ID == "AGJ3840A" ~ "AGJ3840",
                          ID == "DOI4478-1" ~ "DOI4478",
                          ID == "GYA4910G" ~ "GYA4910",
                          ID == "EDJ2892" ~ "GJN2296",
                          ID == "EGN0308_2" ~ "EGR7522",
                          ID == "out" & dir == "raw_data/traits/pftc6_leaf_scans/2022-07-30" ~ "BGW5255",
                          TRUE ~ ID)) |>

    # remove duplicates = scans from different days.
    # Probably not removed from folder for next day.
    group_by(ID, leaf_area) |>
    slice(1) |>
    ungroup() |>
    # deal with leaves that are almost duplicates
    filter(!(ID == "BMK5896" & grepl("2022-07-26", dir)),
           !(ID == "DAB4969" & grepl("2022-07-27", dir)),
           !(ID == "DYU0930" & grepl("2022-07-28", dir)),
           !(ID == "FWA6854" & grepl("2022-08-03", dir)),
           !(ID == "HHJ2555" & grepl("2022-08-01", dir)),
           !(ID == "HQQ9527" & grepl("2022-08-01", dir)),
           !(ID == "ICE2736" & grepl("2022-08-02", dir)),
           !(ID == "IIV1523" & grepl("2022-08-02", dir)),
           !(ID == "CIB0635" & grepl("2022-07-27", dir))) |>
    # group_by(ID) |>
    # mutate(n = n()) |> filter(n > 1) |> arrange(ID) |> print(n = Inf)

    # check IDs
    # leaf_area |>
    #   anti_join(valid_codes, by = c("ID" = "hashcode"))

    # correct leaf areas for foreign object and upside down leaves (Leaf_side)
    left_join(corrected_area, by = c("ID")) |>
    mutate(leaf_area = if_else(!is.na(corrected_area), corrected_area, leaf_area),
           n = if_else(!is.na(corrected_n), corrected_n, n)) |>
    select(-corrected_n, -corrected_area) |>

    # FIX COMMENTS
    # add other comments from scans where nothing can be done
    left_join(scanning_checks, by = "ID") |>
    mutate(area_comment = if_else(grepl("overlapping", scanning_comment), paste0(scanning_comment, "_area < expected"), area_comment),
           # petiole missing is not damaged, but area < expected. This is already being fixed in dry mass comments
           #scanning_comment = if_else(grepl("petiol|Petiols", supporting_scanning_comment), NA_character_, scanning_comment),
           scanning_comment = if_else(area_comment == "additional scan cropping", "additional scan cropping", scanning_comment),
           # fix some comments
           area_comment = case_when(ID == "ACU0226" ~ "Low_overlapping_folded_leaves_area < expected",
                                    ID %in% c("EFW1927", "GSR4784") ~ "Low_overlapping_folded_leaves_area < expected",
                                    # useful comments that matter for the traits
                                    ID == "CPQ8518" ~ "partly outside_area < expected",
                                    ID == "BIG3139" ~ "bright, area < expected",
                                    ID == "CJS6825" ~ "Mid_overlapping_folded_leaves_area < expected",
                                    ID == "EAV1989" ~ "Low_overlapping_folded_leaves_area < expected, leaf damage potential mass and area problem",
                                    # herbivory
                                    grepl("Herbivory|herbivory|herbivoy|herb and", supporting_scanning_comment) ~ "leaf damage, potential mass and area problem",
                                    grepl("frost damage", supporting_scanning_comment) ~ "leaf damage, potential mass and area problem",
                                    grepl("dark spots", supporting_scanning_comment) ~ "leaf damage, potential mass and area problem",
                                    is.na(area_comment) & scanning_comment == "Damaged_leaf" ~ "leaf damage, potential mass and area problem",
                                    # comment for missing areas that were matched
                                    ID %in% c("GJN2296", "EGR7522", "BGW5255") ~ "Missing area from other leaf, potential wrong ID",
                                    supporting_scanning_comment %in% c("cannot be fixed, flag area", "one damage of 3 leaves", "one is damaged", "1 out of 15") ~ "corrected invisible area",
                                    ID %in% c("DRZ8028", "CUL6951") ~ "corrected invisible area, leaf damage potential mass and area problem",
                                    ID %in% c("APL1495", "APP3418", "AQI0874", "CXI4978") ~ "corrected invisible area, petiole missing, potential mass and area problem",
                                    ID == "DVA0594" ~ "removed foreign object",
                                    TRUE ~ area_comment),
           area_comment = tolower(area_comment)) |>
    # clean up comments
    mutate(scanning_comment = if_else(!is.na(supporting_scanning_comment), paste(scanning_comment, supporting_scanning_comment, sep = "_"), scanning_comment)) |>
    select(-supporting_scanning_comment) |>
    rename(comment_area = area_comment)


}

# Code to check stuff
# valid_codes |> filter(grepl("EOP", hashcode))
# leaf_area |> filter(grepl("CQF", ID))
