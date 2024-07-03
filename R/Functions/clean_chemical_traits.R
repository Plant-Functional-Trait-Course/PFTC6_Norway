#########################
#### Chemical Traits ####
#########################

# clean chem traits

clean_chem_traits <- function(leaf_traits_Incline){

  ############################################################################
  #### PHOSPHORUS DATA ####

  p_files <- dir(path = "raw_data/P_PCFT6/", pattern = "\\.xlsx$", full.names = TRUE)

  standards <- p_files %>%
    set_names() %>%
    map_df(., ~ {read_excel(.x, skip = 1, n_max = 7, col_types = "text")},
           .id = "filename") |>
    clean_names() |>
    select(filename, standard = x4, first_standard_absorbance, second_standard_absorbance) |>
    filter(standard != "R2")

  all_p <- p_files %>%
    set_names() %>%
    map_df(., ~ {read_excel(.x, skip = 11)},
           .id = "filename") |>
    clean_names() |>
    select(ID = sample_id, site, sample_absorbance, sample_mass, volume_of_sample_ml, filename)

  p <- all_p |>
    # keep red wheat
    mutate(ID = if_else(site == "Hard Red Spring Wheat Flour", "red", ID)) |>
    tidylog::filter(!is.na(ID)) |>
    mutate(ID = case_when(ID == "EGX4796" ~ "EGX4795",
                          ID == "EJ16479" ~ "EJI6479",
                          ID == "DZM2566" ~ "DZM2568",
                          ID == "ERB5287" ~ "EBR5287",
                          ID == "DIG7541" ~ "DIG7451",
                          ID == "DLX6663_DLT9027_DLP7535" ~ "DLX6663_DLT9027_DLP7533",
                          ID == "HFD7606_HDI1092" ~ "HFD7606_HID1092",
                          ID == "DIA_EKX6404" ~ "DIA1838_EKX6404",
                          ID == "R" ~ "DZZ8153",
                          TRUE ~ ID))

  # Check IDs (all seem to be fine)
  # all_codes <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)
  #
  # p |>
  #   separate(col = ID, into = c("ID1", "ID2", "ID3"), sep = "_") |>
  #   #filter(!is.na(ID1)) |>
  #   anti_join(all_codes, by = c("ID1" = "hashcode"))
  #
  # all_codes |> filter(str_detect(hashcode, "DIA"))
  # cn_isotopes |> filter(str_detect(ID, "DIG7451"))


  # pull of standard, calculate R2, choose standard for absorbance curve, make regression and plot
  standard_concentration <- tibble(standard = c(0, 2, 4, 8, 12, 16),
                                   concentration = c(0, 0.061, 0.122, 0.242, 0.364, 0.484))

  Standard <- standards %>%
    pivot_longer(cols = c(first_standard_absorbance, second_standard_absorbance), names_to = "replicate", values_to = "sample_absorbance") |>
    mutate(standard = as.numeric(standard),
           replicate = str_remove(replicate, "_standard_absorbance"),
           sample_absorbance = as.numeric(sample_absorbance)) |>
    arrange(filename, replicate) |>
    left_join(standard_concentration, by = "standard")

  # Plot 2 Standard curves
  # Standard %>%
  #   unnest() %>%
  #   ggplot(aes(x = Sample_Absorbance, y = Concentration, colour = ID)) +
  #   geom_point() +
  #   geom_smooth(method = "lm", se = FALSE) +
  #   labs(x = "Absorbance", y = expression(paste("Concentration ", mu, "g/ml"))) +
  #   facet_wrap(~ Batch)



  # Choose standard and make model
  ModelResult <- Standard %>%
    group_by(filename, replicate) |>
    nest() |>
    mutate(correlation = map_dbl(data, ~ {cor(.x$sample_absorbance, .x$concentration, use = "pair")})) %>%
    group_by(filename) %>%
    slice(which.max(correlation)) |>
    mutate(fit = map(data, ~lm(concentration ~ sample_absorbance, .)))

  # Calculate Mean, sd, coeficiant variability for each leaf and flag data
  p2 <- p %>%
    group_by(filename) %>%
    nest() %>%
    # add estimate from model
    left_join(ModelResult |>
                select(-data), by = c("filename"))


  OriginalValues <- p2 %>%
    mutate(data = map2(.x = data, .y = fit, ~ mutate(.x, sample_yg_ml = predict(.y, newdata = select(.x, sample_absorbance))))) %>%
    unnest(data) %>%
    mutate(p_mass = sample_yg_ml * volume_of_sample_ml,
           p_conc = p_mass / sample_mass * 100) %>%
    # Calculate mean, sd, coefficient of variation
    group_by(filename, ID) %>%
    mutate(p_mean = mean(p_conc, na.rm = TRUE),
           p_sd = sd(p_conc, na.rm = TRUE),
           p_coeff_var = p_sd / p_mean) %>%
    # flag data
    mutate(flag_orig = ifelse(p_coeff_var >= 0.2, "flag", ""))


  # wheat: check values, flag/remove, calculate convertion factor
  RedWheatValue <- 0.137
  CorrectionFactor <- OriginalValues %>%
    filter(ID %in% c("red")) %>%
    mutate(p_correction = p_conc / RedWheatValue) %>%
    # Calculate mean, sd, coefficient of variation
    group_by(filename, ID) %>%
    summarise(correction_factor = mean(p_correction, na.rm = TRUE)) %>%
    select(-ID)


  # Use Correction Factor on data
  Corrected_P <- OriginalValues %>%
    filter(!ID %in% c("red")) %>%
    tidylog::left_join(CorrectionFactor, by = c("filename")) %>%
    mutate(p_conc_corrected = p_conc * correction_factor) %>%
    # Calculate mean, sd, coefficient of variation
    group_by(filename, ID) %>%
    summarise(p_percent = mean(p_conc_corrected, na.rm = TRUE),
              p_sd = sd(p_conc_corrected, na.rm = TRUE),
              p_coeff_var = p_sd / p_mean,
              nrep = n()) %>%
    tidylog::distinct() |>
    ungroup() |>
    # flag data
    mutate(flag = ifelse(p_coeff_var >= 0.2, "coeff. of variation > 0.2", NA_character_)) |>
    # remove unusable data
    tidylog::filter(p_percent < 1) |>
    tidylog::filter(p_percent > 0) |>
    mutate(merged = if_else(str_detect(ID, "_"), "merged", NA_character_),
           trait = "p_percent") |>
    select(ID, trait, value = p_percent, merged, filename, flag)


  ############################################################################
  #### ISOTOPE DATA ####
  # import CN and isotope data and merge
  # Read isotope data
  list_files <- dir(path = "raw_data/PCFT6_CN_and_Isotopes/", pattern = "\\.xlsx$", full.names = TRUE)

  cn <- list_files %>%
    set_names() %>%
    map_df(., ~ {read_excel(.x, skip = 13, col_types = "text")},
           .id = "filename") |>
    clean_names() |>
    filter(!is.na(x1)) |> # removing empty rows
    select(-c(x11:d13c_vpdb)) |>
    rename(ID = sample_id, site = x3, d15n = d15n_atm, d13c = d13c_pdb) |>
    select(ID, site, c_percent, n_percent, c_n, d15n, d13c, everything()) |>
    filter(!is.na(ID)) |>
    mutate(ID = case_when(ID == "EGX4796" ~ "EGX4795",
                          ID == "EJ16479" ~ "EJI6479",
                          ID == "GPZ4087_GPI5077" ~ "GPQ4087_GPI5077",
                          ID == "HSJ3702_EQD7138" ~ "HJS3702_EQD7138",
                          TRUE ~ ID))


  cn_isotopes <- cn |>
    pivot_longer(cols = c(c_percent:d13c), names_to = "trait", values_to = "value") |>
    mutate(value = if_else(value %in% c("REPEAT", "Empty Cell"), NA_character_, value),
           value = as.numeric(value)) |>
    filter(!is.na(value)) |>
    select(ID, site, trait, value, filename)


  # Check IDs (all seem to be fine)
  #   all_codes <- get_PFTC_envelope_codes(seed = 49, as.3.5 = FALSE)
  #
  # cn_isotopes |>
  #   separate(col = ID, into = c("ID1", "ID2", "ID3"), sep = "_") |>
  #   filter(!is.na(ID3)) |>
  #   anti_join(all_codes, by = c("ID3" = "hashcode"))


  # cn_isotopes |>
  #   ggplot(aes(x = value)) +
  #   geom_density() +
  #   facet_wrap(~ traits, scales = "free")

  ############################################################################
  ### MERGE ###

  incline_id <- leaf_traits_Incline |>
    distinct(ID, date, siteID, elevation_m_asl, blockID, warming, species, individual_nr)

  cnp_data <- bind_rows(cn_isotopes, Corrected_P) |>
    mutate(ID_merged = ID) |>
    separate(col = ID, into = c("ID1", "ID2", "ID3"), sep = "_") |>
    pivot_longer(cols = c(ID1, ID2, ID3), names_to = "nr", values_to = "ID") |>
    filter(!is.na(ID)) |>
    tidylog::left_join(incline_id, by = "ID") |>
    mutate(project = if_else(siteID %in% c("Gudmedalen", "Skjelingahaugen", "Ulvehaugen"), "Incline", "3D")) |>
    tidylog::select(project, ID, ID_merged, date, siteID, elevation_m_asl, blockID, warming, individual_nr, species, trait, value, merged, flag)


}

# cnp_data |>
#   ggplot(aes(x = value, fill = siteID)) +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~ trait, scales = "free")
#
# d <- read_csv("cnp/PFTC6_3D_cnp_traits_warmingTreatment_5-may-2023.csv")
# i <- read_csv("cnp/PFTC6_Incline_cnp_traits_5-may-2023.csv")
#
# d |> distinct(species) |>
#   full_join(i |> distinct(species))
#
# # 461
#  # 968

