# PREVIOUS POSITIVE TESTS
# Only select tests of individuals present in the SGTF dataset and exclude tests after the study period
data_teststraten_selection <-  data_teststraten_all %>%
  # filter on all positive samples from cases included in the study
  filter.(
    Uitslag == "Positief" &
      Afspraak_start_datum <= data_teststraten_lab_org$Afspraak_start_datum %>% max &
      # voor een snellere koppeling alleen relevant IDs mee.
      Pseudoniem %in% data_teststraten_lab_org$Pseudoniem &
      !is.na(Pseudoniem)
  ) %>%
  distinct.(Monsternummer, .keep_all = T)

previous_infections <- data_teststraten_selection %>%
  select(Pseudoniem, Monsternummer, Afspraak_start_datum) %>%
  # Left join met SGTF en WGS data
  arrange.(Pseudoniem, Afspraak_start_datum) %>%
  mutate.(interval_afspraken = as.numeric(Afspraak_start_datum - lag(Afspraak_start_datum)),
          .by = Pseudoniem) %>%
  filter.(interval_afspraken > 29 | is.na(interval_afspraken)) %>%
  mutate.(
    n_pos_tests = n(),
    lag_monsternummer = lag(Monsternummer),
    lag_afspraak_start_datum = lag(Afspraak_start_datum),
    .by = Pseudoniem
  ) %>%
  left_join.(
    data_SGTF_org %>%
      select.(lag_monsternummer = Monsternummer, previous_SGTF = `S result`) %>%
      filter.(
        previous_SGTF %in% c("Detected", "Not detected") &
          !is.na(lag_monsternummer)
      ),
    by = "lag_monsternummer"
  ) %>%
  left_join.(
    data_kiemsurv %>%
      filter(!is.na(`CoronIT Id`)) %>%
      select.(
        lag_monsternummer = `CoronIT Id`,
        previous_variant_WGS = `Variant (WGS)`
      ),
    by = "lag_monsternummer"
  ) %>%
  mutate.(
    # Determined by WGS or SGTF + date
    previous_variant = case_when(
      # Pre VOC
      previous_variant_WGS == "Pre-VOC" |
        (
          previous_SGTF == "Detected" &
            lag_afspraak_start_datum %in% (
              koppeltabel_variant_S_result %>%
                filter(Variant == "Pre-VOC") %>%
                summarise(seq(min_date_90, max_date_90, 1)) %>% pull
            )
        ) ~ "Pre-VOC",
      previous_variant_WGS == "Alpha" |
        (
          previous_SGTF == "Not detected" &
            lag_afspraak_start_datum %in% (
              koppeltabel_variant_S_result %>%
                filter(Variant == "Alpha") %>%
                summarise(seq(min_date_90, max_date_90, 1)) %>% pull
            )
        ) ~ "Alpha",
      previous_variant_WGS == "Delta" |
        (
          previous_SGTF == "Detected" &
            lag_afspraak_start_datum %in% (
              koppeltabel_variant_S_result %>%
                filter(Variant == "Delta") %>%
                summarise(seq(min_date_90, max_date_90, 1)) %>% pull
            )
        ) ~ "Delta",
      previous_variant_WGS == "Omicron BA.1" |
        (
          previous_SGTF == "Not detected" &
            lag_afspraak_start_datum %in% (
              koppeltabel_variant_S_result %>%
                filter(Variant == "Omicron BA.1") %>%
                summarise(seq(min_date_90, max_date_90, 1)) %>% pull
            )
        ) ~ "Omicron BA.1",
      previous_variant_WGS == "Omicron BA.2" |
        (
          previous_SGTF == "Detected" &
            lag_afspraak_start_datum %in% (
              koppeltabel_variant_S_result %>%
                filter(Variant == "Omicron BA.2") %>%
                summarise(seq(min_date_90, max_date_90, 1)) %>% pull
            )
        ) ~ "Omicron BA.2",
      
      previous_variant_WGS == "Omicron BA.4" ~ "Omicron BA.4" ,
      previous_variant_WGS == "Omicron BA.5" ~ "Omicron BA.5"
    ) %>% factor(
      levels = c("Pre-VOC", "Alpha", "Delta", "Omicron BA.1", "Omicron BA.2")
    )
  ) %>%
  # take last positive test
  arrange(desc(Afspraak_start_datum)) %>%
  distinct(Pseudoniem, .keep_all = T)

datum_start_vaccinatie_booster <- as_date("2021-11-18")

data_teststraten_lab <- data_teststraten_lab_org %>%
  left_join.(previous_infections %>% select(!c(Afspraak_start_datum, Pseudoniem)),
             by = "Monsternummer") %>%
  left_join.(
    data_teststraten_all %>% filter(Monsternummer %in% data_teststraten_lab_org$Monsternummer) %>%
      select(
        Monsternummer,
        Vaccinatie_status,
        Vaccinatie_aantal,
        Vaccinatie_merk,
        Vaccinatie_datum_laatste,
        Vaccinatie_eerder_janss,
        Leeftijd,
        Geslacht,
        Teruggekeerd_uit_buitenland,
        Veiligheidsregio,
        Leeftijdsgroep10 = Leeftijdsgroep,
        Na_Zelftest,
        Is_gevaccineerd,
        Vaccinatie_DUFS_EZD
      ),
    by = "Monsternummer"
  ) %>%
  mutate.(
    `S result` = `S result` %>% factor(levels = c("Detected", "Not detected")),
    Uitslag_S = if_else(Uitslag == "NEGATIEF", "Negatief", `S result`) %>%
      factor(levels = c("Negatief", "Detected", "Not detected")),
    
    Afspraak_start_week = floor_date(Afspraak_start_datum, unit = "week", week_start = 1),
    
    Previous_infection = if_else(!is.na(lag_monsternummer),
                                 1,
                                 0) %>% replace_na(0),
    # txt var
    reinfection = ifelse.(
      Previous_infection == 1,
      "Previous infection",
      "No previous infection"
    ),
    
    Vaccinatie_merk = factor(
      case_when(
        Vaccinatie_status == "Ongevaccineerd" ~ "Geen",
        Vaccinatie_merk %>% str_detect("COM") ~ "COM",
        TRUE ~    Vaccinatie_merk %>% as.character()
      ),
      levels = c("COM", "MOD", "AZ", "JANSS", "Geen", "UNK")
    ),
    
    Vaccstatus = case_when(
      Vaccinatie_status == "Ongevaccineerd" ~ 0,
      Vaccinatie_status == "Deels"          ~ 1,
      # Individuals with 2-doses, vaccinated after the start of the booster campaign
      # with an unknown status if they recived JANSS as their first dose (variable implemented end of January 2022) and
      # vaccinated with a vaccin used for the booster.
      # we now label them as booster
      Vaccinatie_status == "Volledig" &
        is.na(Vaccinatie_eerder_janss) &
        Vaccinatie_datum_laatste >= datum_start_vaccinatie_booster &
        # is 2021-11-18
        Vaccinatie_aantal == 2 &
        Vaccinatie_merk %in% c("COM", "MOD") ~ NA,
      Vaccinatie_status == "Volledig"       ~ 2,
      Vaccinatie_status == "Booster"        ~ 3,
      Vaccinatie_status == "Herhaalprik"        ~ 4
    ),
    Immuunstatus =
      factor(
        case_when(
          Previous_infection == 0 & Vaccstatus == 0 ~ "Naive",
          Previous_infection == 1 &
            Vaccstatus == 0 ~ "Previous infection,\nunvaccinated",
          Previous_infection == 0 &
            Vaccstatus == 2 ~ "Primary vaccination",
          Previous_infection == 1 &
            Vaccstatus == 2 ~ "Previous infection,\nprimary vaccination",
          Previous_infection == 0 & Vaccstatus == 3 ~ "Booster",
          Previous_infection == 1 &
            Vaccstatus == 3 ~ "Previous infection,\nbooster",
          Previous_infection == 0 &
            Vaccstatus == 4 ~ "Other vaccination\n(>3 dosis)",
          Previous_infection == 1 &
            Vaccstatus == 4 ~ "Previous infection,\nOther vaccination\n(>3 dosis)",
          TRUE ~ "Unknown"
        ),
        levels = c(
          "Naive",
          "Previous infection,\nunvaccinated",
          "Primary vaccination",
          "Previous infection,\nprimary vaccination",
          "Booster",
          "Previous infection,\nbooster",
          "Other vaccination\n(>3 dosis)",
          "Previous infection,\nOther vaccination\n(>3 dosis)",
          "Unknown"
        ),
        
      ),
    vacc_status = case_when(
      Vaccstatus ==  4 ~ "Other (>3 dosis)",
      Vaccstatus == 3 ~ "Booster\nvaccination",
      Vaccstatus == 2 ~ "Primary\nvaccination",
      Vaccstatus == 1 ~ "Partial\nvaccination",
      Vaccstatus == 0 ~ "Unvaccinated",
      TRUE ~ "Unknown"
    ) %>%
      factor(
        levels = c(
          "Other (>3 dosis)",
          "Booster\nvaccination",
          "Primary\nvaccination",
          "Partial\nvaccination",
          "Unvaccinated",
          "Unknown"
        )
      ),
    Leeftijdsgroep = Leeftijd %>%
      cut(
        breaks = c(0, 18, 30, 50, 70, Inf),
        right = FALSE,
        include.lowest = TRUE,
        labels = c("0-17", "18-29", "30-49", "50-69", "70+")
      ) %>%
      fct_explicit_na("Niet vermeld"),
    #
    # Combineer vacc + laatste infectie
    immunizatie_datum_laatste = case_when(
      # unused statement in current dataset.
      Vaccinatie_datum_laatste > Afspraak_start_datum                  ~ lag_afspraak_start_datum,
      # Wanneer de vaccinatiedatum na de afspraak ligt, het niet meenemen
      is.na(lag_afspraak_start_datum)                         ~ Vaccinatie_datum_laatste,
      is.na(Vaccinatie_datum_laatste)                                  ~ lag_afspraak_start_datum,
      Vaccinatie_datum_laatste >= lag_afspraak_start_datum    ~ Vaccinatie_datum_laatste,
      # nieuwere vaccinatie
      lag_afspraak_start_datum > Vaccinatie_datum_laatste     ~ lag_afspraak_start_datum,
      TRUE ~ NA_Date_
    ),
    
    immunizatie_interval = as.numeric(Afspraak_start_datum - immunizatie_datum_laatste),
    
    immunizatie_interval_groep = immunizatie_interval %>%
      cut(
        breaks = c(0, 60, 120, 180, Inf),
        right = FALSE,
        include.lowest = TRUE,
        labels = c("0-59", "60-119", "120-179", "180+")
      ),
  ) %>%
  filter(!is.na(`S result`) &
           Afspraak_start_datum %in% seq(start_date, end_date, 1)) %>%
  filter(!is.na(Pseudoniem)) %>%
  arrange(Afspraak_start_datum) %>%
  group_by(Pseudoniem) %>%
  mutate(interval = as.numeric(Afspraak_start_datum - lag(Afspraak_start_datum))) %>%
  ungroup %>%
  filter(interval > 29 | is.na(interval)) %>%
  select(-interval)
