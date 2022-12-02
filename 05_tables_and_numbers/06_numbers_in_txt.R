# Abstract
# N
data_teststraten_lab %>% filter(!is.na(`S result`)) %>% nrow

# Frac reinfections
data_teststraten_lab %>% filter(!is.na(`S result`)) %>% count(`S result`, Previous_infection) %>%
  group_by(`S result`) %>%
  mutate(`%` = n / sum(n) * 100)

# Method
# previous variant by WGS
data_teststraten_lab %>% filter(!is.na(`S result`) &
                                  Previous_infection == 1) %>%
  count(!is.na(previous_variant_WGS),
        !is.na(previous_SGTF),
        !is.na(previous_variant)) %>%
  mutate(N = sum(n))

# previous variant by SGTF

data_teststraten_lab_variant <- data_teststraten_lab %>%
  left_join(
    data_kiemsurv_org %>% select(`CoronIT Id`, Clade, `Pango Lineage`),
    by = c("Monsternummer" = "CoronIT Id")
  )

data_teststraten_lab_variant %>% filter(!is.na(Clade)) %>%
  filter(`S result` == "Not detected") %>% count(Clade)
# 22A is BA.4
# 22B is BA.5


data_teststraten_lab_variant %>% filter(!is.na(Clade)) %>%
  filter(`S result` == "Detected") %>% count(Clade)
# 22L + 22C


# previous infection
data_teststraten_lab %>% count(Previous_infection)

data_teststraten_lab %>% count(Previous_infection,!is.na(previous_variant),!is.na(previous_variant_WGS))


data_teststraten_lab %>% count(`S result`)

data_teststraten_lab %>%
  filter(!is.na(`S result`)) %>%
  group_by(Pseudoniem) %>%
  mutate(dif = Afspraak_start_datum - lag(Afspraak_start_datum)) %>%
  ungroup %>% count(dif) %>% view

# Result
data_teststraten_lab %>% count(`S result`) %>%
  mutate(proc = n / sum(n) * 100)

data_teststraten_lab %>% group_by(Afspraak_start_week) %>% count(`S result`) %>%
  mutate(proc = n / sum(n) * 100) %>% as.data.frame()

data_teststraten_lab %>% count(`S result`, Previous_infection)

# median between infections (days)
data_teststraten_lab %>% filter(!is.na(`S result`)) %>%
  group_by(`S result`) %>%
  summarise(median(interval_afspraken, na.rm = T))

data_teststraten_lab %>% filter(!is.na(`S result`) &
                                  !is.na(previous_variant)) %>%
  count(`S result`, previous_variant) %>%
  group_by(`S result`) %>%
  mutate(proc = n / sum(n) * 100)


# Figure captions:
# Fig 1A
data_teststraten_lab %>% nrow
# Fig 1B:
data_teststraten_lab %>%
  filter(
    `S result` %in% c("Detected", "Not detected") &
      !Vaccinatie_status %in% c("Onbekend", "Deels", "Pas", "Herhaalprik") &
      !is.na(Vaccinatie_status)
  ) %>%
  nrow
# Fig 1C
data_teststraten_lab %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Leeftijd >= 18 &
      !Immuunstatus %in% c("Unknown", "Booster2", "Previous infection,\nbooster2") &
      Geslacht != "Niet vermeld"
  ) %>%
  nrow

# Fig 2A
data_teststraten_lab %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Leeftijd >= 18 &
      Geslacht != "Niet vermeld"
  ) %>% nrow

data_teststraten_lab %>% mutate(
  prev_variant = case_when(
    Previous_infection == 0 ~ "naive",!is.na(previous_variant) ~ as.character(previous_variant)
  ) %>% factor(
    levels = c(
      "naive",
      "Pre-VOC",
      "Alpha",
      "Delta",
      "Omicron BA.1",
      "Omicron BA.2"
    )
  )
) %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Leeftijd >= 18 &
      `S result` %in% c("Detected", "Not detected") &
      !is.na(prev_variant) &
      Geslacht != "Niet vermeld"
  ) %>% nrow
# Fig2B
data_teststraten_lab %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Previous_infection == 1 &
      `S result` %in% c("Detected", "Not detected") &
      !is.na(previous_variant)
  ) %>% nrow
# Fig2C

data_teststraten_lab %>%
  filter.(Afspraak_start_datum >= as.Date("2022-05-02") &
            !is.na(interval_afspraken)) %>% nrow
