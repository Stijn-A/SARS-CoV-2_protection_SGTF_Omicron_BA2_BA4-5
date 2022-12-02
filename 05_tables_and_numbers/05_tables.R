tabel1 <- data_teststraten_lab %>%
  rename(
    `Age group` = Leeftijdsgroep,
    Sex = Geslacht,
    `Previous infection status` = reinfection,
    `Vaccination status` = vacc_status
  ) %>%
  mutate(
    `Previous infection and vaccination status` = Immuunstatus %>%
      recode(
        `Naive` = "No previous infection,\nunvaccinated",
        `Primary vaccination` = "No previous infection,\nprimary vaccination",
        `Booster` = "No previous infection,\nbooster",
        `Other vaccination\n(>3 dosis)` = "No previous infection,\nOther vaccination (>3 dosis)"
      ) %>%
      factor(levels = rev(
        c(
          "Unknown",
          "No previous infection,\nunvaccinated",
          "Previous infection,\nunvaccinated",
          "No previous infection,\nprimary vaccination",
          "Previous infection,\nprimary vaccination",
          "No previous infection,\nbooster",
          "Previous infection,\nbooster",
          "No previous infection,\nOther vaccination (>3 dosis)",
          "Previous infection,\nOther vaccination\n(>3 dosis)"
        )
      )),
    Afspraak_start_week = as.character(Afspraak_start_week)
  ) %>%
  CreateTableOne(
    vars = c(
      "Age group",
      "Sex",
      "Afspraak_start_week",
      "Previous infection status",
      "Vaccination status",
      "Previous infection and vaccination status"
    ),
    strata = c("S result")
  )

tabel1_csv <- print(
  tabel1,
  showAllLevels = TRUE,
  printToggle = FALSE,
  catDigits = 1
)
