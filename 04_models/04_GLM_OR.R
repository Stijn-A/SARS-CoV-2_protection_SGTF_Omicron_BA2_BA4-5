model_IS <- data_teststraten_lab %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Leeftijd >= 18 &
      !Immuunstatus %in% c(
        "Unknown",
        "Other vaccination\n(>3 dosis)",
        "Previous infection,\nOther vaccination\n(>3 dosis)"
      ) &
      Geslacht != "Niet vermeld"
  ) %>%
  glm(
    `S result` ~
      Afspraak_start_datum +
      Immuunstatus +
      Leeftijdsgroep +
      Geslacht
    ,
    family = binomial(logit),
    data = .
  )

OR <- model_IS %>%
  coef() %>%
  exp() %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  rename("OR" = ".")

ci_OR <- model_IS %>%
  confint() %>%
  exp() %>%
  as.data.frame()

# table for figure
tabel_s_dropout_adj <-
  bind_cols(OR = OR, ci = ci_OR) %>% as_tibble() %>%
  filter(var %>% str_detect("Imm")) %>%
  mutate(
    label = str_c(
      format(round(OR, 1), nsmall = 1, trim = T),
      " (",
      format(round(`2.5 %`, 1), nsmall = 1, trim = T),
      "-",
      format(round(`97.5 %`, 1), nsmall = 1, trim = T),
      ")"
    ),
    var = var %>% str_remove("Immuunstatus")
  ) %>%
  full_join(
    data_teststraten_lab %>%
      filter(
        Afspraak_start_datum %in% seq(start_date, end_date, 1) &
          Leeftijd >= 18 &
          !Immuunstatus %in% c(
            "Unknown",
            "Other vaccination\n(>3 dosis)",
            "Previous infection,\nOther vaccination\n(>3 dosis)"
          ) &
          Geslacht != "Niet vermeld"
      ) %>%
      count(`S result`, Immuunstatus) %>%
      mutate(Variant = if_else(`S result` == "Detected", "BA.2", "BA.4/5")) %>%
      pivot_wider(
        id_cols = Immuunstatus,
        values_from = n,
        names_from = Variant
      ) %>%
      mutate(
        `%` = round(BA.2 / sum(BA.2) * 100, 1),
        `% ` = round(`BA.4/5` / sum(`BA.4/5`) * 100, 1)
      ) %>%
      select(Immuunstatus, BA.2, `%`, `BA.4/5`, `% `),
    by = c("var" = "Immuunstatus")
  ) %>%
  mutate(
    var = var %>%
      recode(
        `Naive` = "No previous infection,\nunvaccination",
        `Primary vaccination` = "No previous infection,\nprimary vaccination",
        `Booster` = "No previous infection,\nbooster"
      ) %>%
      factor(levels = rev(
        c(
          "No previous infection,\nunvaccination",
          "Previous infection,\nunvaccinated",
          "No previous infection,\nprimary vaccination",
          "Previous infection,\nprimary vaccination",
          "No previous infection,\nbooster",
          "Previous infection,\nbooster"
        )
      ))
  ) %>%
  arrange(desc(var))

figuur_GLM_OR <- tabel_s_dropout_adj %>%
  ggplot(data = ., aes(x = OR, y = var, label = label)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(
    aes(xmax = `97.5 %`, xmin = `2.5 %`),
    size = 0.7,
    height = 0.2,
    color = "gray50"
  ) +
  geom_point(size = 2.5, color = "#ffb612") +
  geom_label(aes(x = 4), label.size = NA, size = 3) +
  coord_cartesian(xlim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 20, 1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 10)) +
  ylab("") +
  xlab("Odds ratio")

model_herinfectie <- data_teststraten_lab %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Leeftijd >= 18 &
      Geslacht != "Niet vermeld"
  ) %>%
  rename(`Previous infection` =  Previous_infection) %>%
  glm(
    `S result` ~
      Afspraak_start_datum +
      `Previous infection` +
      Leeftijdsgroep +
      Geslacht
    ,
    family = binomial(logit),
    data = .
  )

OR <- model_herinfectie %>%
  coef() %>%
  exp() %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  rename("OR" = ".")

ci_OR <- model_herinfectie %>%
  confint() %>%
  exp() %>%
  as.data.frame()

tabel_s_dropout_herinfectie_adj <-
  bind_cols(OR = OR, ci = ci_OR) %>%
  as_tibble() %>%
  filter(var %>% str_detect("Previous infection")) %>%
  mutate(
    label = str_c(
      format(round(OR, 1), nsmall = 1, trim = T),
      " (",
      format(round(`2.5 %`, 1), nsmall = 1, trim = T),
      "-",
      format(round(`97.5 %`, 1), nsmall = 1, trim = T),
      ")"
    ),
    var = var %>% str_remove_all("`"),
    placeholder = " "
  ) %>%
  full_join(
    data_teststraten_lab %>%
      rename(`Previous infection` =  Previous_infection) %>%
      filter(
        Afspraak_start_datum %in% seq(start_date, end_date, 1) &
          Leeftijd >= 18 &
          Geslacht != "Niet vermeld"
      ) %>%
      count(`S result`, `Previous infection`) %>%
      mutate(Variant = if_else(`S result` == "Detected", "BA.2", "BA.4/5")) %>%
      pivot_wider(
        id_cols = `Previous infection`,
        values_from = n,
        names_from = Variant
      ) %>%
      mutate(
        `%` = round(BA.2 / sum(BA.2) * 100, 1),
        `% ` = round(`BA.4/5` / sum(`BA.4/5`) * 100, 1),
        `Previous infection` = if_else(
          `Previous infection` == 1,
          "Previous infection",
          "No previous infection"
        )
      ) %>%
      select(`Previous infection`, BA.2, `%`, `BA.4/5`, `% `),
    by = c("var" = "Previous infection")
  ) %>%
  mutate(var = var %>%
           factor(levels = c(
             "Previous infection", "No previous infection"
           ))) %>%
  arrange(desc(var)) %>%
  select(!placeholder)

# OR after a specific variant
model_variant <- data_teststraten_lab %>%
  mutate(
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
  ) %>%
  glm(
    `S result` ~
      Afspraak_start_datum +
      prev_variant +
      Leeftijdsgroep +
      Geslacht
    ,
    family = binomial(logit),
    data = .
  )

OR <- model_variant %>%
  coef() %>%
  exp() %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  rename("OR" = ".")

ci_OR <- model_variant %>%
  confint() %>%
  exp() %>%
  as.data.frame()

tabel_s_dropout_variant <-
  bind_cols(OR = OR, ci = ci_OR) %>% as_tibble() %>%
  filter(var %>% str_detect("prev_variant")) %>%
  mutate(
    label = str_c(
      format(round(OR, 1), nsmall = 1, trim = T),
      " (",
      format(round(`2.5 %`, 1), nsmall = 1, trim = T),
      "-",
      format(round(`97.5 %`, 1), nsmall = 1, trim = T),
      ")"
    ),
    var = var %>% str_remove("prev_variant"),
    placeholder = "  "
  ) %>%
  full_join(
    data_teststraten_lab %>%
      mutate(
        prev_variant = case_when(
          Previous_infection == 0 ~ "No previous infection",
          !is.na(previous_variant) ~ as.character(previous_variant)
        )
      ) %>%
      filter(
        Afspraak_start_datum %in% seq(start_date, end_date, 1) &
          Leeftijd >= 18 &
          `S result` %in% c("Detected", "Not detected") &
          !is.na(prev_variant) &
          Geslacht != "Niet vermeld"
      ) %>%
      count(`S result`, prev_variant) %>%
      mutate(Variant = if_else(`S result` == "Detected", "BA.2", "BA.4/5")) %>%
      pivot_wider(
        id_cols = prev_variant,
        values_from = n,
        names_from = Variant
      ) %>%
      mutate(
        `%` = round(BA.2 / sum(BA.2) * 100, 1),
        `% ` = round(`BA.4/5` / sum(`BA.4/5`) * 100, 1)
      ) %>%
      select(prev_variant, BA.2, `%`, `BA.4/5`, `% `),
    by = c("var" = "prev_variant")
  ) %>%
  mutate(var = var %>%
           factor(
             levels = c(
               "No previous infection",
               "Pre-VOC",
               "Alpha",
               "Delta",
               "Omicron BA.1",
               "Omicron BA.2"
             )
           )) %>%
  arrange(var)

figuur_GLM_OR_variant <- tabel_s_dropout_herinfectie_adj %>%
  bind_rows(tabel_s_dropout_variant)  %>%
  mutate(var = var %>% factor(
    levels = c(
      "Omicron BA.2",
      "Omicron BA.1",
      "Delta",
      "Alpha",
      "Pre-VOC",
      "Previous infection"
    )
  )) %>%
  ggplot(data = ., aes(x = OR, y = var, label = label)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(
    aes(xmax = `97.5 %`, xmin = `2.5 %`),
    size = 0.7,
    height = 0.2,
    color = "gray50"
  ) +
  geom_point(size = 1.5, color = "#ffb612") +
  geom_label(aes(x = 4), label.size = NA, size = 3) +
  coord_cartesian(xlim = c(0, 5)) +
  scale_x_continuous(breaks = seq(0, 20, 1)) +
  facet_grid (vars(placeholder), scales = "free_y", space = "free_y") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    text = element_text(size = 10)
  ) +
  ylab("") +
  xlab("Odds ratio")
