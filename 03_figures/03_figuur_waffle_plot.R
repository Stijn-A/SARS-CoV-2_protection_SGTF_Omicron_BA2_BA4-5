text_size <- 10

table_waffle_total <- data_teststraten_lab %>%
  select(previous_variant,
         `S result`,
         Afspraak_start_datum,
         Previous_infection) %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Previous_infection == 1 &
      `S result` %in% c("Detected", "Not detected") &
      !is.na(previous_variant)
  ) %>%
  mutate(`S result` =
           recode(`S result`, `Not detected` = "SGTF", `Detected` = "non-SGTF")) %>%
  count(`S result`, previous_variant)

n_values = table_waffle_total %>%
  group_by(`S result`) %>%
  summarise(n_total = sum(n))

fig_waffle_total <- ggplot() +
  geom_waffle(
    data = table_waffle_total,
    aes(values = n, fill = previous_variant),
    size = 0.31,
    flip = T,
    make_proportional = T,
    height = 0.9,
    width = 0.9,
    color = "white"
  ) +
  geom_text(
    data = n_values,
    aes(
      label = paste0("n = ", n_total),
      x = 5.5,
      y = 12
    ),
    show.legend = FALSE,
    size = 2.5
  ) +
  coord_equal() +
  facet_wrap( ~ `S result`) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  labs(fill = "Previous variant") +
  theme() +
  theme(
    text = element_text(size = text_size),
    axis.text = element_blank(),
    axis.title = element_blank(),
    line = element_blank()
  )


data_teststraten_lab_test <- data_teststraten_lab %>%
  filter.(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
            !is.na(previous_variant))

table(data_teststraten_lab_test$`S result`,
      data_teststraten_lab_test$previous_variant)

chisq.test(
  data_teststraten_lab_test$`S result`,
  data_teststraten_lab_test$previous_variant == "Omicron BA.1" ,
  simulate.p.value = T
)

# Data from Table 3
data_teststraten_lab_test <- data_teststraten_lab %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Leeftijd >= 18 &
      Geslacht != "Niet vermeld"
  )

table(data_teststraten_lab_test$`S result`,
      data_teststraten_lab_test$previous_variant)

chisq.test(
  data_teststraten_lab_test$`S result`,
  data_teststraten_lab_test$previous_variant == "Omicron BA.1" ,
  simulate.p.value = T
)

data_teststraten_lab %>%
  filter.(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
            !is.na(previous_variant)) %>%
  
  count(`S result`, previous_variant) %>%
  group_by(`S result`) %>%
  summarise(
    totaal = sum(n),
    n = n,
    frac = n / totaal * 100,
    previous_variant  = previous_variant
  )

#https://stackoverflow.com/questions/70520851/geom-waffle-not-showing-one-cell
data_teststraten_lab_waffle <- data_teststraten_lab %>%
  select(previous_variant,
         `S result`,
         Afspraak_start_datum,
         Previous_infection) %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Previous_infection == 1 &
      `S result` %in% c("Detected", "Not detected") &
      !is.na(previous_variant)
  ) %>%
  mutate(
    Afspraak_start_3week = case_when(
      Afspraak_start_datum %in% seq(start_date, start_date + 21, 1) ~
        paste0(start_date, " – ", start_date + 21),
      Afspraak_start_datum %in% seq(start_date + 22, start_date + 43, 1) ~
        paste0(start_date + 22, " – ", start_date + 43),
      Afspraak_start_datum %in% seq(start_date + 44, start_date + 65, 1) ~
        paste0(start_date + 44, " – ", start_date + 65),
      Afspraak_start_datum %in% seq(start_date + 66, start_date + 87, 1) ~
        paste0(start_date + 66, " – ", end_date)
    ),
    `S result` =
      recode(`S result`, `Not detected` = "SGTF", `Detected` = "non-SGTF")
  )

table_waffle_week <- data_teststraten_lab_waffle %>%
  count(`S result`, Afspraak_start_3week, previous_variant) %>%
  group_by(`S result`, Afspraak_start_3week) %>%
  # Rounding using the largest remainder method as we always want to add up to 100
  # https://en.wikipedia.org/wiki/Largest_remainder_method
  mutate(
    frac = n / sum(n) * 100,
    trunc_frac = trunc(frac),
    remainder = frac - trunc_frac
  ) %>%
  summarise(
    n_sum = sum(trunc_frac),
    trunc_frac = trunc_frac,
    remainder = remainder,
    diff_to_100 = 100 - n_sum,
    previous_variant = previous_variant,
    n = sum(n, na.rm = T)
  ) %>%
  arrange(`S result`, Afspraak_start_3week, desc(remainder)) %>%
  mutate(row_number = row_number()) %>%
  mutate(fraction = if_else(row_number <= diff_to_100, trunc_frac + 1, trunc_frac)) %>%
  ungroup %>%
  mutate(previous_variant = previous_variant %>% factor(
    levels = c("Pre-VOC", "Alpha", "Delta", "Omicron BA.1", "Omicron BA.2")
  )) %>%
  complete(`S result`,
           Afspraak_start_3week,
           previous_variant,
           fill = list(fraction = 0))

n_values_week <- table_waffle_week %>%
  select(`S result`, Afspraak_start_3week, n) %>%
  filter(!is.na(n)) %>%
  distinct()

fig_waffle_week <- ggplot() +
  geom_waffle(
    data = table_waffle_week,
    aes(values = fraction, fill = previous_variant),
    size = 0.31,
    flip = T,
    make_proportional = F,
    height = 0.9,
    width = 0.9,
    color = "white"
  ) +
  geom_text(data = n_values_week,
            aes(
              label = paste0("n = ", n),
              x = 5.5,
              y = 12
            ),
            show.legend = FALSE) +
  coord_equal() +
  facet_wrap( ~ Afspraak_start_3week + `S result`, ncol = 2) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  labs(fill = "Previous variant") +
  theme() +
  theme(axis.text = element_blank(),
        line = element_blank(),
        axis.title = element_blank())
