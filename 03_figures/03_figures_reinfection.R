text_size <- 10

tabel_reinfection_week <- data_teststraten_lab %>%
  filter(`S result` %in% c("Detected", "Not detected")) %>%
  count.(Afspraak_start_week, `S result`, reinfection, name = "N") %>%
  mutate(max_y = N %>% max()) %>%
  mutate.(
    `%` = N / sum(N) * 100,
    perc_scaled = max_y * `%` / max(`%`),
    .by = c(Afspraak_start_week, `S result`)
  ) %>%
  mutate(`S result` =
           recode(`S result`, `Not detected` = "SGTF", `Detected` = "non-SGTF")) %>%
  filter(Afspraak_start_week %in% seq(start_date, end_date, 7))

n_values_reinf <- tabel_reinfection_week %>%
  group_by(Afspraak_start_week, `S result`) %>%
  summarise(reinfection,
            N = N,
            n_total = sum(N)) %>%
  filter(reinfection == "Previous infection")

figuur_fractie_reinf <- ggplot(xlab = "S result") +
  geom_bar(
    data = tabel_reinfection_week,
    aes(x = `S result` , y = N, fill = reinfection),
    stat = "identity",
    position = "fill"
  )  +
  geom_text(
    data = n_values_reinf,
    aes(
      label = n_total,
      x = `S result`,
      y = n_total * 0.5 / n_total
    ),
    size = 2.8,
    angle = 90
  ) +
  facet_grid(cols = vars(Afspraak_start_week)) +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  labs(y = "Proportion") +
  theme_minimal() +
  theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.title = element_blank(),
    strip.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank()
  )

tabel_vacc_week <- data_teststraten_lab %>%
  filter(
    `S result` %in% c("Detected", "Not detected") &
      vacc_status %in% c(
        "Booster\nvaccination",
        "Primary\nvaccination",
        "Unvaccinated"
      )
  ) %>%
  count.(Afspraak_start_week, `S result`, vacc_status, name = "N") %>%
  mutate(max_y = N %>% max()) %>%
  mutate.(
    `%` = N / sum(N),
    perc_scaled = max_y * `%` / max(`%`),
    .by = c(Afspraak_start_week, `S result`)
  ) %>%
  mutate(`S result` =
           recode(`S result`, `Not detected` = "SGTF", `Detected` = "non-SGTF")) %>%
  filter(Afspraak_start_week %in% seq(start_date, end_date, 7))

figuur_fractie_vacc <- tabel_vacc_week %>%
  ggplot(data = .,
         xlab = "S result") +
  geom_bar(aes(x = `S result` , y = `%`, fill = vacc_status),
           stat = "identity",)  +
  geom_text(aes(label = N, y = `%` / 2, x = `S result`),
            size = 2.8,
            angle = 90) +
  facet_grid(vacc_status ~ Afspraak_start_week) +
  scale_fill_brewer(type = "seq",
                    palette = 5,
                    direction = -1) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  labs(y = "Proportion") +
  theme_minimal() +
  theme(
    text = element_text(size = text_size),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text.x = element_text(
      size = text_size - 2,
      angle = 90,
      vjust = 0.5
    ),
    legend.title = element_blank(),
    strip.text = element_blank()
  )


data_teststraten_lab %>%
  filter.(Afspraak_start_datum >= as.Date("2022-05-02") &
            !is.na(interval_afspraken)) %>%
  summarize.(
    mean_interval = mean(interval_afspraken,
                         na.rm = T),
    `interval 0.25` = quantile(interval_afspraken,
                               probs = 0.25,
                               na.rm = T),
    `interval 0.5` = quantile(interval_afspraken,
                              probs = 0.5,
                              na.rm = T),
    `interval 0.75` = quantile(interval_afspraken,
                               probs = 0.75,
                               na.rm = T),
    N = n(),
    .by = `S result`
  )

figuur_interval_reinf <- data_teststraten_lab %>%
  filter(
    Afspraak_start_datum %in% seq(start_date, end_date, 1) &
      Previous_infection == 1 &
      `S result` %in% c("Detected", "Not detected")
  ) %>%
  mutate(`S result` =
           recode(`S result`,
                  `Not detected` = "SGTF",
                  `Detected` = "non-SGTF")) %>%
  rename(`Previous variant` = previous_variant) %>%
  ggplot(data = ., aes(y = `S result`, x = interval_afspraken)) +
  geom_jitter(
    aes(fill = `Previous variant`),
    shape = 21,
    size = 2,
    color = "black",
    alpha = 0.5,
    height = 0.3
  ) +
  geom_boxplot(width = 0.2) +
  geom_violin(
    aes(color =  `S result`),
    fill = NA,
    trim = T,
    show.legend = F
  ) +
  stat_summary(
    fun = 'mean',
    geom = 'crossbar',
    col = 'red',
    alpha = 0.8,
    size = 0.3,
    width = 0.2,
    linetype = "dotted"
  ) +
  scale_x_continuous(breaks = seq(0, 700, 100),
                     limits = c(0, 700)) +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_color_brewer(type = "qual") +
  xlab("Interval testing dates (days)") +
  theme_minimal() +
  theme(text = element_text(size = text_size),
        axis.title.y = element_blank())

wilcox.test(
  x = data_teststraten_lab %>%
    filter.(
      Afspraak_start_datum %in% seq(start_date, end_date, 1) &
        !is.na(interval_afspraken) &
        `S result` == "Not detected"
    ) %>%
    pull(interval_afspraken),
  y = data_teststraten_lab %>%
    filter.(
      Afspraak_start_datum %in% seq(start_date, end_date, 1) &
        !is.na(interval_afspraken) &
        `S result` == "Detected"
    ) %>%
    pull(interval_afspraken),
  paired = FALSE
)
