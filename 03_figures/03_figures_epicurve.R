plot_1L <- ggplot() +
  geom_bar(
    data = data_teststraten_lab %>% filter(`S result` %in% c("Detected", "Not detected"))
    ,
    aes(x = Afspraak_start_datum,
        fill = `S result`),
    width = 1
  ) +
  scale_colour_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  scale_linetype_manual(values = c(3, 4), name = "Cohort") +
  scale_x_date(
    breaks = seq(start_date, end_date, 7),
    limits = c(start_date - 1, end_date + 1),
    expand = expansion(add = 0.5)
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
    legend.position = "none"
  )

kleurenpallet2 <- c(
  `Omicron BA.1` = '#AA4499',
  `Omicron BA.2` = '#999933',
  `Omicron BA.4` = '#44AA99',
  `Omicron BA.5` = '#882255',
  Other = '#888888'
)

# Figuur kiemsurv
plot_2L <-
  ggplot() +
  geom_bar(
    data = data_kiemsurv %>% filter(`Test date` %in% seq(start_date,
                                                         end_date, 1)),
    aes(x = `Test date`, color = `Variant (WGS)`, fill = `Variant (WGS)`),
    width = 0.72
  ) +
  scale_colour_manual(values = kleurenpallet2) +
  scale_fill_manual(values = kleurenpallet2) +
  scale_linetype_manual(values = c(3, 4)) +
  scale_x_date(
    breaks = seq(start_date,
                 end_date, 7),
    expand = expansion(add = 0.5),
    limits = c(start_date - 1, end_date + 1)
  ) +
  guides(linetype = "none") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x  = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
    legend.position = "none"
  )

plot_1R <- ggplot() +
  geom_bar(
    data = data_teststraten_lab %>%
      filter(`S result` %in% c("Detected", "Not detected")) %>%
      mutate(
        `S result` =
          recode(`S result`, `Not detected` = "S not\ndetected", `Detected` = "S detected")
      )
    ,
    aes(x = Afspraak_start_datum,
        fill = `S result`),
    width = 1,
    position = "fill"
  ) +
  scale_colour_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  scale_linetype_manual(values = c(3, 4), name = "Cohort") +
  scale_x_date(
    breaks = seq(start_date, end_date, 7),
    limits = c(start_date - 1, end_date + 1),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) +
  ylab("proportion") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
  )

# Figuur kiemsurv
plot_2R <-
  ggplot() +
  geom_bar(
    data = data_kiemsurv %>% filter(
      `Test date` %in% seq(as_date("2021-11-22"),
                           data_kiemsurv$`Test date` %>% max, 1)
    ),
    aes(x = `Test date`, color = `Variant (WGS)`, fill = `Variant (WGS)`),
    width = 0.72,
    position = "fill"
  ) +
  scale_colour_manual(values = kleurenpallet2) +
  scale_fill_manual(values = kleurenpallet2) +
  scale_linetype_manual(values = c(3, 4)) +
  scale_x_date(
    breaks = seq(start_date,
                 end_date, 7),
    expand = expansion(add = 0.5),
    limits = c(start_date - 1, end_date + 1)
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) +
  guides(linetype = "none") +
  ylab("proportion") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x  = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
  )

fig_SGTF_WGS_day <- plot_grid(
  plot_1L,
  plot_1R,
  plot_2L,
  plot_2R,
  align = "v",
  labels = c("A", "B", "C", "D"),
  ncol = 2,
  axis = "tblr",
  rel_heights = c(1, 1.2)
)


# Week
plot_1L_week <- ggplot() +
  geom_bar(data = data_teststraten_lab %>% filter(`S result` %in% c("Detected", "Not detected")),
           aes(x = Afspraak_start_week,
               fill = `S result`)) +
  scale_colour_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  scale_linetype_manual(values = c(3, 4), name = "Cohort") +
  scale_x_date(
    expand = expansion(add = -0.5),
    # Maatstreeplabel per week
    breaks = seq(start_date, end_date, 7),
    date_labels = "%e %b",
    limits = c(start_date - 3.5, end_date + 3.5),
    minor_breaks = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
    legend.position = "none"
  )

# Figuur kiemsurv
plot_2L_week <-
  ggplot() +
  geom_bar(
    data = data_kiemsurv %>% filter(
      `Test date` %in% seq(as_date("2021-11-22"),
                           data_kiemsurv$`Test date` %>% max, 1)
    ),
    aes(x = `Test week`, color = `Variant (WGS)`, fill = `Variant (WGS)`)
  ) +
  scale_fill_manual(values = kleurenpallet2) +
  scale_colour_manual(values = kleurenpallet2) +
  scale_linetype_manual(values = c(3, 4)) +
  scale_x_date(
    expand = expansion(add = -0.5),
    # Maatstreeplabel per week
    breaks = seq(start_date, end_date, 7),
    date_labels = "%e %b",
    limits = c(start_date - 3.5, end_date + 3.5),
    minor_breaks = NULL
  ) +
  guides(linetype = "none") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x  = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
    legend.position = "none"
  )

plot_1R_week <- ggplot() +
  geom_bar(
    data = data_teststraten_lab %>% filter(`S result` %in% c("Detected", "Not detected")),
    aes(x = Afspraak_start_week,
        fill = `S result`),
    position = "fill"
  ) +
  scale_colour_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  scale_linetype_manual(values = c(3, 4), name = "Cohort") +
  scale_x_date(
    expand = expansion(add = -0.5),
    # Maatstreeplabel per week
    breaks = seq(start_date, end_date, 7),
    date_labels = "%e %b",
    limits = c(start_date - 3.5, end_date + 3.5),
    minor_breaks = NULL
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) +
  ylab("proportion") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
  )

# Figuur kiemsurv
plot_2R_week <-
  ggplot() +
  geom_bar(
    data = data_kiemsurv %>% filter(
      `Test date` %in% seq(as_date("2021-11-22"),
                           data_kiemsurv$`Test date` %>% max, 1)
    ),
    aes(x = `Test week`, color = `Variant (WGS)`, fill = `Variant (WGS)`),
    position = "fill"
  ) +
  scale_fill_manual(values = kleurenpallet2) +
  scale_colour_manual(values = kleurenpallet2) +
  scale_linetype_manual(values = c(3, 4)) +
  scale_x_date(
    expand = expansion(add = -0.5),
    # Maatstreeplabel per week
    breaks = seq(start_date, end_date, 7),
    date_labels = "%e %b",
    limits = c(start_date - 3.5, end_date + 3.5),
    minor_breaks = NULL
  ) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) +
  guides(linetype = "none") +
  ylab("proportion") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x  = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10)
  )

figuur_SGTF_WGS_week <-
  plot_grid(
    plot_1L_week,
    plot_1R_week,
    plot_2L_week,
    plot_2R_week,
    align = "v",
    labels = c("A", "B", "C", "D"),
    ncol = 2,
    axis = "tblr",
    rel_heights = c(1, 1.2)
  )
