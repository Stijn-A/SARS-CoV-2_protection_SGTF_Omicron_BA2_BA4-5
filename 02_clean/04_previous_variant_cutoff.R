# Importeren

data_kiemsurv <- data_kiemsurv_org %>%
  filter(Monsterstroom %in% c("TESTSTRAAT")) %>%
  mutate(
    `Test date` = as.Date(`Datum-monstername`, format = "%d-%m-%Y"),
    `Test week` = floor_date(`Test date`, unit = "week", week_start = 1),
    `Variant (WGS)` = case_when(
      Clade %>% str_detect("Alpha") ~ "Alpha",
      Clade %>% str_detect("Beta") ~ "Beta",
      Clade %>% str_detect("Gamma") ~ "Gamma",
      Clade %>% str_detect("Delta") ~ "Delta",
      Clade == "21K (Omicron)" ~ "Omicron BA.1",
      Clade %in% c("21L (Omicron)", "22C (Omicron)") ~ "Omicron BA.2",
      Clade == "22A (Omicron)" ~ "Omicron BA.4",
      Clade == "22B (Omicron)" ~ "Omicron BA.5",
      Clade %in% c("19A", "19B", "20A", "20B", "20C", "20D", "20E (EU1)", "20G") ~ "Pre-VOC",
      TRUE ~ "Other"
    ) %>% factor(
      levels = c(
        "Pre-VOC",
        "Alpha",
        "Beta",
        "Gamma",
        "Delta",
        "Omicron BA.1",
        "Omicron BA.2",
        "Omicron BA.4",
        "Omicron BA.5",
        "Other"
      )
    )
  )

# source https://www.thermofisher.com/blog/clinical-conversations/a-reference-guide-to-notable-sars-cov-2-variants/
koppeltabel_variant_S_result <- as_tibble(list(
  `S Result` = c(
    "Detected",
    "Not detected",
    "Detected",
    "Detected",
    "Detected",
    "Not detected",
    "Detected",
    "Not detected",
    "Not detected"
  ),
  Variant = c(
    "Pre-VOC",
    "Alpha",
    "Beta",
    "Gamma",
    "Delta",
    "Omicron BA.1",
    "Omicron BA.2",
    "Omicron BA.4",
    "Omicron BA.5"
  ),
  min_date_90 = c(
    data_SGTF_org$Afspraak_start_datum %>% min(na.rm = T),
    data_SGTF_org$Afspraak_start_datum %>% min(na.rm = T),
    NA_Date_,
    NA_Date_,
    as_date("2021-06-20"),
    as_date("2021-11-23"),
    as_date("2022-01-29"),
    NA_Date_,
    NA_Date_
  ),
  max_date_90 = c(
    as_date("2021-02-17"),
    as_date("2021-09-27"),
    NA_Date_,
    NA_Date_,
    as_date("2022-01-07"),
    as_date("2022-04-09"),
    as_date("2022-07-18"),
    NA_Date_,
    NA_Date_
  )
))


# Tabel
Tabel_variant_S_result <- data_kiemsurv %>%
  filter(`Test date` >= data_SGTF_org$Afspraak_start_datum %>% min(na.rm = T)) %>%
  count(`Test date`, `Variant (WGS)`) %>%
  left_join(
    koppeltabel_variant_S_result %>% select(Variant, `S Result`),
    by = c("Variant (WGS)" = "Variant")
  ) %>%
  mutate.(
    `Variant found in genomic community surveillance (%)` = n / sum(n) * 100,
    .by = c(`Test date`, `S Result`)
  )
threshold <- 90

# Onder de 90%
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Pre-VOC")
#
# # 2021-02-17
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Alpha") %>% view
#
# # 2021-09-27
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Beta") %>% view
#
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Gamma") %>% view
#
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Delta") %>% view
# # 2021-06-20 -- 2022-01-07
#
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Omicron BA.1") %>% view
# # 2021-11-23 -- 2022-04-09
#
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Omicron BA.2") %>% view
# # 2022-01-29 -- 2022-07-18 (end date)
#
# Tabel_variant_S_result %>%
#   filter(
#     `Variant (WGS)` == "Omicron BA.5") %>% view

kleurenpallet <- c(
  `Pre-VOC` = '#88CCEE',
  Alpha = '#CC6677',
  Beta = '#DDCC77',
  Gamma = '#117733',
  Delta = '#332288',
  `Omicron BA.1` = '#AA4499',
  `Omicron BA.2` = '#999933',
  `Omicron BA.4` = '#44AA99',
  `Omicron BA.5` = '#882255'
)

plot_E <- ggplot() +
  geom_line(
    data = Tabel_variant_S_result %>% filter(
      !is.na(`S Result`) &
        `Test date` %in% seq(
          data_SGTF_org$Afspraak_start_datum %>% min(na.rm = T),
          end_date,
          1
        )
    ),
    aes(y = `Variant found in genomic community surveillance (%)`, x = `Test date`, color = `Variant (WGS)`)
  ) +
  geom_hline(yintercept = 90,
             color = "red",
             linetype =  2) +
  geom_vline(data = koppeltabel_variant_S_result,
             aes(xintercept = min_date_90, color = Variant),
             linetype = 3) +
  geom_vline(data = koppeltabel_variant_S_result,
             aes(xintercept = max_date_90, color = Variant),
             linetype = 3) +
  scale_colour_manual(values = kleurenpallet) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_date(breaks = "months",
               expand = expansion(add = 0.5)) +
  facet_wrap( ~ `S Result`, ncol = 1) +
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

kleurenpallet2 <- c(
  `Pre-VOC` = '#88CCEE',
  Alpha = '#CC6677',
  Beta = '#DDCC77',
  Gamma = '#117733',
  Delta = '#332288',
  `Omicron BA.1` = '#AA4499',
  `Omicron BA.2` = '#999933',
  `Omicron BA.4` = '#44AA99',
  `Omicron BA.5` = '#882255',
  Other = '#888888'
)

# In order to match SGTF result
variants <- c(
  "Other",
  "Pre-VOC",
  "Beta",
  "Gamma",
  "Delta",
  "Omicron BA.2",
  "Alpha",
  "Omicron BA.1",
  "Omicron BA.4",
  "Omicron BA.5"
)

# Figuur kiemsurv
plot_A <-
  ggplot() +
  geom_bar(
    data = data_kiemsurv %>% filter(
      `Test date` %in% seq(
        data_SGTF_org$Afspraak_start_datum %>% min(na.rm = T),
        end_date,
        1
      )
    ) %>%
      mutate(`Variant (WGS)` = `Variant (WGS)` %>% factor(levels = variants)),
    aes(x = `Test date`, color = `Variant (WGS)`, fill = `Variant (WGS)`),
    width = 0.72
  ) +
  scale_colour_manual(values = kleurenpallet2) +
  scale_fill_manual(values = kleurenpallet2) +
  scale_linetype_manual(values = c(3, 4)) +
  scale_x_date(breaks = "months",
               expand = expansion(add = 0.5)) +
  guides(linetype = "none") +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    #legend.position="top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10),
    legend.position = "none"
  )

# Figuur kiemsurv
plot_B <-
  ggplot() +
  geom_bar(
    data = data_kiemsurv %>% filter(
      `Test date` %in% seq(
        data_SGTF_org$Afspraak_start_datum %>% min(na.rm = T),
        end_date,
        1
      )
    ) %>%
      mutate(`Variant (WGS)` = `Variant (WGS)` %>% factor(levels = variants)),
    aes(x = `Test date`, color = `Variant (WGS)`, fill = `Variant (WGS)`),
    width = 0.72,
    position = "fill"
  ) +
  scale_colour_manual(values = kleurenpallet2) +
  scale_fill_manual(values = kleurenpallet2) +
  scale_linetype_manual(values = c(3, 4)) +
  scale_x_date(breaks = "months",
               expand = expansion(add = 0.5)) +
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
    #legend.position="top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10)
  )

# large epicurves A-D
plot_C <- ggplot() +
  geom_bar(
    data = data_SGTF_org %>% filter(`S result` %in% c("Detected", "Not detected"))
    ,
    aes(x = Afspraak_start_datum,
        fill = `S result`),
    width = 1
  ) +
  scale_colour_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  scale_linetype_manual(values = c(3, 4), name = "Cohort") +
  scale_x_date(breaks = "months",
               expand = expansion(add = 0.5)) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -1, -10, -10)
  )

plot_D <- ggplot() +
  geom_bar(
    data = data_SGTF_org %>% filter(`S result` %in% c("Detected", "Not detected"))
    ,
    aes(x = Afspraak_start_datum,
        fill = `S result`),
    width = 1,
    position = "fill"
  ) +
  scale_colour_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  scale_linetype_manual(values = c(3, 4), name = "Cohort") +
  scale_x_date(breaks = "months",
               expand = expansion(add = 0.5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) +
  ylab("proportion") +
  xlab("Test date") +
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

fig_SGTF_WGS_day_AE <-
  plot_grid(
    plot_A,
    plot_B,
    plot_C,
    plot_D,
    plot_E,
    align = "v",
    labels = c("A", "B", "C", "D", "E"),
    ncol = 1,
    axis = "tblr",
    rel_heights = c(1, 1, 1, 1, 2)
  )