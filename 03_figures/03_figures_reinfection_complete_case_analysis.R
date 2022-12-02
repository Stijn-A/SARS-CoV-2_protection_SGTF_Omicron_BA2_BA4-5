# Complete case analysis Fig 1A

text_size <- 10

tabel_reinfection_week_CC <- data_teststraten_lab %>% 
  filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
           Leeftijd >= 18 & 
           !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
           Geslacht != "Niet vermeld") %>% 
  filter(`S result` %in% c("Detected", "Not detected")) %>% 
  count.(Afspraak_start_week, `S result`, reinfection, name = "N") %>% 
  mutate(max_y = N %>% max()) %>% 
  mutate.(
    `%` = N / sum(N) * 100,
    perc_scaled = max_y * `%` / max( `%`),
    .by = c(Afspraak_start_week,`S result`)
  ) %>% 
  mutate(`S result` = 
           recode(`S result`, `Not detected` = "SGTF", `Detected` = "non-SGTF")) %>% 
  filter(Afspraak_start_week %in% seq(start_date, end_date, 7))

n_values_reinf_CC <- tabel_reinfection_week_CC %>% 
  group_by(Afspraak_start_week, `S result`) %>% 
  summarise(
    reinfection,
    N = N,
    n_total = sum(N)
  ) %>% 
  filter(reinfection == "Previous infection")

figuur_fractie_reinf_CC <- ggplot(xlab = "S result") +
  geom_bar(data = tabel_reinfection_week_CC,
           aes(x = `S result` , y = N, fill = reinfection),
           stat = "identity",
           #width = .5,
           position = "fill")  +
  geom_text(data = n_values_reinf_CC, aes(label=paste0("n = ",n_total), x = `S result`, y = n_total * 0.5 / n_total), size = 2.8, angle = 90) +
  facet_grid(cols = vars(Afspraak_start_week)) +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,1,0.1)) +
  labs(y = "Proportion") +
  theme_minimal() +
  theme(text = element_text(size = text_size),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank())



figuur_interval_reinf_CC <- data_teststraten_lab %>%
  filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
           Leeftijd >= 18 & 
           !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
           Geslacht != "Niet vermeld" &
           Previous_infection == 1) %>% 
  mutate(`S result` = 
           recode(`S result`, 
                  `Not detected` = "SGTF", 
                  `Detected` = "non-SGTF")) %>% 
  rename(`Previous variant` = previous_variant) %>% 
  ggplot(data = ., aes(y = `S result`, x = interval_afspraken)) +
  geom_jitter(aes(fill = `Previous variant`), shape = 21, size=2,  color="black", alpha=0.5, height = 0.3) +
  geom_boxplot(width = 0.2) +
  geom_violin(aes(color =  `S result`), 
              fill = NA,
              trim = T,
              show.legend = F
  ) +
  stat_summary(fun='mean', geom='crossbar', col='red', alpha = 0.8,
               size = 0.3, width = 0.2, linetype = "dotted") +
  scale_x_continuous(breaks = seq(0,700,100),
                     limits = c(0,700)) +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_color_brewer(type = "qual") +
  xlab("Interval testing dates (days)") +
  theme_minimal() +
  theme(text = element_text(size = text_size),
        axis.title.y = element_blank())

wilcox.test(
  x = data_teststraten_lab %>% 
    filter.(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
              !is.na(interval_afspraken) &
              `S result` == "Not detected") %>% 
    filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
             Leeftijd >= 18 & 
             !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
             Geslacht != "Niet vermeld") %>% 
    pull(interval_afspraken),
  y = data_teststraten_lab %>% 
    filter.(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
              !is.na(interval_afspraken) &
              `S result` == "Detected") %>% 
    filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
             Leeftijd >= 18 & 
             !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
             Geslacht != "Niet vermeld") %>% 
    pull(interval_afspraken),
  paired = FALSE
)



model_herinfectie_CC <- data_teststraten_lab %>% 
  filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
           Leeftijd >= 18 & 
           !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
           Geslacht != "Niet vermeld") %>% 
  rename(`Previous infection` =  Previous_infection) %>% 
  glm(`S result` ~ 
        Afspraak_start_datum +
        `Previous infection` + 
        Leeftijdsgroep + 
        Geslacht
      , 
      family = binomial(logit), 
      data = .)  
OR    <- model_herinfectie_CC %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR <- model_herinfectie_CC %>% confint() %>% exp() %>% as.data.frame()

tabel_s_dropout_herinfectie_adj_CC <- bind_cols(OR = OR, ci = ci_OR) %>% 
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
      filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
               Leeftijd >= 18 & 
               !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
               Geslacht != "Niet vermeld") %>% 
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
model_variant_CC <- data_teststraten_lab %>% 
  mutate(
    prev_variant = case_when(
      Previous_infection == 0 ~ "naive",
      !is.na(previous_variant) ~ as.character(previous_variant)
    ) %>% factor(levels = c("naive", "Pre-VOC", "Alpha", "Delta", "Omicron BA.1", "Omicron BA.2"))
  ) %>% 
  filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
           Leeftijd >= 18 &
           !is.na(prev_variant) &
           Geslacht != "Niet vermeld" & 
           !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)")) %>% 
  glm(`S result` ~ 
        Afspraak_start_datum +
        prev_variant + 
        Leeftijdsgroep + 
        Geslacht
      , 
      family = binomial(logit), 
      data = .)  

OR    <- model_variant_CC %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR <- model_variant_CC %>% confint() %>% exp() %>% as.data.frame()


tabel_s_dropout_variant_CC <-
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
          Previous_infection == 0 ~ "No previous infection",!is.na(previous_variant) ~ as.character(previous_variant)
        )
      ) %>%
      filter(
        Afspraak_start_datum %in% seq(start_date, end_date, 1) &
          Leeftijd >= 18 &
          `S result` %in% c("Detected", "Not detected") &
          !is.na(prev_variant) &
          Geslacht != "Niet vermeld" &
          !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)")
      ) %>%
      count(`S result`, prev_variant) %>%
      mutate(Variant = if_else(`S result` == "Detected", "BA.2", "BA.4/5")) %>%
      pivot_wider(
        id_cols = prev_variant,
        values_from = n,
        names_from = Variant
      ) %>%
      mutate(
        `%` = round(BA.2 / sum(BA.2,  na.rm = T) * 100, 1),
        `% ` = round(`BA.4/5` / sum(`BA.4/5`,  na.rm = T) * 100, 1),
        #`Previous infection` = if_else(
        #  `Previous infection` == 1,
        #  "Previous infection",
        #  "No previous infection")
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


# figuur_GLM_OR_variant_CC <- tabel_s_dropout_herinfectie_adj_CC %>% 
#   bind_rows(tabel_s_dropout_variant_CC)  %>% 
#   mutate(var = var %>% factor(levels = c("Omicron BA.2","Omicron BA.1","Delta","Alpha","Pre-VOC","Previous infection"))) %>% 
#   ggplot(data = ., aes(x = OR, y = var, label = label)) + 
#   geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
#   geom_errorbarh(aes(xmax = `97.5 %`, xmin = `2.5 %`), 
#                  size = 0.7, 
#                  height = 0.2, 
#                  color = "gray50") +
#   geom_point(size = 1.5, color = "#ffb612") +
#   geom_label(aes(x = 4),label.size = NA, size = 3) +
#   coord_cartesian(xlim = c(0,5)) +
#   scale_x_continuous(breaks = seq(0,20,1)) +
#   facet_grid (vars(placeholder), scales = "free_y", space = "free_y") +
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         text = element_text(size = 10)) +
#   ylab("") +
#   xlab("Odds ratio") 
# 
# table_waffle_total_CC <- data_teststraten_lab %>%
#   filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
#            Leeftijd >= 18 & 
#            !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
#            Geslacht != "Niet vermeld") %>% 
#   select(previous_variant, `S result`, Afspraak_start_datum, Previous_infection) %>% 
#   filter(
#     Afspraak_start_datum %in% seq(start_date, end_date, 1) &
#       Previous_infection == 1 &
#       `S result` %in% c("Detected", "Not detected") & 
#       !is.na(previous_variant)
#   ) %>%
#   #mutate(previous_variant = as.character(previous_variant)) %>% 
#   mutate(`S result` = 
#            recode(`S result`, `Not detected` = "SGTF", `Detected` = "non-SGTF")) %>% 
#   count(`S result`, previous_variant) 
# 
# n_values_CC = table_waffle_total_CC %>% 
#   group_by(`S result`) %>% 
#   summarise(
#     n_total = sum(n)
#   )
# 
# fig_waffle_total_CC <- ggplot() +
#   geom_waffle(
#     data = table_waffle_total_CC,
#     aes(values = n, fill = previous_variant),
#     size = 0.31,
#     flip = T,
#     make_proportional = T,
#     height = 0.9,
#     width = 0.9,
#     color = "white"
#   ) +
#   geom_text(
#     data = n_values_CC,
#     aes(label = paste0("n = ", n_total), x = 5.5, y = 12),
#     show.legend = FALSE, size = 2.5
#   ) +
#   coord_equal() +
#   facet_wrap(~`S result`) +
#   scale_fill_brewer(type = "qual", palette = 2) +
#   theme_minimal() +
#   labs(fill = "Previous variant") +
#   theme() +
#   theme(
#     text = element_text(size = text_size),
#     axis.text = element_blank(), 
#     axis.title = element_blank(), 
#     line = element_blank())


data_teststraten_lab_test_CC <- data_teststraten_lab %>% 
  filter.(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
            !is.na(previous_variant)) %>% 
  filter(Afspraak_start_datum %in% seq(start_date, end_date, 1) &
           Leeftijd >= 18 & 
           !Immuunstatus %in% c("Unknown", "Other vaccination\n(>3 dosis)", "Previous infection,\nOther vaccination\n(>3 dosis)") &
           Geslacht != "Niet vermeld")

chisq.test(
  data_teststraten_lab_test_CC$`S result`, 
  data_teststraten_lab_test_CC$previous_variant == "Omicron BA.1" ,
  simulate.p.value = F
)

data_teststraten_lab_test_CC %>%
  
  count(`S result`, previous_variant) %>% 
  group_by(`S result`) %>% 
  summarise(
    totaal = sum(n),
    n = n,
    frac = n / totaal * 100,
    previous_variant  = previous_variant 
  )


fig_complete_case <-
  plot_grid(figuur_fractie_reinf_CC,
            #figuur_GLM_OR_variant_CC,
            #fig_waffle_total_CC,
            figuur_interval_reinf_CC,
            labels = c("A", "B"),
            nrow = 2
            #rel_widths = c(0.7,1)
  )
