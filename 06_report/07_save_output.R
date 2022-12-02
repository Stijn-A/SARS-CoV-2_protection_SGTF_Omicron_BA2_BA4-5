dir.create("output", showWarnings = F)

PATH_figures <- "output/"

ggsave(
  file = str_c(
    PATH_figures,
    'S1_SGTF_WGS_curve_',
    format(now(), format = "%Y%m%d_%H%M"),
    ".tiff"
  ),
  plot = fig_SGTF_WGS_day,
  width = 12,
  height = 6
)

ggsave(
  file = str_c(
    PATH_figures,
    'S2_SGTF_historie_',
    format(now(), format = "%Y%m%d_%H%M"),
    ".tiff"
  ),
  plot = fig_SGTF_WGS_day_AE,
  width = 8,
  height = 12
)

ggsave(
  file = str_c(
    PATH_figures,
    'S3_previous_variants_week_',
    format(now(), format = "%Y%m%d_%H%M"),
    ".tiff"
  ),
  plot = fig_waffle_week,
  width = 8,
  height = 10
)

# combine plots
fig_1 <-
  plot_grid(
    figuur_fractie_reinf,
    figuur_fractie_vacc,
    labels = c("A", "B"),
    align = "v",
    axis = "rlbt",
    ncol = 1
  )

ggsave(
  file = str_c(
    PATH_figures,
    'F1_',
    format(now(), format = "%Y%m%d_%H%M"),
    ".svg"
  ),
  plot = fig_1,
  width = 12,
  height = 8
)

ggsave(
  file = str_c(
    PATH_figures,
    'F2_OR_',
    format(now(), format = "%Y%m%d_%H%M"),
    ".svg"
  ),
  plot = figuur_interval_reinf,
  width = 10,
  height = 8
)

ggsave(
  file = str_c(
    PATH_figures,
    'S4_complete_case_analysis_',
    format(now(), format = "%Y%m%d_%H%M"),
    ".tiff"
  ),
  plot = fig_complete_case,
  width = 10,
  height = 8
)


write.xlsx(tabel1_csv,
           file = str_c(
             PATH_figures,
             "tabel1_BA5_characteristics_",
             format(now(), format = "%Y%m%d_%H%M"),
             ".xlsx"
           ))

write.xlsx(
  tabel_s_dropout_adj %>%
    select(!c(OR, `2.5 %`, `97.5 %`)),
  file = str_c(
    PATH_figures,
    "tabel2_OR_",
    format(now(), format = "%Y%m%d_%H%M"),
    ".xlsx"
  )
)

write.xlsx(
  tabel_s_dropout_herinfectie_adj %>%
    select(!c(OR, `2.5 %`, `97.5 %`)) %>%
    bind_rows(tabel_s_dropout_variant %>%
                select(!c(
                  OR, `2.5 %`, `97.5 %`, placeholder
                ))),
  file = str_c(
    PATH_figures,
    "tabel3_OR_",
    format(now(), format = "%Y%m%d_%H%M"),
    ".xlsx"
  )
)

# Table S1 complete case analysis
write.xlsx(
  tabel_s_dropout_herinfectie_adj_CC %>%
    select(!c(OR, `2.5 %`, `97.5 %`)) %>%
    bind_rows(tabel_s_dropout_variant_CC %>%
                select(!c(
                  OR, `2.5 %`, `97.5 %`, placeholder
                ))),
  file = str_c(
    PATH_figures,
    "tabelS1_OR_CC_",
    format(now(), format = "%Y%m%d_%H%M"),
    ".xlsx"
  )
)
