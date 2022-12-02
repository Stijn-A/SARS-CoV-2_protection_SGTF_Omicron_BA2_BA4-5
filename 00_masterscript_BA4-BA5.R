# Masterscript reduced protection against BA.5 compared to BA.2

# set WD
setwd("~/01_projects/01_sars-cov-2/sgtf_reduced_protection_ba.4-5_ba.2/")

## devtools::install_github("hrbrmstr/waffle")
lapply(c("tidyverse", "rlang", "tableone", "xlsx", "VGAM", "lubridate", "readxl", 
         "tidytable", "DBI", "odbc", "cowplot", "waffle"), 
       require, 
       character.only = TRUE)

start_date <- as_date("2022-05-02")
end_date <- as_date("2022-07-24")

source("scripts/01_import/01_import_BA4-BA5.R")

# determine cut-off points for previous variants
source("scripts/02_clean/04_previous_variant_cutoff.R")
# clean data
source("scripts/02_clean/02_clean_BA4-BA5.R")

# figures
source("scripts/03_figures/03_figures_epicurve.R")

source("scripts/03_figures/03_figures_reinfection.R")

source("scripts/03_figures/03_figuur_waffle_plot.R")

# logistic regression analysis
source("scripts/04_models/04_GLM_OR.R")

# table 1
source("scripts/05_tables_and_numbers/05_tables.R")

source("scripts/05_tables_and_numbers/06_numbers_in_txt.R")

# report output
source("scripts/06_report/07_save_output.R")