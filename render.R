library(quarto)
library(tidyverse)

# load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/commentdata.Rdata")

npi_nums<-commentdata %>%
  distinct(npi_num) %>%
  pull(npi_num) %>%
  as.character()
  

names<-commentdata %>%
  distinct(provider_nm) %>%
  pull(provider_nm) %>%
  as.character()

reports<-
  tibble(
    input="recognition.qmd",
    output_file = str_glue("{names}.html"),
    execute_params=map(npi_nums,~list(npi=.))
  )



pwalk(reports,quarto_render)