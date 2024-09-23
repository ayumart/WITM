

#open data and variables construction


#Library upload

library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(writexl)
library(eph)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(kableExtra)

library(readr)


rm(list = ls())

#witm <- read.csv2("Data/WITM_FINAL_sept.csv")
witm <- read.csv("Data/WITM_final_cleaned_08_sept.csv")

### FALTA RECUPERAR TODO EL CLEAN PREVIO
witm_id <- read.csv("Data/WITM_cleanup_id.csv")

witm<- left_join(witm,witm_id, by="X_id")

##levanto los datos para borrar
delete_witm <-read.csv("Data/WITM_delete_orgs.csv")
witm_clean <- anti_join(witm, delete_witm, by = "id")

##actualizacion de la q2

update_witm <-read.csv("Data/update_q2.csv")

names(witm_clean)

witm_clean <- witm_clean %>%
  left_join(update_witm, by = "id") %>%
  mutate(q1_description = ifelse(!is.na(q2_adjust), q2_adjust, q1_description)) %>%
  select(-q2_adjust)  # Optionally remove q2_cleaned column


write.csv(witm_clean, "Data/WITM_cleaned_09232024.csv", row.names = FALSE)


names(witm_clean)

