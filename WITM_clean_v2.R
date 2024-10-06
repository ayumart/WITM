

#open data and variables construction


#Library upload

library(tidyverse)
library(dplyr)


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

update_witm <-read.csv("Data/update_q1.csv")

qry<-witm_clean %>%
  group_by(q1_description) %>% 
  summarise(Count=n())
qry

witm_clean <- witm_clean %>%
  left_join(update_witm, by = "id") %>%
  mutate(q1_description = ifelse((!is.na(q2_adjust)&q2_adjust!=""), trimws(q2_adjust), q1_description)) %>%
  select(-q2_adjust)  # Optionally remove q2_cleaned column


qry<-witm_clean %>%
  group_by(q1_description) %>% 
  summarise(Count=n())
qry

##I delete also the 98 organisations
witm_clean<-witm_clean %>% filter(q1_description!="98")

qry<-witm_clean %>%
  group_by(q1_description) %>% 
  summarise(Count=n())
qry

write.csv(witm_clean, "Data/WITM_cleaned_09232024.csv", row.names = FALSE)

delete_witm<-delete_witm %>% filter(Action=="FUND") %>% select(id,Action)
funds<- left_join(delete_witm,witm, by="id")

names(funds)
qry<-funds %>%
  group_by(q1_description) %>% 
  summarise(Count=n())
qry

write.csv(funds, "Data/WITM_funds.csv", row.names = FALSE)


