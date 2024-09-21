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


# OPEN CSV file

witm <- read.csv("Data/WITM_final_cleaned_08_sept.csv")

source(file="WITM_variables_v2.R") 




##############################################################################

# ANÁLISIS Q10

archivo <- "cuadros/q10_budget.xlsx"
write.xlsx(q10_grouped, file = archivo, sheetName="q10")

q10_2021<-base %>% 
  filter(q9_year_formation<2022) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(n=n()) %>% 
  rename("Annual_budget"=q10_budget_grp_2021) %>% 
  mutate(Year=2021)

q10_2022<-base %>% 
  filter(q9_year_formation<2023) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(n=n()) %>% 
  rename("Annual_budget"=q10_budget_grp_2022) %>% 
  mutate(Year=2022)

q10_2023<-base %>% 
  filter(!is.na(q10_budget_year_2023)) %>% 
  group_by(q10_budget_grp_2023) %>% 
  summarise(n=n()) %>% 
  rename("Annual_budget"=q10_budget_grp_2023) %>% 
  mutate(Year=2023)

# Unir los dataframes
q10_grouped <- bind_rows(q10_2021, q10_2022, q10_2023)

#q10_total
q10_grouped<-q10_grouped %>% arrange(Annual_budget)
# Crear un nuevo data frame con los resultados
q10_grouped <- q10_grouped %>%
  pivot_wider(names_from = Year, values_from = c(n)) %>% 
  mutate(Media = round(rowMeans(cbind(`2021`, `2022`, `2023`), na.rm = TRUE), 0))


write.xlsx(q10_grouped, file = archivo, sheetName="q10")

###

#q4agrup

q10_q4agrup<-base %>% 
  


q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q4_agrup")
writeData(q10, sheet = "q4_agrup", x = q4_agrup)
saveWorkbook(q10, archivo, overwrite = TRUE)


###

#cruce por q5 registrados


# Crear q10_2021_q5
q10_2021_q5 <- base %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q5
q10_2022_q5 <- base %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q5
q10_2023_q5 <- base %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q10_budget_year_2023)) %>% 
  group_by(q10_budget_grp_2023, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)

# Unir los dataframes
q10_grouped_q5 <- bind_rows(q10_2021_q5, q10_2022_q5, q10_2023_q5)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q5 %>%
  group_by(Year, Annual_budget, q5) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q5, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q5 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q5, Media) %>%
  pivot_wider(names_from = q5, values_from = Media, values_fill = list(Media = 0))

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q5")
writeData(q10, sheet = "q10_q5", x = q10_table)
saveWorkbook(q10, archivo, overwrite = TRUE)

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q5_media")
writeData(q10, sheet = "q10_q5_media", x = q10_media_table)
saveWorkbook(q10, archivo, overwrite = TRUE)

###



# cruce por q6 scope

# Crear q10_2021_q6
q10_2021_q6 <- base %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q6
q10_2022_q6 <- base %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q6
q10_2023_q6 <- base %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q10_budget_year_2023)) %>%
  group_by(q10_budget_grp_2023, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
    mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q6 <- bind_rows(q10_2021_q6, q10_2022_q6, q10_2023_q6)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q6 %>%
  group_by(Year, Annual_budget, q6) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q6, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q6 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q6, Media) %>%
  pivot_wider(names_from = q6, values_from = Media, values_fill = list(Media = 0))

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q6")
writeData(q10, sheet = "q10_q6", x = q10_table)
saveWorkbook(q10, archivo, overwrite = TRUE)

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q6_media")
writeData(q10, sheet = "q10_q6_media", x = q10_media_table)
saveWorkbook(q10, archivo, overwrite = TRUE)

#######

## cruce por q9 year of formation

# Crear q10_2021_q9
q10_2021_q9 <- base %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q6
q10_2022_q9 <- base %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q6
q10_2023_q9 <- base %>% 
  filter(!is.na(q10_budget_year_2023)) %>%
  group_by(q10_budget_grp_2023, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q9 <- bind_rows(q10_2021_q9, q10_2022_q9, q10_2023_q9)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q9 %>%
  group_by(Year, Annual_budget, q9_year_formation_agrup) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q9_year_formation_agrup, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q9 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q9_year_formation_agrup, Media) %>%
  pivot_wider(names_from = q9_year_formation_agrup, values_from = Media, values_fill = list(Media = 0))

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q9")
writeData(q10, sheet = "q10_q9", x = q10_table)
saveWorkbook(q10, archivo, overwrite = TRUE)

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q9_media")
writeData(q10, sheet = "q10_q9_media", x = q10_media_table)
saveWorkbook(q10, archivo, overwrite = TRUE)


##############################################################################

# ANÁLISIS DE LA q13

archivo <- "cuadros/q13_external_funding.xlsx"



q13<-base %>%
  filter(!is.na(q13_ext_funding)) %>% 
  group_by(q13_ext_funding) %>% 
  summarise(n=n()) 

write.xlsx(q13, file = archivo, sheetName="q13")


## CRUCE POR Q5



# Crear q13_q5
q13_q5 <- base %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q13_ext_funding)) %>% 
  group_by(q13_ext_funding, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("External funding" = q13_ext_funding)

q13_q5 <- q13_q5 %>%
  pivot_wider(names_from = q5, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`External funding`)


q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_q5")
writeData(q13, sheet = "q13_q5", x = q13_q5)
saveWorkbook(q13, archivo, overwrite = TRUE)



#######

#CRUCE POR Q6

q13_q6 <- base %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q13_ext_funding)) %>% 
  group_by(q13_ext_funding, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("External funding" = q13_ext_funding)

q13_q6 <- q13_q6 %>%
  pivot_wider(names_from = q6, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`External funding`)

q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_q6")
writeData(q13, sheet = "q13_q6", x = q13_q6)
saveWorkbook(q13, archivo, overwrite = TRUE)

#####

#CRUCE POR q9

q13_q9 <- base %>% 
  filter(!is.na(q13_ext_funding)) %>% 
  group_by(q13_ext_funding, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("External funding" = q13_ext_funding)

q13_q9 <- q13_q9 %>%
  pivot_wider(names_from = q9_year_formation_agrup, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`External funding`)

q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_q9")
writeData(q13, sheet = "q13_q9", x = q13_q9)
saveWorkbook(q13, archivo, overwrite = TRUE)


###

#CRUCE POR q10

# Crear q10_2021_q13
q10_2021_q13 <- base %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q13_ext_funding) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q13
q10_2022_q13 <- base %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q13_ext_funding) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q13
q10_2023_q13 <- base %>% 
  filter(!is.na(q10_budget_year_2023)) %>%
  group_by(q10_budget_grp_2023, q13_ext_funding) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q13 <- bind_rows(q10_2021_q13, q10_2022_q13, q10_2023_q13)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q13 %>%
  group_by(Year, Annual_budget, q13_ext_funding) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q13_ext_funding, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q13 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q13_ext_funding, Media) %>%
  pivot_wider(names_from = q13_ext_funding, values_from = Media, values_fill = list(Media = 0))

q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_q10")
writeData(q13, sheet = "q13_q10", x = q10_table)
saveWorkbook(q13, archivo, overwrite = TRUE)

q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_q10_media")
writeData(q13, sheet = "q13_q10_media", x = q10_media_table)
saveWorkbook(q13, archivo, overwrite = TRUE)
##############################################################################


#ANÁLISIS DE LA q14

archivo <- "cuadros/q14_budget.xlsx"


q14_2023 <- base %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2023) %>% 
  summarise("2023" = n())%>% 
  rename("q14"=1)



q14_2022 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2022) %>% 
  summarise("2022" = n()) %>% 
  rename("q14"=1)

q14_2021 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2021) %>% 
  summarise("2021" = n())%>% 
  rename("q14"=1)

#Unir los dataframes
q14_combined <- q14_2021 %>%
  full_join(q14_2022, by = "q14") %>%
  full_join(q14_2023, by = "q14")

q14_grouped<-q14_combined %>% 
  rename("Percentage of External Funding in Total Annual Budget"=q14)

write.xlsx(q14_grouped, file = archivo, sheetName="q14")


###

#CRUCE POR q5

q14_2023_q5 <- base %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>%
  group_by(q14_funding_annual_budget_2023, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2023)

q14_2022_q5 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>%
  group_by(q14_funding_annual_budget_2022, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2022)


q14_2021_q5 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>%
  group_by(q14_funding_annual_budget_2021, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2021)


# Unir los dataframes
q14_grouped_q5 <- bind_rows(q14_2021_q5, q14_2022_q5, q14_2023_q5)



# Crear la tabla de doble entrada
q14_table <- q14_grouped_q5 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q14)  # Opcional: ordenar por la columna de "q14"

# Crear la tabla de doble entrada con q14 y q5
q14_table <- q14_grouped_q5 %>%
  group_by(Year, q14, q5) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q14, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q5)  # Opcional: ordenar por el año y q5

q14 <- loadWorkbook(archivo)
addWorksheet(q14, sheetName = "q14_q5")
writeData(q14, sheet = "q14_q5", x = q14_table)
saveWorkbook(q14, archivo, overwrite = TRUE)

###

#CRUCE POR q6


q14_2023_q6 <- base %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  group_by(q14_funding_annual_budget_2023, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2023)

q14_2022_q6 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  group_by(q14_funding_annual_budget_2022, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2022)


q14_2021_q6 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  group_by(q14_funding_annual_budget_2021, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2021)


# Unir los dataframes
q14_grouped_q6 <- bind_rows(q14_2021_q6, q14_2022_q6, q14_2023_q6)


# Crear la tabla de doble entrada con q14 y q5
q14_table <- q14_grouped_q6 %>%
  group_by(Year, q14, q6) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q14, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q6)  # Opcional: ordenar por el año y q5

q14 <- loadWorkbook(archivo)
addWorksheet(q14, sheetName = "q14_q6")
writeData(q14, sheet = "q14_q6", x = q14_table)
saveWorkbook(q14, archivo, overwrite = TRUE)

######

#CRUCE POR q9

q14_2023_q9 <- base %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2023, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2023)

q14_2022_q9 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2022, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2022)


q14_2021_q9 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2021, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2021)


# Unir los dataframes
q14_grouped_q9 <- bind_rows(q14_2021_q9, q14_2022_q9, q14_2023_q9)


# Crear la tabla de doble entrada con q14 y q5
q14_table <- q14_grouped_q9 %>%
  group_by(Year, q14, q9_year_formation_agrup) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q14, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q9_year_formation_agrup)  # Opcional: ordenar por el año y q5

q14 <- loadWorkbook(archivo)
addWorksheet(q14, sheetName = "q14_q9")
writeData(q14, sheet = "q14_q9", x = q14_table)
saveWorkbook(q14, archivo, overwrite = TRUE)

### 
# CRUCE POR q10



q14_2021_q10 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2021, q10_budget_grp_2021) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1)

  # Crear la tabla de doble entrada
q14_table_2021 <- q14_2021_q10 %>%
    pivot_wider(names_from = q14, values_from = n, values_fill = list(n = 0)) %>%
    arrange(q10_budget_grp_2021)  # Opcional: ordenar por grupo de presupuesto  
  

q14_2022_q10 <- base %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2022, q10_budget_grp_2022) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) 

# Crear la tabla de doble entrada
q14_table_2022 <- q14_2022_q10 %>%
  pivot_wider(names_from = q14, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q10_budget_grp_2022)  # Opcional: ordenar por grupo de presupuesto  
  


q14_2023_q10 <- base %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2023, q10_budget_grp_2023) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) 

# Crear la tabla de doble entrada
q14_table_2023 <- q14_2023_q10 %>%
  pivot_wider(names_from = q14, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q10_budget_grp_2023)  # Opcional: ordenar por grupo de presupuesto  



q14 <- loadWorkbook(archivo)
addWorksheet(q14, sheetName = "q14_q10")
# Escribir las tablas en la misma hoja
writeData(q14, "q14_q10", "Table for 2021", startRow = 1, startCol = 1)
writeData(q14, "q14_q10", q14_table_2021, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q14, "q14_q10", "Table for 2022", startRow = nrow(q14_table_2021) + 4, startCol = 1)
writeData(q14, "q14_q10", q14_table_2022, startRow = nrow(q14_table_2021) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q14, "q14_q10", "Table for 2023", startRow = nrow(q14_table_2021) + nrow(q14_table_2022) + 8, startCol = 1)
writeData(q14, "q14_q10", q14_table_2023, startRow = nrow(q14_table_2021) + nrow(q14_table_2022) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q14, archivo, overwrite = TRUE)


##############################################################################

#ANÁLISIS DE LA Q15

archivo <- "cuadros/q15_funds_type.xlsx"

#Convertir campos vacíos de la variable q15 en NA
base <- base %>%
  mutate(q15_key_sources = na_if(q15_key_sources, ""))


q15 <- base %>%
  filter( !is.na(q15_key_sources)) %>%
  summarise(Total = n(),
            Multirateral = sum(q15_key_sources.multilateral_funders == 1),
            Bilateral = sum(q15_key_sources.bilateral_funders == 1),
            Philanthropic = sum(q15_key_sources.philanthropic_foundations == 1),
            Womens = sum(q15_key_sources.womens_feminist_funds == 1),
            Private = sum(q15_key_sources.private_sector == 1),
            Ingos = sum(q15_key_sources.ingos == 1),
            Individual = sum(q15_key_sources.individual_donors == 1),
            Goverments = sum(q15_key_sources.national_local_goverment_or_bodies == 1),
            Other = sum(q15_key_sources.98 == 1)) %>% 
  pivot_longer(cols = everything(),
             names_to = "Source",
             values_to = "N")


write.xlsx(q15, file = archivo, sheetName="q15")


###

#cruce por q5


q15_q5 <- base %>%
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter( !is.na(q15_key_sources)) %>%
  group_by(q5) %>% 
  summarise(Total = n(),
            Multilateral = sum(q15_key_sources.multilateral_funders == 1),
            Bilateral = sum(q15_key_sources.bilateral_funders == 1),
            Philanthropic = sum(q15_key_sources.philanthropic_foundations == 1),
            Womens = sum(q15_key_sources.womens_feminist_funds == 1),
            Private = sum(q15_key_sources.private_sector == 1),
            Ingos = sum(q15_key_sources.ingos == 1),
            Individual = sum(q15_key_sources.individual_donors == 1),
            Governments = sum(q15_key_sources.national_local_goverment_or_bodies == 1),
            Other = sum(q15_key_sources.98 == 1)) 


q15 <- loadWorkbook(archivo)
addWorksheet(q15, sheetName = "q15_q5")
writeData(q15, sheet = "q15_q5", x = q15_q5)
saveWorkbook(q15, archivo, overwrite = TRUE)

###

#cruce por q6


q15_q6 <- base %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter( !is.na(q15_key_sources)) %>%
  group_by(q6) %>% 
  summarise(Total = n(),
            Multilateral = sum(q15_key_sources.multilateral_funders == 1),
            Bilateral = sum(q15_key_sources.bilateral_funders == 1),
            Philanthropic = sum(q15_key_sources.philanthropic_foundations == 1),
            Womens = sum(q15_key_sources.womens_feminist_funds == 1),
            Private = sum(q15_key_sources.private_sector == 1),
            Ingos = sum(q15_key_sources.ingos == 1),
            Individual = sum(q15_key_sources.individual_donors == 1),
            Governments = sum(q15_key_sources.national_local_goverment_or_bodies == 1),
            Other = sum(q15_key_sources.98 == 1)) 


q15 <- loadWorkbook(archivo)
addWorksheet(q15, sheetName = "q15_q6")
writeData(q15, sheet = "q15_q6", x = q15_q6)
saveWorkbook(q15, archivo, overwrite = TRUE)

###

#cruce por q9


q15_q9 <- base %>% 
  filter( !is.na(q15_key_sources)) %>%
  group_by(q9_year_formation_agrup) %>% 
  summarise(Total = n(),
            Multilateral = sum(q15_key_sources.multilateral_funders == 1),
            Bilateral = sum(q15_key_sources.bilateral_funders == 1),
            Philanthropic = sum(q15_key_sources.philanthropic_foundations == 1),
            Womens = sum(q15_key_sources.womens_feminist_funds == 1),
            Private = sum(q15_key_sources.private_sector == 1),
            Ingos = sum(q15_key_sources.ingos == 1),
            Individual = sum(q15_key_sources.individual_donors == 1),
            Governments = sum(q15_key_sources.national_local_goverment_or_bodies == 1),
            Other = sum(q15_key_sources.98 == 1)) 


q15 <- loadWorkbook(archivo)
addWorksheet(q15, sheetName = "q15_q9")
writeData(q15, sheet = "q15_q9", x = q15_q9)
saveWorkbook(q15, archivo, overwrite = TRUE)


#####


#CRUCE POR q10

# Crear q10_2021_q15
q10_2021_q15 <- base %>% 
  filter(q9_year_formation < 2022 & !is.na(q15_key_sources)) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(Total = n(),
            Multilateral = sum(q15_key_sources.multilateral_funders == 1),
            Bilateral = sum(q15_key_sources.bilateral_funders == 1),
            Philanthropic = sum(q15_key_sources.philanthropic_foundations == 1),
            Womens = sum(q15_key_sources.womens_feminist_funds == 1),
            Private = sum(q15_key_sources.private_sector == 1),
            Ingos = sum(q15_key_sources.ingos == 1),
            Individual = sum(q15_key_sources.individual_donors == 1),
            Governments = sum(q15_key_sources.national_local_goverment_or_bodies == 1),
            Other = sum(q15_key_sources.98 == 1)) %>%  
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q13
q10_2022_q15 <- base %>% 
  filter(q9_year_formation < 2023 & !is.na(q15_key_sources)) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(Total = n(),
            Multilateral = sum(q15_key_sources.multilateral_funders == 1),
            Bilateral = sum(q15_key_sources.bilateral_funders == 1),
            Philanthropic = sum(q15_key_sources.philanthropic_foundations == 1),
            Womens = sum(q15_key_sources.womens_feminist_funds == 1),
            Private = sum(q15_key_sources.private_sector == 1),
            Ingos = sum(q15_key_sources.ingos == 1),
            Individual = sum(q15_key_sources.individual_donors == 1),
            Governments = sum(q15_key_sources.national_local_goverment_or_bodies == 1),
            Other = sum(q15_key_sources.98 == 1)) %>%   
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q13
q10_2023_q15 <- base %>% 
  filter(!is.na(q10_budget_year_2023) & !is.na(q15_key_sources)) %>%
  group_by(q10_budget_grp_2023) %>% 
  summarise(Total = n(),
            Multilateral = sum(q15_key_sources.multilateral_funders == 1),
            Bilateral = sum(q15_key_sources.bilateral_funders == 1),
            Philanthropic = sum(q15_key_sources.philanthropic_foundations == 1),
            Womens = sum(q15_key_sources.womens_feminist_funds == 1),
            Private = sum(q15_key_sources.private_sector == 1),
            Ingos = sum(q15_key_sources.ingos == 1),
            Individual = sum(q15_key_sources.individual_donors == 1),
            Governments = sum(q15_key_sources.national_local_goverment_or_bodies == 1),
            Other = sum(q15_key_sources.98 == 1)) %>%   
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)




# Unir los dataframes
q10_grouped_q15 <- bind_rows(q10_2021_q15, q10_2022_q15, q10_2023_q15)

# Calcular la media entre los años para cada categoría de q15, agrupando por las categorías de q10
final_means_q15 <- q10_grouped_q15 %>%
  group_by(Annual_budget) %>%  # Agrupando por la categoría de q10
  summarise(
    Total=round(mean(Total, na.rm=TRUE),0),
    Multilateral = round(mean(Multilateral, na.rm = TRUE),0),
    Bilateral = round(mean(Bilateral, na.rm = TRUE),0),
    Philanthropic = round(mean(Philanthropic, na.rm = TRUE),0),
    Womens = round(mean(Womens, na.rm = TRUE),0),
    Private = round(mean(Private, na.rm = TRUE),0),
    Ingos = round(mean(Ingos, na.rm = TRUE),0),
    Individual = round(mean(Individual, na.rm = TRUE),0),
    Governments = round(mean(Governments, na.rm = TRUE),0),
    Other = round(mean(Other, na.rm = TRUE),0))




q15 <- loadWorkbook(archivo)
addWorksheet(q15, sheetName = "q15_q10")
# Escribir las tablas en la misma hoja
writeData(q15, "q15_q10", "Table for 2021", startRow = 1, startCol = 1)
writeData(q15, "q15_q10", q10_2021_q15, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q15, "q15_q10", "Table for 2022", startRow = nrow(q10_2021_q15) + 4, startCol = 1)
writeData(q15, "q15_q10", q10_2022_q15, startRow = nrow(q10_2021_q15) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q15, "q15_q10", "Table for 2023", startRow = nrow(q10_2021_q15) + nrow(q10_2022_q15) + 8, startCol = 1)
writeData(q15, "q15_q10", q10_2023_q15, startRow = nrow(q10_2021_q15) + nrow(q10_2022_q15) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q15, archivo, overwrite = TRUE)


q15 <- loadWorkbook(archivo)
addWorksheet(q15, sheetName = "q15_q10_media")
writeData(q15, sheet = "q15_q10_media", x = final_means_q15)
saveWorkbook(q15, archivo, overwrite = TRUE)


#############################################################################
#ANÁLISIS DE LA q25

archivo <- "cuadros/q25_counter_anti.xlsx"



q25<-base %>%
  filter(!is.na(q25_counter_anti)) %>% 
  group_by(q25_counter_anti) %>% 
  summarise(n=n()) 

write.xlsx(q25, file = archivo, sheetName="q25")

###

#CRUCE POR q5



# Crear q25_q5
q25_q5 <- base %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q25_counter_anti)) %>% 
  group_by(q25_counter_anti, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Funding" = q25_counter_anti)

q25_q5 <- q25_q5 %>%
  pivot_wider(names_from = q5, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Funding`)


q25 <- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_q5")
writeData(q25, sheet = "q25_q5", x = q25_q5)
saveWorkbook(q25, archivo, overwrite = TRUE)



#######

#CRUCE POR q6

q25_q6 <- base %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q25_counter_anti)) %>% 
  group_by(q25_counter_anti, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Funding" = q25_counter_anti)

q25_q6 <- q25_q6 %>%
  pivot_wider(names_from = q6, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Funding`)


q25 <- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_q6")
writeData(q25, sheet = "q25_q6", x = q25_q6)
saveWorkbook(q25, archivo, overwrite = TRUE)

#####

#CRUCE POR q9

q25_q9 <- base %>% 
  filter(!is.na(q25_counter_anti)) %>% 
  group_by(q25_counter_anti, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Funding" = q25_counter_anti)

q25_q9 <- q25_q9 %>%
  pivot_wider(names_from = q9_year_formation_agrup, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Funding`)

q25 <- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_q9")
writeData(q25, sheet = "q25_q9", x = q25_q9)
saveWorkbook(q25, archivo, overwrite = TRUE)



###

#CRUCE POR q10

# Crear q10_2021_q25
q10_2021_q25 <- base %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q25_counter_anti) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q25
q10_2022_q25 <- base %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q25_counter_anti) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q25
q10_2023_q25 <- base %>% 
  filter(!is.na(q10_budget_year_2023)) %>%
  group_by(q10_budget_grp_2023, q25_counter_anti) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q25 <- bind_rows(q10_2021_q25, q10_2022_q25, q10_2023_q25)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q25 %>%
  group_by(Year, Annual_budget, q25_counter_anti) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q25_counter_anti, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q25 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q25_counter_anti, Media) %>%
  pivot_wider(names_from = q25_counter_anti, values_from = Media, values_fill = list(Media = 0))

q25 <- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_q10")
writeData(q25, sheet = "q25_q10", x = q10_table)
saveWorkbook(q25, archivo, overwrite = TRUE)

q25<- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_q10_media")
writeData(q25, sheet = "q25_q10_media", x = q10_media_table)
saveWorkbook(q25, archivo, overwrite = TRUE)

################################################################################


#ANÁLISIS DE LA q30

archivo <- "cuadros/q30_shifting_power.xlsx"

unique(base$q30_shift_priorities)

q30<-base %>%
  filter(!is.na(q30_shift_priorities)) %>% 
  group_by(q30_shift_priorities) %>% 
  summarise(n=n()) 

write.xlsx(q30, file = archivo, sheetName="q30")

###

#CRUCE POR q5



# Crear q30_q5
q30_q5 <- base %>% 
  mutate(q5 = case_when(
    q5_registered == "n_registered" ~ "No",
    q5_registered == "y_registered" ~ "Yes",
    q5_registered == "98" ~ "Other",
    is.na(q5_registered) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q30_shift_priorities)) %>% 
  group_by(q30_shift_priorities, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Shift priorities" = q30_shift_priorities)

q30_q5 <- q30_q5 %>%
  pivot_wider(names_from = q5, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Shift priorities`)


q30 <- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_q5")
writeData(q30, sheet = "q30_q5", x = q30_q5)
saveWorkbook(q30, archivo, overwrite = TRUE)



#######

#CRUCE POR q6

q30_q6 <- base %>% 
  mutate(q6 = case_when(
    q6_geo_scope=="globally_focused" ~ "Global",
    q6_geo_scope=="transnationally_focused" ~ "Transnational",
    q6_geo_scope=="regionally_focused" ~ "Regional",
    q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6_geo_scope=="nationally_focused" ~ "National",
    q6_geo_scope=="locally_focused" ~ "Local",
    is.na(q6_geo_scope) ~ "No information",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(q30_shift_priorities)) %>% 
  group_by(q30_shift_priorities, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Shift priorities" = q30_shift_priorities)

q30_q6 <- q30_q6 %>%
  pivot_wider(names_from = q6, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Shift priorities`)


q30 <- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_q6")
writeData(q30, sheet = "q30_q6", x = q30_q6)
saveWorkbook(q30, archivo, overwrite = TRUE)

#####

#CRUCE POR q9

q30_q9 <- base %>% 
  filter(!is.na(q30_shift_priorities)) %>% 
  group_by(q30_shift_priorities, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Shift priorities" = q30_shift_priorities)

q30_q9 <- q30_q9 %>%
  pivot_wider(names_from = q9_year_formation_agrup, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Shift priorities`)

q30 <- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_q9")
writeData(q30, sheet = "q30_q9", x = q30_q9)
saveWorkbook(q30, archivo, overwrite = TRUE)



###

#CRUCE POR q10

# Crear q10_2021_q30
q10_2021_q30 <- base %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q30_shift_priorities) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q30
q10_2022_q30 <- base %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q30_shift_priorities) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q30
q10_2023_q30 <- base %>% 
  filter(!is.na(q10_budget_year_2023)) %>%
  group_by(q10_budget_grp_2023, q30_shift_priorities) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q30 <- bind_rows(q10_2021_q30, q10_2022_q30, q10_2023_q30)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q30 %>%
  group_by(Year, Annual_budget, q30_shift_priorities) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q30_shift_priorities, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q30 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q30_shift_priorities, Media) %>%
  pivot_wider(names_from = q30_shift_priorities, values_from = Media, values_fill = list(Media = 0))

q30 <- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_q10")
writeData(q30, sheet = "q30_q10", x = q10_table)
saveWorkbook(q30, archivo, overwrite = TRUE)

q30<- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_q10_media")
writeData(q30, sheet = "q30_q10_media", x = q10_media_table)
saveWorkbook(q30, archivo, overwrite = TRUE)
