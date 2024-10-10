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

witm <- read.csv("Data/WITM_FINAL_10102024.csv", header=T, sep=";")


##############################################################################

# ANÁLISIS Q10

archivo <- "cuadros/q10_budget.xlsx"


q10_2021<-witm %>% 
  filter(q9_year_formation<2022) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(n=n()) %>% 
  rename("Annual_budget"=q10_budget_grp_2021) %>% 
  mutate(Year=2021)

q10_2022<-witm %>% 
  filter(q9_year_formation<2023) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(n=n()) %>% 
  rename("Annual_budget"=q10_budget_grp_2022) %>% 
  mutate(Year=2022)

q10_2023<-witm %>% 
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

# Crear q10_2021_q4agrup
q10_2021_q4agrup <- witm %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

q10_2021_q4agrup
# Crear q10_2022_q4agrup
q10_2022_q4agrup <- witm %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>%
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q4agrup
q10_2023_q4agrup <- witm %>%  
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>%
  filter(!is.na(q10_budget_year_2023)) %>% 
  group_by(q10_budget_grp_2023, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)

# Unir los dataframes
q10_grouped_q4agrup <- bind_rows(q10_2021_q4agrup, q10_2022_q4agrup, q10_2023_q4agrup)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q4agrup %>%
  group_by(Year, Annual_budget, q4_awid_focus) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q4_awid_focus, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q4agrup %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q4_awid_focus, Media) %>%
  pivot_wider(names_from = q4_awid_focus, values_from = Media, values_fill = list(Media = 0))

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q4agrup")
writeData(q10, sheet = "q10_q4agrup", x = q10_table)
saveWorkbook(q10, archivo, overwrite = TRUE)

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q4agrup_media")
writeData(q10, sheet = "q10_q4agrup_media", x = q10_media_table)
saveWorkbook(q10, archivo, overwrite = TRUE)


 q10 <- loadWorkbook(archivo)
 addWorksheet(q10, sheetName = "q4_agrup")
 writeData(q10, sheet = "q4_agrup", x = q10_grouped_q4agrup)
 saveWorkbook(q10, archivo, overwrite = TRUE)


###


#cruce por q4



# Crear q10_2021_q4
q10_2021_q4 <- witm %>%
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>%  
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q4
q10_2022_q4 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>%
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q4
q10_2023_q4 <- witm %>% 
  filter(!is.na(q10_budget_year_2023)) %>% 
  group_by(q10_budget_grp_2023) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q4 <- bind_rows(q10_2021_q4, q10_2022_q4, q10_2023_q4)


# Calcular el promedio para cada categoría de Annual_budget y redondear a 0 decimales
q10_promedios <- q10_grouped_q4 %>%
  group_by(Annual_budget) %>%
  summarise(
    Promedio_Total = round(mean(Total, na.rm = TRUE), 0),
    Promedio_LGTBIQ = round(mean(LGTBIQ, na.rm = TRUE), 0),
    Promedio_Young = round(mean(Young, na.rm = TRUE), 0),
    Promedio_Sex_workers = round(mean(Sex_workers, na.rm = TRUE), 0),
    Promedio_Anti_caste = round(mean(Anti_caste, na.rm = TRUE), 0),
    Promedio_Climate = round(mean(Climate, na.rm = TRUE), 0),
    Promedio_Countering_anti = round(mean(Countering_anti, na.rm = TRUE), 0),
    Promedio_Harm_reduction = round(mean(Harm_reduction, na.rm = TRUE), 0),
    Promedio_Disability_rights = round(mean(Disability_rights, na.rm = TRUE), 0)
  ) %>%
  arrange(Annual_budget) 



q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q4")
# Escribir las tablas en la misma hoja
writeData(q10, "q10_q4", "Table for 2021", startRow = 1, startCol = 1)
writeData(q10, "q10_q4", q10_2021_q4, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q10, "q10_q4", "Table for 2022", startRow = nrow(q10_2021_q4) + 4, startCol = 1)
writeData(q10, "q10_q4", q10_2022_q4, startRow = nrow(q10_2021_q4) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q10, "q10_q4", "Table for 2023", startRow = nrow(q10_2021_q4) + nrow(q10_2022_q4) + 8, startCol = 1)
writeData(q10, "q10_q4", q10_2023_q4, startRow = nrow(q10_2021_q4) + nrow(q10_2022_q4) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q10, archivo, overwrite = TRUE)

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_q4_media")
writeData(q10, sheet = "q10_q4_media", x = q10_promedios)
saveWorkbook(q10, archivo, overwrite = TRUE)


####

#cruce por q5 registrados


# Crear q10_2021_q5
q10_2021_q5 <- witm %>%
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q5
q10_2022_q5 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q5
q10_2023_q5 <- witm %>% 
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
q10_2021_q6 <- witm %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q6
q10_2022_q6 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q6
q10_2023_q6 <- witm %>% 
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

#cruce por región: q7

# Crear q10_2021_q7
q10_2021_region <- witm %>%
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))%>%  
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q7
q10_2022_region <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))%>%
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q7
q10_2023_region <- witm %>% 
  filter(!is.na(q10_budget_year_2023)) %>% 
  group_by(q10_budget_grp_2023) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1)) %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_region <- bind_rows(q10_2021_region, q10_2022_region, q10_2023_region)


# Calcular el promedio para cada categoría de Annual_budget y redondear a 0 decimales
q10_promedios_region <- q10_grouped_region %>%
  group_by(Annual_budget) %>%
  summarise(
    Promedio_Total = round(mean(Total, na.rm = TRUE), 0),
    Promedio_Latin_America = round(mean(`1. Latin America & the Caribbean`, na.rm = TRUE), 0),
    Promedio_Western_Europe = round(mean(`2. Western Europe & North America`, na.rm = TRUE), 0),
    Promedio_Eastern_Europe = round(mean(`3. Eastern, Southeast and Central Europe`, na.rm = TRUE), 0),
    Promedio_Africa = round(mean(`4. Africa`, na.rm = TRUE), 0),
    Promedio_Asia_Pacific = round(mean(`5. Asia & the Pacific`, na.rm = TRUE), 0),
    Promedio_Central_Asia = round(mean(`6. Central Asia & Caucasus`, na.rm = TRUE), 0),
    Promedio_South_West_Asia = round(mean(`7. South West Asia/Middle East & North Africa`, na.rm = TRUE), 0)
  ) %>%
  arrange(Annual_budget)  # Opcional: ordenar por Annual_budget



q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_region")
# Escribir las tablas en la misma hoja
writeData(q10, "q10_region", "Table for 2021", startRow = 1, startCol = 1)
writeData(q10, "q10_region", q10_2021_region, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q10, "q10_region", "Table for 2022", startRow = nrow(q10_2021_region) + 4, startCol = 1)
writeData(q10, "q10_region", q10_2022_region, startRow = nrow(q10_2021_region) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q10, "q10_region", "Table for 2023", startRow = nrow(q10_2021_region) + nrow(q10_2022_region) + 8, startCol = 1)
writeData(q10, "q10_region", q10_2023_region, startRow = nrow(q10_2021_region) + nrow(q10_2022_region) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q10, archivo, overwrite = TRUE)

q10 <- loadWorkbook(archivo)
addWorksheet(q10, sheetName = "q10_region_media")
writeData(q10, sheet = "q10_region_media", x = q10_promedios_region)
saveWorkbook(q10, archivo, overwrite = TRUE)



#######


## cruce por q9 year of formation

# Crear q10_2021_q9
q10_2021_q9 <- witm %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q6
q10_2022_q9 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q6
q10_2023_q9 <- witm %>% 
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

#ANÁLISIS DE LA Q11

archivo <- "cuadros/cuadros2.xlsx"

base<-witm
q11<-base %>%
  filter(!is.na(q11_covid_incidence) & q11_covid_incidence!="not_applicable" & q11_covid_incidence!="")%>%
  group_by(q11_covid_incidence) %>% 
  mutate(q11_covid_incidence= case_when(
    q11_covid_incidence=="grown_substantially" ~ "1 Grow substantively",
    q11_covid_incidence=="grown_slightly" ~ "2 Grow slightly",
    q11_covid_incidence=="stayed_the_same" ~ "3 Stay the same",
    q11_covid_incidence=="decreased_slightly" ~ "4 Decrease slightly",
    q11_covid_incidence=="decreased_substantially" ~ "5 Decrease substantially",
    # q11_covid_incidence=="not_applicable" ~ NA,
    TRUE ~ NA)) %>%  
  summarise(Count=n()) %>% 
  mutate(Percentage=round((Count/sum(Count))*100,1)) %>% 
  arrange(q11_covid_incidence) 


#cruce por región: q7

q11_region<-witm %>% 
  mutate(q11_covid_incidence= case_when(
  q11_covid_incidence=="grown_substantially" ~ "1 Grow substantively",
  q11_covid_incidence=="grown_slightly" ~ "2 Grow slightly",
  q11_covid_incidence=="stayed_the_same" ~ "3 Stay the same",
  q11_covid_incidence=="decreased_slightly" ~ "4 Decrease slightly",
  q11_covid_incidence=="decreased_substantially" ~ "5 Decrease substantially",
  # q11_covid_incidence=="not_applicable" ~ NA,
  TRUE ~ NA)) %>%  
  filter(!is.na(q11_covid_incidence)) %>% 
  group_by(q11_covid_incidence) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))

write.xlsx(q11_region, file = archivo, sheetName="q11_region")

#################################################################################


# ANÁLISIS DE LA q13

archivo <- "cuadros/q13_external_funding.xlsx"



q13<-witm %>%
  filter(!is.na(q13_ext_funding)) %>% 
  group_by(q13_ext_funding) %>% 
  summarise(n=n()) 

write.xlsx(q13, file = archivo, sheetName="q13")

#####

#cruce por q4agrup


# Crear q13_q4agrup
q13_q4agrup <- witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  filter(!is.na(q13_ext_funding)) %>% 
  group_by(q13_ext_funding, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("External funding" = q13_ext_funding)

q13_q4agrup <- q13_q4agrup %>%
  pivot_wider(names_from = q4_awid_focus, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`External funding`)


q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_q4agrup")
writeData(q13, sheet = "q13_q4agrup", x = q13_q4agrup)
saveWorkbook(q13, archivo, overwrite = TRUE)


#cruce por q4


q13_q4 <- witm %>%
  filter( !is.na(q13_ext_funding) & !is.na(q4_forms_organizing)) %>%
  group_by(q13_ext_funding ) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1))  


q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_q4")
writeData(q13, sheet = "q13_q4", x = q13_q4)
saveWorkbook(q13, archivo, overwrite = TRUE)



## CRUCE POR Q5



# Crear q13_q5
q13_q5 <- witm %>% 
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

q13_q6 <- witm %>% 
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


#cruce por región: q7

q13_region<-witm %>% 
  filter(!is.na(q13_ext_funding)) %>% 
  group_by(q13_ext_funding) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))



q13 <- loadWorkbook(archivo)
addWorksheet(q13, sheetName = "q13_region")
writeData(q13, sheet = "q13_region", x = q13_region)
saveWorkbook(q13, archivo, overwrite = TRUE)



#####

#CRUCE POR q9

q13_q9 <- witm %>% 
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
q10_2021_q13 <- witm %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q13_ext_funding) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q13
q10_2022_q13 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q13_ext_funding) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q13
q10_2023_q13 <- witm %>% 
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


q14_2023 <- witm %>%
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



q14_2022 <- witm %>%
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

q14_2021 <- witm %>%
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


#CRUCE POR q4agrup

q14_2023_q4agrup <- witm %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  group_by(q14_funding_annual_budget_2023, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2023)

q14_2022_q4agrup <- witm %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  group_by(q14_funding_annual_budget_2022, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2022)


q14_2021_q4agrup <- witm %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  group_by(q14_funding_annual_budget_2021, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2021)


# Unir los dataframes
q14_grouped_q4agrup <- bind_rows(q14_2021_q4agrup, q14_2022_q4agrup, q14_2023_q4agrup)



# Crear la tabla de doble entrada
q14_table <- q14_grouped_q4agrup %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q14)  # Opcional: ordenar por la columna de "q14"

# Crear la tabla de doble entrada con q14 y q5
q14_table <- q14_grouped_q4agrup %>%
  group_by(Year, q14, q4_awid_focus) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q14, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q4_awid_focus)  # Opcional: ordenar por el año y q5

q14 <- loadWorkbook(archivo)
addWorksheet(q14, sheetName = "q14_q4agrup")
writeData(q14, sheet = "q14_q4agrup", x = q14_table)
saveWorkbook(q14, archivo, overwrite = TRUE)

####


#cruce por q4


q14_2023_q4 <- witm %>%
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
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>% 
  rename("q14"=1) %>% 
  mutate(Year=2023)


q14_2022_q4 <- witm %>%
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
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>% 
  rename("q14"=1) %>% 
  mutate(Year=2022)



q14_2021_q4 <- witm %>%
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
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>% 
  rename("q14"=1) %>% 
  mutate(Year=2021)

q14_unificada<-bind_rows(q14_2021_q4, q14_2022_q4, q14_2023_q4)



q14 <- loadWorkbook(archivo)
addWorksheet(q14, sheetName = "q14_q4")
# Escribir las tablas en la misma hoja
writeData(q14, "q14_q4", "Table for 2021", startRow = 1, startCol = 1)
writeData(q14, "q14_q4", q14_2021_q4, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q14, "q14_q4", "Table for 2022", startRow = nrow(q14_2021_q4) + 4, startCol = 1)
writeData(q14, "q14_q4", q14_2022_q4, startRow = nrow(q14_2021_q4) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q14, "q14_q4", "Table for 2023", startRow = nrow(q14_2021_q4) + nrow(q14_2022_q4) + 8, startCol = 1)
writeData(q14, "q14_q4", q14_2023_q4, startRow = nrow(q14_2021_q4) + nrow(q14_2022_q4) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q14, archivo, overwrite = TRUE)


###

#CRUCE POR q5

q14_2023_q5 <- witm %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2023, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2023)

q14_2022_q5 <- witm %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2022, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2022)


q14_2021_q5 <- witm %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
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


q14_2023_q6 <- witm %>%
  filter(q13_ext_funding == "yes") %>%
  mutate(q14_funding_annual_budget_2023=case_when(
    q14_funding_annual_budget_2023=="0" ~ "0 Zero",
    q14_funding_annual_budget_2023=="10" | q14_funding_annual_budget_2023=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2023=="30" | q14_funding_annual_budget_2023=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2023=="50" | q14_funding_annual_budget_2023=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2023=="70" | q14_funding_annual_budget_2023=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2023=="90" | q14_funding_annual_budget_2023=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2023, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2023)

q14_2022_q6 <- witm %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2023) %>%
  mutate(q14_funding_annual_budget_2022=case_when(
    q14_funding_annual_budget_2022=="0" ~ "0 Zero",
    q14_funding_annual_budget_2022=="10" | q14_funding_annual_budget_2022=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2022=="30" | q14_funding_annual_budget_2022=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2022=="50" | q14_funding_annual_budget_2022=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2022=="70" | q14_funding_annual_budget_2022=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2022=="90" | q14_funding_annual_budget_2022=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q14_funding_annual_budget_2022, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q14"=1) %>% 
  mutate(Year = 2022)


q14_2021_q6 <- witm %>%
  filter(q13_ext_funding == "yes" & q9_year_formation<2022) %>%
  mutate(q14_funding_annual_budget_2021=case_when(
    q14_funding_annual_budget_2021=="0" ~ "0 Zero",
    q14_funding_annual_budget_2021=="10" | q14_funding_annual_budget_2021=="20"  ~ "1 Lower than 30%",
    q14_funding_annual_budget_2021=="30" | q14_funding_annual_budget_2021=="40" ~ "2 Between 30% and 40%",
    q14_funding_annual_budget_2021=="50" | q14_funding_annual_budget_2021=="60" ~ "3 Between 50% and 60%",
    q14_funding_annual_budget_2021=="70" | q14_funding_annual_budget_2021=="80" ~ "4 Between 70% and 80%",
    q14_funding_annual_budget_2021=="90" | q14_funding_annual_budget_2021=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
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

#cruce por región

q14_2023_region <- witm %>%
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
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1)) %>% 
  rename("q14"=1) %>% 
  mutate(Year=2023)


q14_2022_region <- witm %>%
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
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1)) %>% 
  rename("q14"=1) %>% 
  mutate(Year=2022)



q14_2021_region <- witm %>%
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
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1)) %>% 
  rename("q14"=1) %>% 
  mutate(Year=2021)

q14_region_unificada<-bind_rows(q14_2021_region, q14_2022_region, q14_2023_region)



q14 <- loadWorkbook(archivo)
addWorksheet(q14, sheetName = "q14_region")
# Escribir las tablas en la misma hoja
writeData(q14, "q14_region", "Table for 2021", startRow = 1, startCol = 1)
writeData(q14, "q14_region", q14_2021_region, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q14, "q14_region", "Table for 2022", startRow = nrow(q14_2021_region) + 4, startCol = 1)
writeData(q14, "q14_region", q14_2022_region, startRow = nrow(q14_2021_region) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q14, "q14_region", "Table for 2023", startRow = nrow(q14_2021_region) + nrow(q14_2022_region) + 8, startCol = 1)
writeData(q14, "q14_region", q14_2023_region, startRow = nrow(q14_2021_region) + nrow(q14_2022_region) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q14, archivo, overwrite = TRUE)


######

#CRUCE POR q9

q14_2023_q9 <- witm %>%
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

q14_2022_q9 <- witm %>%
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


q14_2021_q9 <- witm %>%
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



q14_2021_q10 <- witm %>%
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
  

q14_2022_q10 <- witm %>%
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
  


q14_2023_q10 <- witm %>%
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
witm <- witm %>%
  mutate(q15_key_sources = na_if(q15_key_sources, ""))


q15 <- witm %>%
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


#cruce por q4


q15_q4agrup <- witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>%
  filter( !is.na(q15_key_sources)) %>%
  group_by(q4_awid_focus) %>% 
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
addWorksheet(q15, sheetName = "q15_q4agrup")
writeData(q15, sheet = "q15_q4agrup", x = q15_q4agrup)
saveWorkbook(q15, archivo, overwrite = TRUE)

 
#Cruce por q4


q15_q4a <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_LGBTIQ==1) %>%
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
               values_to = "LGBTIQ")

q15_q4b <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_young==1) %>%
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
               values_to = "Young")

q15_q4c <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_sex==1) %>%
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
               values_to = "Sex workers")

q15_q4d <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_anticaste==1) %>%
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
               values_to = "Anticaste")

q15_q4e <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_climate==1) %>%
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
               values_to = "Climate")

q15_q4f <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_antigender==1) %>%
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
               values_to = "Countering anti-gender & anti-rights")

q15_q4g <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_harm==1) %>%
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
               values_to = "Harm reduction")
  
q15_q4h <- witm %>%
  filter( !is.na(q15_key_sources) & q4_awid_disability==1) %>%
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
               values_to = "Disability rights")

q15_q4_total <- q15_q4a %>%
  left_join(q15_q4b, by = "Source") %>%
  left_join(q15_q4c, by = "Source") %>%
  left_join(q15_q4d, by = "Source") %>%
  left_join(q15_q4e, by = "Source") %>%
  left_join(q15_q4f, by = "Source") %>%
  left_join(q15_q4g, by = "Source") %>%
  left_join(q15_q4h, by = "Source")



q15 <- loadWorkbook(archivo)
addWorksheet(q15, sheetName = "q15_q4_total")
writeData(q15, sheet = "q15_q4_total", x = q15_q4_total)
saveWorkbook(q15, archivo, overwrite = TRUE)

#cruce por q5


q15_q5 <- witm %>%
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


q15_q6 <- witm %>% 
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

#cruce por región: q7



q15_region_1 <- witm %>%
  filter( !is.na(q15_key_sources) & region_1==1) %>%
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
               values_to = "1. Latin America & the Caribbean")

q15_region_2 <- witm %>%
  filter( !is.na(q15_key_sources) & region_2==1) %>%
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
               values_to = "2. Western Europe & North America")




  
q15_region_3 <- witm %>%
  filter( !is.na(q15_key_sources) & region_3==1) %>%
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
               values_to = "3. Eastern, Southeast and Central Europe")

q15_region_4 <- witm %>%
  filter( !is.na(q15_key_sources) & region_4==1) %>%
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
               values_to = "4. Africa")


q15_region_5 <- witm %>%
  filter( !is.na(q15_key_sources) & region_5==1) %>%
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
               values_to = "5. Asia & the Pacific")

q15_region_6 <- witm %>%
  filter( !is.na(q15_key_sources) & region_6==1) %>%
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
               values_to = "6. Central Asia & Caucasus")

q15_region_7 <- witm %>%
  filter( !is.na(q15_key_sources) & region_7==1) %>%
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
               values_to = "7. South West Asia/Middle East & North Africa")



q15_region_total <- q15_region_1 %>%
  left_join(q15_region_2, by = "Source") %>%
  left_join(q15_region_3, by = "Source") %>%
  left_join(q15_region_4, by = "Source") %>%
  left_join(q15_region_5, by = "Source") %>%
  left_join(q15_region_6, by = "Source") %>%
  left_join(q15_region_7, by = "Source")



q15 <- loadWorkbook(archivo)
addWorksheet(q15, sheetName = "q15_region")
writeData(q15, sheet = "q15_region", x = q15_region_total)
saveWorkbook(q15, archivo, overwrite = TRUE)


###



#cruce por q9


q15_q9 <- witm %>% 
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
q10_2021_q15 <- witm %>% 
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

# Crear q10_2022_q15
q10_2022_q15 <- witm %>% 
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

# Crear q10_2023_q15
q10_2023_q15 <- witm %>% 
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

############################################################################

#ANÁLISIS DE LA Q16

archivo <- "cuadros/q16_percentage.xlsx"

#Convertir campos vacíos de la variable q13 en NA
datos <- witm %>%
  mutate(q13_ext_funding = na_if(q13_ext_funding, ""))

#MULTIRATERAL
q16_multirateral_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.multilateral_funders==1) %>% 
  group_by(q16_funding_source_2023_multilateral) %>% 
  summarise(n = n()) %>%
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Multirateral")

q16_multirateral_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.multilateral_funders==1) %>% 
  group_by(q16_funding_source_2022_multilateral) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Multirateral")

q16_multirateral_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.multilateral_funders==1) %>% 
  group_by(q16_funding_source_2021_multilateral) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1)%>% 
  mutate(Type="Multirateral")

#BILATERAL
q16_bilateral_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.bilateral_funders==1) %>% 
  group_by(q16_funding_source_2023_bilateral) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Bilateral")

q16_bilateral_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.bilateral_funders==1) %>% 
  group_by(q16_funding_source_2022_bilateral) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Bilateral")

q16_bilateral_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.bilateral_funders==1) %>% 
  group_by(q16_funding_source_2021_bilateral) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="Bilateral")

#PHILANTHROPIC
q16_philanthropic_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.philanthropic_foundations==1) %>% 
  group_by(q16_funding_source_2023_philanthropic) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Philanthropic")

q16_philanthropic_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.philanthropic_foundations==1) %>% 
  group_by(q16_funding_source_2022_philanthropic) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Philanthropic")

q16_philanthropic_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.philanthropic_foundations==1) %>% 
  group_by(q16_funding_source_2021_philanthropic) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="Philanthropic")

#FEMINIST
q16_feminist_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.womens_feminist_funds ==1) %>% 
  group_by(q16_funding_source_2023_feminist) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Feminist")

q16_feminist_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.womens_feminist_funds ==1) %>% 
  group_by(q16_funding_source_2022_feminist) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Feminist")


q16_feminist_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.womens_feminist_funds ==1) %>% 
  group_by(q16_funding_source_2021_feminist) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="Feminist")


#PRIVATE
q16_private_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.private_sector ==1) %>% 
  group_by(q16_funding_source_2023_private) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Private")


q16_private_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.private_sector ==1) %>% 
  group_by(q16_funding_source_2022_private) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Private")

q16_private_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.private_sector ==1) %>% 
  group_by(q16_funding_source_2021_private) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="Private")

#INGOS
q16_ingos_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.ingos==1) %>% 
  group_by(q16_funding_source_2023_ingos) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="INGOS")

q16_ingos_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.ingos==1) %>% 
  group_by(q16_funding_source_2022_ingos) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="INGOS")

q16_ingos_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.ingos==1) %>% 
  group_by(q16_funding_source_2021_ingos) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="INGOS")

#INDIVIDUAL
q16_individual_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.individual_donors==1) %>% 
  group_by(q16_funding_source_2023_individual) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Individual")

q16_individual_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.individual_donors==1) %>% 
  group_by(q16_funding_source_2022_individual) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Individual")

q16_individual_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.individual_donors==1) %>% 
  group_by(q16_funding_source_2021_individual) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="Individual")

#GOVERMENT
q16_goverment_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.national_local_goverment_or_bodies ==1) %>% 
  group_by(q16_funding_source_2023_goverment) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Goverment")

q16_goverment_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.national_local_goverment_or_bodies ==1) %>% 
  group_by(q16_funding_source_2022_goverment) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Goverment")

q16_goverment_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.national_local_goverment_or_bodies ==1) %>% 
  group_by(q16_funding_source_2021_goverment) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="Goverment")

#OTHER
q16_other_2023<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.98==1) %>% 
  group_by(q16_funding_source_2023_other) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2023", 1) %>% 
  mutate(Type="Other")

q16_other_2022<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.98==1) %>% 
  group_by(q16_funding_source_2022_other) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2022", 1) %>% 
  mutate(Type="Other")

q16_other_2021<- datos %>% 
  filter(q13_ext_funding == "yes" & q15_key_sources.98==1) %>% 
  group_by(q16_funding_source_2021_other) %>% 
  summarise(n = n()) %>% 
  rename_with(~ "2021", 1) %>% 
  mutate(Type="Other")



#2023
# Unir los dataframes
ext_2023 <- bind_rows(q16_bilateral_2023,q16_feminist_2023, q16_goverment_2023, q16_individual_2023, q16_ingos_2023, q16_multirateral_2023, q16_other_2023, q16_philanthropic_2023, q16_private_2023)

# Crear un nuevo data frame con los resultados
q16_2023 <- ext_2023 %>%
  pivot_wider(names_from = Type, values_from = n) %>% 
  rename("Range"=1) %>% 
  mutate(Year="2023")

#2022
# Unir los dataframes
ext_2022 <- bind_rows(q16_bilateral_2022,q16_feminist_2022, q16_goverment_2022, q16_individual_2022, q16_ingos_2022, q16_multirateral_2022, q16_other_2022, q16_philanthropic_2022, q16_private_2022)

# Crear un nuevo data frame con los resultados
q16_2022 <- ext_2022 %>%
  pivot_wider(names_from = Type, values_from = c(n)) %>% 
  rename("Range"=1) %>% 
  mutate(Year="2022")

#2021
# Unir los dataframes
ext_2021 <- bind_rows(q16_bilateral_2021,q16_feminist_2021, q16_goverment_2021, q16_individual_2021, q16_ingos_2021, q16_multirateral_2021, q16_other_2021, q16_philanthropic_2021, q16_private_2021)

# Crear un nuevo data frame con los resultados
q16_2021 <- ext_2021 %>%
  pivot_wider(names_from = Type, values_from = c(n)) %>% 
  rename("Range"=1) %>% 
  mutate(Year="2021")

# Unir los dataframes
q16_grouped <- bind_rows(q16_2021, q16_2022, q16_2023)

# Calcular la media entre los años para cada categoría de q15, agrupando por las categorías de q10
final_means_q16 <- q16_grouped %>%
  group_by(Range) %>%  # Agrupando por la categoría de q10
  summarise(
    Multirateral = round(mean(Multirateral, na.rm = TRUE),0),
    Bilateral = round(mean(Bilateral, na.rm = TRUE),0),
    Philanthropic = round(mean(Philanthropic, na.rm = TRUE),0),
    Womens = round(mean(Feminist, na.rm = TRUE),0),
    Private = round(mean(Private, na.rm = TRUE),0),
    Ingos = round(mean(INGOS, na.rm = TRUE),0),
    Individual = round(mean(Individual, na.rm = TRUE),0),
    Governments = round(mean(Goverment, na.rm = TRUE),0),
    Other = round(mean(Other, na.rm = TRUE),0))


write.xlsx(final_means_q16, file = archivo, sheetName="q16_media")

q16 <- loadWorkbook(archivo)
addWorksheet(q16, sheetName = "q16")
# Escribir las tablas en la misma hoja
writeData(q16, "q16", "Table for 2021", startRow = 1, startCol = 1)
writeData(q16, "q16", q16_2021, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q16, "q16", "Table for 2022", startRow = nrow(q16_2021) + 4, startCol = 1)
writeData(q16, "q16", q16_2022, startRow = nrow(q16_2021) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q16, "q16", "Table for 2023", startRow = nrow(q16_2021) + nrow(q16_2021) + 8, startCol = 1)
writeData(q16, "q16", q16_2023, startRow = nrow(q16_2022) + nrow(q16_2022) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q16, archivo, overwrite = TRUE)

#####

#Cruce por q5

names(witm)

############################################################################

#ANÁLISIS DE LA Q18

archivo <- "cuadros/q18_new_funder.xlsx"


#Convertir campos vacíos de la variable q18 en NA
witm <- witm %>%
  mutate(q18_new_funders = na_if(q18_new_funders, ""))

q18 <- witm %>%
  filter(!is.na(q18_new_funders)) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "N")


write.xlsx(q18, file = archivo, sheetName="q18")

#####

#cruce por q4

q18_q4agrup <- witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>%
  filter(!is.na(q18_new_funders)) %>%
  group_by(q4_awid_focus) %>% 
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE))


q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_q4agrup")
writeData(q18, sheet = "q18_q4agrup", x = q18_q4agrup)
saveWorkbook(q18, archivo, overwrite = TRUE)

##

#Cruce por q4


q18_q4a <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_LGBTIQ==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "LGBTIQ")

q18_q4b <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_young==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Young")

q18_q4c <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_sex==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Sex workers")

q18_q4d <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_anticaste==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Anticaste")

q18_q4e <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_climate==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Climate")

q18_q4f <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_antigender==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Countering anti-gender & anti-rights")

q18_q4g <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_harm==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Harm reduction")

q18_q4h <- witm %>%
  filter( !is.na(q18_new_funders) & q4_awid_disability==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Disability rights")

q18_q4_total <- q18_q4a %>%
  left_join(q18_q4b, by = "Source") %>%
  left_join(q18_q4c, by = "Source") %>%
  left_join(q18_q4d, by = "Source") %>%
  left_join(q18_q4e, by = "Source") %>%
  left_join(q18_q4f, by = "Source") %>%
  left_join(q18_q4g, by = "Source") %>%
  left_join(q18_q4h, by = "Source")



q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_q4_total")
writeData(q18, sheet = "q18_q4_total", x = q18_q4_total)
saveWorkbook(q18, archivo, overwrite = TRUE)

#cruce por q5

q18_q5 <- witm %>%
  filter(!is.na(q18_new_funders)) %>%
  group_by(q5) %>% 
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE))


q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_q5")
writeData(q18, sheet = "q18_q5", x = q18_q5)
saveWorkbook(q18, archivo, overwrite = TRUE)

###

#cruce por q6


q18_q6 <- witm %>%
  filter(!is.na(q18_new_funders)) %>%
  group_by(q6) %>% 
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE))


q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_q6")
writeData(q18, sheet = "q18_q6", x = q18_q6)
saveWorkbook(q18, archivo, overwrite = TRUE)

###


#cruce por región: q7



q18_region_1 <- witm %>%
  filter( !is.na(q18_new_funders) & region_1==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "1. Latin America & the Caribbean")

q18_region_2 <- witm %>%
  filter( !is.na(q18_new_funders) & region_2==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "2. Western Europe & North America")





q18_region_3 <- witm %>%
  filter( !is.na(q18_new_funders) & region_3==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "3. Eastern, Southeast and Central Europe")

q18_region_4 <- witm %>%
  filter( !is.na(q18_new_funders) & region_4==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "4. Africa")


q18_region_5 <- witm %>%
  filter( !is.na(q18_new_funders) & region_5==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "5. Asia & the Pacific")


q18_region_6 <- witm %>%
  filter( !is.na(q18_new_funders) & region_6==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "6. Central Asia & Caucasus")

q18_region_7 <- witm %>%
  filter( !is.na(q18_new_funders) & region_7==1) %>%
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "7. South West Asia/Middle East & North Africa")



q18_region_total <- q18_region_1 %>%
  left_join(q18_region_2, by = "Source") %>%
  left_join(q18_region_3, by = "Source") %>%
  left_join(q18_region_4, by = "Source") %>%
  left_join(q18_region_5, by = "Source") %>%
  left_join(q18_region_6, by = "Source") %>%
  left_join(q18_region_7, by = "Source")



q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_region")
writeData(q18, sheet = "q18_region", x = q18_region_total)
saveWorkbook(q18, archivo, overwrite = TRUE)


###

#cruce por q9


q18_q9 <- witm %>%
  filter(!is.na(q18_new_funders)) %>%
  group_by(q9_year_formation_agrup) %>% 
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE))


q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_q9")
writeData(q18, sheet = "q18_q9", x = q18_q9)
saveWorkbook(q18, archivo, overwrite = TRUE)


####



#CRUCE POR q10

# Crear q10_2021_q18
q10_2021_q18 <- witm %>% 
  filter(q9_year_formation < 2022 & !is.na(q18_new_funders)) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%  
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q18
q10_2022_q18 <- witm %>% 
  filter(q9_year_formation < 2023 & !is.na(q18_new_funders)) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%  
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q18
q10_2023_q18 <- witm %>% 
  filter(!is.na(q10_budget_year_2023) & !is.na(q18_new_funders)) %>%
  group_by(q10_budget_grp_2023) %>% 
  summarise(
    Total = (round(n(),0)),
    Multilateral = sum(q18_new_funders.multilateral_funders == 1, na.rm = TRUE),
    Bilateral = sum(q18_new_funders.bilateral_funders == 1, na.rm = TRUE),
    Philanthropic = sum(q18_new_funders.philanthropic_foundations == 1, na.rm = TRUE),
    Womens = sum(q18_new_funders.womens_feminist_funds == 1, na.rm = TRUE),
    Private = sum(q18_new_funders.private_sector == 1, na.rm = TRUE),
    Ingos = sum(q18_new_funders.ingos == 1, na.rm = TRUE),
    Individual = sum(q18_new_funders.individual_donors == 1, na.rm = TRUE),
    Goverment = sum(q18_new_funders.national_goverment == 1, na.rm = TRUE),
    Other = sum(q18_new_funders.98 == 1, na.rm = TRUE),
    No_new_funder = sum(q18_new_funders.not_new_funders == 1, na.rm = TRUE)) %>%  
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)




# Unir los dataframes
q10_grouped_q18 <- bind_rows(q10_2021_q18, q10_2022_q18, q10_2023_q18)

# Calcular la media entre los años para cada categoría de q15, agrupando por las categorías de q10
final_means_q18 <- q10_grouped_q18 %>%
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
    Governments = round(mean(Goverment, na.rm = TRUE),0),
    Other = round(mean(Other, na.rm = TRUE),0),
    No_new_funder=round(mean(No_new_funder, na.rm = TRUE),0))




q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_q10")
# Escribir las tablas en la misma hoja
writeData(q18, "q18_q10", "Table for 2021", startRow = 1, startCol = 1)
writeData(q18, "q18_q10", q10_2021_q18, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q18, "q18_q10", "Table for 2022", startRow = nrow(q10_2021_q18) + 4, startCol = 1)
writeData(q18, "q18_q10", q10_2022_q18, startRow = nrow(q10_2021_q18) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q18, "q18_q10", "Table for 2023", startRow = nrow(q10_2021_q18) + nrow(q10_2022_q18) + 8, startCol = 1)
writeData(q18, "q18_q10", q10_2023_q18, startRow = nrow(q10_2021_q18) + nrow(q10_2022_q18) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q18, archivo, overwrite = TRUE)


q18 <- loadWorkbook(archivo)
addWorksheet(q18, sheetName = "q18_q10_media")
writeData(q18, sheet = "q18_q10_media", x = final_means_q18)
saveWorkbook(q18, archivo, overwrite = TRUE)


##############################################################################

#ANÁLISIS Q19


archivo <- "cuadros/q19_lose_funding.xlsx"

#Convertir campos vacíos de la variable q19 en NA
witm <- witm %>%
  mutate(q19_lose_funding = na_if(q19_lose_funding, ""))

q19 <-witm %>%
  filter(!is.na(q19_lose_funding)) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "N")

write.xlsx(q19, file = archivo, sheetName="q19")


###

#Cruce por q4

q19_q4agrup <-witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  filter(!is.na(q19_lose_funding)) %>% 
  group_by(q4_awid_focus) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE))



q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q4agrup")
writeData(q19, sheet = "q19_q4agrup", x = q19_q4agrup)
saveWorkbook(q19, archivo, overwrite = TRUE)


#Cruce por q4


q19_q4a <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_LGBTIQ==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "LGBTIQ")

q19_q4b <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_young==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Young")

q19_q4c <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_sex==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Sex workers")

q19_q4d <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_anticaste==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Anticaste")

q19_q4e <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_climate==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Climate")

q19_q4f <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_antigender==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Countering anti-gender & anti-rights")

q19_q4g <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_harm==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Harm reduction")

q19_q4h <- witm %>%
  filter( !is.na(q19_lose_funding) & q4_awid_disability==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Disability rights")

q19_q4_total <- q19_q4a %>%
  left_join(q19_q4b, by = "Source") %>%
  left_join(q19_q4c, by = "Source") %>%
  left_join(q19_q4d, by = "Source") %>%
  left_join(q19_q4e, by = "Source") %>%
  left_join(q19_q4f, by = "Source") %>%
  left_join(q19_q4g, by = "Source") %>%
  left_join(q19_q4h, by = "Source")



q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q4_total")
writeData(q19, sheet = "q19_q4_total", x = q19_q4_total)
saveWorkbook(q19, archivo, overwrite = TRUE)


#Cruce por la q5



q19_q5 <-witm %>%
  filter(!is.na(q19_lose_funding)) %>% 
  group_by(q5) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE))



q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q5")
writeData(q19, sheet = "q19_q5", x = q19_q5)
saveWorkbook(q19, archivo, overwrite = TRUE)

###

#Cruce por q6


q19_q6 <-witm %>% 
  filter(!is.na(q19_lose_funding)) %>% 
  group_by(q6) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE))



q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q6")
writeData(q19, sheet = "q19_q6", x = q19_q6)
saveWorkbook(q19, archivo, overwrite = TRUE)

###

#Cruce por region:q7



q19_region_1 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_1==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "1. Latin America & the Caribbean")

q19_region_2 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_2==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "2. Western Europe & North America")





q19_region_3 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_3==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "3. Eastern, Southeast and Central Europe")

q19_region_4 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_4==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "4. Africa")


q19_region_5 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_5==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "5. Asia & the Pacific")


q19_region_6 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_6==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "6. Central Asia & Caucasus")

q19_region_7 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_7==1) %>%
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "7. South West Asia/Middle East & North Africa")



q19_region_total <- q19_region_1 %>%
  left_join(q19_region_2, by = "Source") %>%
  left_join(q19_region_3, by = "Source") %>%
  left_join(q19_region_4, by = "Source") %>%
  left_join(q19_region_5, by = "Source") %>%
  left_join(q19_region_6, by = "Source") %>%
  left_join(q19_region_7, by = "Source")



q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_region")
writeData(q19, sheet = "q19_region", x = q19_region_total)
saveWorkbook(q19, archivo, overwrite = TRUE)


###

#Cruce por q9

q19_q9 <- witm %>%
  filter(!is.na(q19_lose_funding)) %>%
  group_by(q9_year_formation_agrup) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE))


q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q9")
writeData(q19, sheet = "q19_q9", x = q19_q9)
saveWorkbook(q19, archivo, overwrite = TRUE)

###

#Cruce por q10

# Crear q10_2021_q19
q10_2021_q19 <- witm %>% 
  filter(q9_year_formation < 2022 & !is.na(q19_lose_funding)) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>%   
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q19
q10_2022_q19 <- witm %>% 
  filter(q9_year_formation < 2023 & !is.na(q19_lose_funding)) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>%   
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q19
q10_2023_q19 <- witm %>% 
  filter(!is.na(q10_budget_year_2023) & !is.na(q19_lose_funding)) %>%
  group_by(q10_budget_grp_2023) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE)) %>%  
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)




# Unir los dataframes
q10_grouped_q19 <- bind_rows(q10_2021_q19, q10_2022_q19, q10_2023_q19)

# Calcular la media entre los años para cada categoría de q15, agrupando por las categorías de q10
final_means_q19 <- q10_grouped_q19 %>%
  group_by(Annual_budget) %>%  # Agrupando por la categoría de q10
  summarise(
    Total=round(mean(Total, na.rm=TRUE),0),
    No=round(mean(No, na.rm = TRUE),0),
    Multilateral = round(mean(Multilateral, na.rm = TRUE),0),
    Bilateral = round(mean(Bilateral, na.rm = TRUE),0),
    Philanthropic = round(mean(Philanthropic, na.rm = TRUE),0),
    Womens = round(mean(Womens, na.rm = TRUE),0),
    Private = round(mean(Private, na.rm = TRUE),0),
    Ingos = round(mean(Ingos, na.rm = TRUE),0),
    Individual = round(mean(Individual, na.rm = TRUE),0),
    Governments = round(mean(Goverments, na.rm = TRUE),0),
    Other = round(mean(Other, na.rm = TRUE),0))




q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q10")
# Escribir las tablas en la misma hoja
writeData(q19, "q19_q10", "Table for 2021", startRow = 1, startCol = 1)
writeData(q19, "q19_q10", q10_2021_q19, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q19, "q19_q10", "Table for 2022", startRow = nrow(q10_2021_q19) + 4, startCol = 1)
writeData(q19, "q19_q10", q10_2022_q19, startRow = nrow(q10_2021_q19) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q19, "q19_q10", "Table for 2023", startRow = nrow(q10_2021_q19) + nrow(q10_2022_q19) + 8, startCol = 1)
writeData(q19, "q19_q10", q10_2023_q19, startRow = nrow(q10_2021_q19) + nrow(q10_2022_q19) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q19, archivo, overwrite = TRUE)


q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q10_media")
writeData(q19, sheet = "q19_q10_media", x = final_means_q19)
saveWorkbook(q19, archivo, overwrite = TRUE)

#############################################################################

#ANÁLISIS Q20


archivo <- "cuadros/q20_loss_impact.xlsx"

#Convertir campos vacíos de la variable q20 en NA
witm <- witm%>%
  mutate(q20_loss_impact = na_if(q20_loss_impact, ""))

q20 <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>%
  summarise(
    Total = (round(n(),0)),
    "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
    "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
    "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
    "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
    "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
    "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "N")

write.xlsx(q20, file = archivo, sheetName="q20")


###

#Cruce por q4

q20_q4agrup <-witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>% 
  group_by(q4_awid_focus) %>% 
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE))



q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_q4agrup")
writeData(q20, sheet = "q20_q4agrup", x = q20_q4agrup)
saveWorkbook(q20, archivo, overwrite = TRUE)


#Cruce por q4


q20_q4a <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact) & q4_awid_LGBTIQ==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "LGBTIQ")

q20_q4b <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact) & q4_awid_young==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Young")

q20_q4c <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact) & q4_awid_sex==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Sex workers")

q20_q4d <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact) & q4_awid_anticaste==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Anticaste")

q20_q4e <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact) & q4_awid_climate==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Climate")

q20_q4f <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)  & q4_awid_antigender==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Countering anti-gender & anti-rights")

q20_q4g <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)  & q4_awid_harm==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Harm reduction")

q20_q4h <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)  & q4_awid_disability==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Disability rights")

q20_q4_total <- q20_q4a %>%
  left_join(q20_q4b, by = "Source") %>%
  left_join(q20_q4c, by = "Source") %>%
  left_join(q20_q4d, by = "Source") %>%
  left_join(q20_q4e, by = "Source") %>%
  left_join(q20_q4f, by = "Source") %>%
  left_join(q20_q4g, by = "Source") %>%
  left_join(q20_q4h, by = "Source")



q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_q4_total")
writeData(q20, sheet = "q20_q4_total", x = q20_q4_total)
saveWorkbook(q20, archivo, overwrite = TRUE)


#Cruce por la q5



q20_q5 <-witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>% 
  group_by(q5) %>% 
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE))



q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_q5")
writeData(q20, sheet = "q20_q5", x = q20_q5)
saveWorkbook(q20, archivo, overwrite = TRUE)

###

#Cruce por q6


q20_q6 <-witm %>% 
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>% 
  group_by(q6) %>% 
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE))



q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_q6")
writeData(q20, sheet = "q20_q6", x = q20_q6)
saveWorkbook(q20, archivo, overwrite = TRUE)

###

#Cruce por region:q7



q20_region_1 <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact) & region_1==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "1. Latin America & the Caribbean")

q20_region_2 <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)  & region_2==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "2. Western Europe & North America")





q20_region_3 <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)  & region_3==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "3. Eastern, Southeast and Central Europe")

q20_region_4 <- witm %>%
  filter( q19_lose_funding!=1 & !is.na(q20_loss_impact) & region_4==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "4. Africa")


q20_region_5 <- witm %>%
  filter( q19_lose_funding!=1 & !is.na(q20_loss_impact) & region_5==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "5. Asia & the Pacific")


q20_region_6 <- witm %>%
  filter( q19_lose_funding!=1 & !is.na(q20_loss_impact) & region_6==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "6. Central Asia & Caucasus")

q20_region_7 <- witm %>%
  filter( q19_lose_funding!=1 & !is.na(q20_loss_impact) & region_7==1) %>%
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "7. South West Asia/Middle East & North Africa")



q20_region_total <- q20_region_1 %>%
  left_join(q20_region_2, by = "Source") %>%
  left_join(q20_region_3, by = "Source") %>%
  left_join(q20_region_4, by = "Source") %>%
  left_join(q20_region_5, by = "Source") %>%
  left_join(q20_region_6, by = "Source") %>%
  left_join(q20_region_7, by = "Source")



q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_region")
writeData(q20, sheet = "q20_region", x = q20_region_total)
saveWorkbook(q20, archivo, overwrite = TRUE)


###

#Cruce por q9

q20_q9 <- witm %>%
  filter(q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>%
  group_by(q9_year_formation_agrup) %>% 
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE))


q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_q9")
writeData(q20, sheet = "q20q9", x = q20_q9)
saveWorkbook(q20, archivo, overwrite = TRUE)

###

#Cruce por q10

# Crear q10_2021_q19
q10_2021_q20 <- witm %>% 
  filter(q9_year_formation < 2022 & q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>%   
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q19
q10_2022_q20 <- witm %>% 
  filter(q9_year_formation < 2023 & q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>%   
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q19
q10_2023_q20 <- witm %>% 
  filter(!is.na(q10_budget_year_2023) & q19_lose_funding!=1 & !is.na(q20_loss_impact)) %>%
  group_by(q10_budget_grp_2023) %>% 
  summarise(Total = (round(n(),0)),
            "No big impact"=sum(q20_loss_impact.no_impact==1, na.rm=TRUE),
            "Staff reduction"=sum(q20_loss_impact.staff_fired==1, na.rm = TRUE),
            "Activity reduction"=sum(q20_loss_impact.activity_reduction==1, na.rm = TRUE),
            "Discontinuation of programs or projects"=sum(q20_loss_impact.cut_programs==1, na.rm = TRUE),
            "Staff faced a salary gap"=sum(q20_loss_impact.cut_staff_salary==1, na.rm = TRUE),
            "Other"=sum(q20_loss_impact.98==1, na.rm=TRUE)) %>%  
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)




# Unir los dataframes
q10_grouped_q20 <- bind_rows(q10_2021_q20, q10_2022_q20, q10_2023_q20)

# Calcular la media entre los años para cada categoría de q15, agrupando por las categorías de q10
final_means_q20 <- q10_grouped_q20 %>%
  group_by(Annual_budget) %>%  # Agrupando por la categoría de q10
  summarise(
    Total = round(mean(Total, na.rm = TRUE), 0),
    `No big impact` = round(mean(`No big impact`, na.rm = TRUE), 0),
    `Staff reduction` = round(mean(`Staff reduction`, na.rm = TRUE), 0),
    `Activity reduction` = round(mean(`Activity reduction`, na.rm = TRUE), 0),
    `Discontinuation of programs or projects` = round(mean(`Discontinuation of programs or projects`, na.rm = TRUE), 0),
    `Staff faced a salary gap` = round(mean(`Staff faced a salary gap`, na.rm = TRUE), 0),
    `Other` = round(mean(`Other`, na.rm = TRUE), 0)
  )



q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_q10")
# Escribir las tablas en la misma hoja
writeData(q20, "q20_q10", "Table for 2021", startRow = 1, startCol = 1)
writeData(q20, "q20_q10", q10_2021_q20, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q20, "q20_q10", "Table for 2022", startRow = nrow(q10_2021_q20) + 4, startCol = 1)
writeData(q20, "q20_q10", q10_2022_q20, startRow = nrow(q10_2021_q20) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q20, "q20_q10", "Table for 2023", startRow = nrow(q10_2021_q20) + nrow(q10_2022_q20) + 8, startCol = 1)
writeData(q20, "q20_q10", q10_2023_q20, startRow = nrow(q10_2021_q20) + nrow(q10_2022_q20) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q20, archivo, overwrite = TRUE)


q20 <- loadWorkbook(archivo)
addWorksheet(q20, sheetName = "q20_q10_media")
writeData(q20, sheet = "q20_q10_media", x = final_means_q20)
saveWorkbook(q20, archivo, overwrite = TRUE)


############################################################################
#ANÁLISIS Q21

archivo <- "cuadros/q21_types_funding.xlsx"

###2023

q21_core_2023<-witm %>% 
  mutate(q21_core_2023=case_when(
    q21_types_funding_2023_core<26 ~"1 Up to 25%",
    q21_types_funding_2023_core>25 & q21_types_funding_2023_core<51 ~ "2 Between 26%-50%",
    q21_types_funding_2023_core>50 & q21_types_funding_2023_core<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2023_core) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2023_core)) %>% 
  group_by(q21_core_2023) %>% 
  summarise(total=n()) %>% 
  rename("2023"=1, "Core"=2)

q21_project_2023<-witm %>% 
  mutate(q21_project_2023=case_when(
    q21_types_funding_2023_project<26 ~"1 Up to 25%",
    q21_types_funding_2023_project>25 & q21_types_funding_2023_project<51 ~ "2 Between 26%-50%",
    q21_types_funding_2023_project>50 & q21_types_funding_2023_project<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2023_project) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2023_project)) %>% 
  group_by(q21_project_2023) %>% 
  summarise(total=n()) %>% 
  rename("2023"=1, "Project"=2)

q21_emergency_2023<-witm %>% 
  mutate(q21_emergency_2023=case_when(
    q21_types_funding_2023_emergency<26 ~"1 Up to 25%",
    q21_types_funding_2023_emergency>25 & q21_types_funding_2023_emergency<51 ~ "2 Between 26%-50%",
    q21_types_funding_2023_emergency>50 & q21_types_funding_2023_emergency<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2023_emergency) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2023_emergency)) %>% 
  group_by(q21_emergency_2023) %>% 
  summarise(total=n()) %>% 
  rename("2023"=1, "Emergency"=2)

#Unir los dataframes
q21_2023 <- q21_core_2023 %>%
  full_join(q21_project_2023, by = "2023") %>%
  full_join(q21_emergency_2023, by = "2023")


###2022


q21_core_2022<-witm %>% 
  mutate(q21_core_2022=case_when(
    q21_types_funding_2022_core<26 ~"1 Up to 25%",
    q21_types_funding_2022_core>25 & q21_types_funding_2022_core<51 ~ "2 Between 26%-50%",
    q21_types_funding_2022_core>50 & q21_types_funding_2022_core<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2022_core) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2022_core)) %>% 
  group_by(q21_core_2022) %>% 
  summarise(total=n()) %>% 
  rename("2022"=1, "Core"=2)

q21_project_2022<-witm %>% 
  mutate(q21_project_2022=case_when(
    q21_types_funding_2022_project<26 ~"1 Up to 25%",
    q21_types_funding_2022_project>25 & q21_types_funding_2022_project<51 ~ "2 Between 26%-50%",
    q21_types_funding_2022_project>50 & q21_types_funding_2022_project<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2022_project) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2022_project)) %>% 
  group_by(q21_project_2022) %>% 
  summarise(total=n()) %>% 
  rename("2022"=1, "Project"=2)

q21_emergency_2022<-witm %>% 
  mutate(q21_emergency_2022=case_when(
    q21_types_funding_2022_emergency<26 ~"1 Up to 25%",
    q21_types_funding_2022_emergency>25 & q21_types_funding_2022_emergency<51 ~ "2 Between 26%-50%",
    q21_types_funding_2022_emergency>50 & q21_types_funding_2022_emergency<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2022_emergency) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2022_emergency)) %>% 
  group_by(q21_emergency_2022) %>% 
  summarise(total=n()) %>% 
  rename("2022"=1, "Emergency"=2)

#Unir los dataframes
q21_2022 <- q21_core_2022 %>%
  full_join(q21_project_2022, by = "2022") %>%
  full_join(q21_emergency_2022, by = "2022")

###2021

q21_core_2021<-witm %>% 
  mutate(q21_core_2021=case_when(
    q21_types_funding_2021_core<26 ~"1 Up to 25%",
    q21_types_funding_2021_core>25 & q21_types_funding_2021_core<51 ~ "2 Between 26%-50%",
    q21_types_funding_2021_core>50 & q21_types_funding_2021_core<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2021_core) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2021_core)) %>% 
  group_by(q21_core_2021) %>% 
  summarise(total=n()) %>% 
  rename("2021"=1, "Core"=2)

q21_project_2021<-witm %>% 
  mutate(q21_project_2021=case_when(
    q21_types_funding_2021_project<26 ~"1 Up to 25%",
    q21_types_funding_2021_project>25 & q21_types_funding_2021_project<51 ~ "2 Between 26%-50%",
    q21_types_funding_2021_project>50 & q21_types_funding_2021_project<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2021_project) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2021_project)) %>% 
  group_by(q21_project_2021) %>% 
  summarise(total=n()) %>% 
  rename("2021"=1, "Project"=2)

q21_emergency_2021<-witm %>% 
  mutate(q21_emergency_2021=case_when(
    q21_types_funding_2021_emergency<26 ~"1 Up to 25%",
    q21_types_funding_2021_emergency>25 & q21_types_funding_2021_emergency<51 ~ "2 Between 26%-50%",
    q21_types_funding_2021_emergency>50 & q21_types_funding_2021_emergency<76 ~ "3 Between 51%-75%",
    is.na(q21_types_funding_2021_emergency) ~ NA,
    TRUE ~ "4 Over 75%")) %>% 
  filter(!is.na(q21_types_funding_2021_emergency)) %>% 
  group_by(q21_emergency_2021) %>% 
  summarise(total=n()) %>% 
  rename("2021"=1, "Emergency"=2)

#Unir los dataframes
q21_2021 <- q21_core_2021 %>%
  full_join(q21_project_2021, by = "2021") %>%
  full_join(q21_emergency_2021, by = "2021")

#########
#AYE ESTO NO ME ANDA pero no se bien que queres hacer

# # Unir los dataframes de 2021, 2022 y 2023
# q21_total <- q21_core_2021 %>%
#   full_join(q21_core_2022, by = "Core") %>%
#   full_join(q21_core_2023, by = "Core") %>%
#   full_join(q21_project_2021, by = "Project") %>%
#   full_join(q21_project_2022, by = "Project") %>%
#   full_join(q21_project_2023, by = "Project") %>%
#   full_join(q21_emergency_2021, by = "Emergency") %>%
#   full_join(q21_emergency_2022, by = "Emergency") %>%
#   full_join(q21_emergency_2023, by = "Emergency")
# 
# # Calcular la media de los totales para cada categoría
# q21_means <- q21_total %>%
#   summarise(
#     Core_mean = mean(c(total_2021, total_2022, total_2023), na.rm = TRUE),
#     Project_mean = mean(c(total_project_2021, total_project_2022, total_project_2023), na.rm = TRUE),
#     Emergency_mean = mean(c(total_emergency_2021, total_emergency_2022, total_emergency_2023), na.rm = TRUE)
#   )





#############################################################################
#ANÁLISIS DE LA q25

archivo <- "cuadros/q25_counter_anti.xlsx"



q25<-witm %>%
  filter(!is.na(q25_counter_anti)) %>% 
  group_by(q25_counter_anti) %>% 
  summarise(n=n()) 

write.xlsx(q25, file = archivo, sheetName="q25")

###



#Cruce por q4

q25_q4agrup <- witm %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  filter(!is.na(q25_counter_anti)) %>% 
  group_by(q25_counter_anti, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Funding" = q25_counter_anti)

q25_q4agrup <- q25_q4agrup %>%
  pivot_wider(names_from = q4_awid_focus, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Funding`)


q25 <- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_q4agrup")
writeData(q25, sheet = "q25_q4agrup", x = q25_q4agrup)
saveWorkbook(q25, archivo, overwrite = TRUE)


#cruce por q4


q25_q4 <- witm %>%
  filter( !is.na(q25_counter_anti) & !is.na(q4_forms_organizing)) %>%
  group_by(q25_counter_anti ) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1))  


q25 <- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_q4")
writeData(q25, sheet = "q25_q4", x = q25_q4)
saveWorkbook(q25, archivo, overwrite = TRUE)

  
  
#CRUCE POR q5



# Crear q25_q5
q25_q5 <- witm %>% 
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

q25_q6 <- witm %>% 
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


#cruce por región: q7

q25_region<-witm %>% 
  filter(!is.na(q25_counter_anti)) %>% 
  group_by(q25_counter_anti) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))



q25 <- loadWorkbook(archivo)
addWorksheet(q25, sheetName = "q25_region")
writeData(q25, sheet = "q25_region", x = q25_region)
saveWorkbook(q25, archivo, overwrite = TRUE)

####



#####

#CRUCE POR q9

q25_q9 <- witm %>% 
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
q10_2021_q25 <- witm %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q25_counter_anti) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q25
q10_2022_q25 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q25_counter_anti) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q25
q10_2023_q25 <- witm %>% 
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

#ANÁLISIS DE LA Q28


archivo <- "cuadros/q28_autonomus_resourcing.xlsx"


#Convertir campos vacíos de la variable q28 en NA
witm <- witm %>%
  mutate(q28_autonomous_resourcing = na_if(q28_autonomous_resourcing, ""))


q28<-witm %>%
  filter(!is.na(q28_autonomous_resourcing)) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
             names_to = "Source",
             values_to = "N")

write.xlsx(q28, file = archivo, sheetName="q28")

###

#Cruce por q4

q28_q4agrup<-witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>%
  filter(!is.na(q28_autonomous_resourcing)) %>%
  group_by(q4_awid_focus) %>% 
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) 


q28<- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_q4agrup")
writeData(q28, sheet = "q28_q4agrup", x = q28_q4agrup)
saveWorkbook(q28, archivo, overwrite = TRUE)


#Cruce por q4

q19_q4agrup <-witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  filter(!is.na(q19_lose_funding)) %>% 
  group_by(q4_awid_focus) %>% 
  summarise(Total= round(n(),0),
            No=sum(q19_lose_funding.no==1, na.rm=TRUE),
            Multilateral = sum(q19_lose_funding.y_multilateral_funders == 1, na.rm = TRUE),
            Bilateral = sum(q19_lose_funding.y_bilateral_funders == 1, na.rm = TRUE),
            Philanthropic = sum(q19_lose_funding.y_philanthropic_foundations == 1, na.rm = TRUE),
            Womens = sum(q19_lose_funding.y_womens_feminist_funds == 1, na.rm = TRUE),
            Private = sum(q19_lose_funding.y_private_sector == 1, na.rm = TRUE),
            Ingos = sum(q19_lose_funding.y_ingos == 1, na.rm = TRUE),
            Individual = sum(q19_lose_funding.y_individual_donors == 1, na.rm = TRUE),
            Goverments = sum(q19_lose_funding.y_national_goverment== 1, na.rm = TRUE),
            Other = sum(q19_lose_funding.y_other == 1, na.rm = TRUE))



q19 <- loadWorkbook(archivo)
addWorksheet(q19, sheetName = "q19_q4agrup")
writeData(q19, sheet = "q19_q4agrup", x = q19_q4agrup)
saveWorkbook(q19, archivo, overwrite = TRUE)


#Cruce por q4


q28_q4a <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_LGBTIQ==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "LGBTIQ")

q28_q4b <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_young==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Young")

q28_q4c <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_sex==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Sex workers")

q28_q4d <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_anticaste==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>%   
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Anticaste")

q28_q4e <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_climate==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Climate")

q28_q4f <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_antigender==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Countering anti-gender & anti-rights")

q28_q4g <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_harm==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Harm reduction")

q28_q4h <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & q4_awid_disability==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "Disability rights")

q28_q4_total <- q28_q4a %>%
  left_join(q28_q4b, by = "Source") %>%
  left_join(q28_q4c, by = "Source") %>%
  left_join(q28_q4d, by = "Source") %>%
  left_join(q28_q4e, by = "Source") %>%
  left_join(q28_q4f, by = "Source") %>%
  left_join(q28_q4g, by = "Source") %>%
  left_join(q28_q4h, by = "Source")



q28 <- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_q4_total")
writeData(q28, sheet = "q28_q4_total", x = q28_q4_total)
saveWorkbook(q28, archivo, overwrite = TRUE)


#Cruce por q5

q28_q5<-witm %>%
  filter(!is.na(q28_autonomous_resourcing)) %>%
  group_by(q5) %>% 
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) 


q28<- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_q5")
writeData(q28, sheet = "q28_q5", x = q28_q5)
saveWorkbook(q28, archivo, overwrite = TRUE)

###

#Cruce por q6

q28_q6<-witm %>%
  filter(!is.na(q28_autonomous_resourcing)) %>%
  group_by(q6) %>% 
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) 


q28<- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_q6")
writeData(q28, sheet = "q28_q6", x = q28_q6)
saveWorkbook(q28, archivo, overwrite = TRUE)

###


#Cruce por region:q7



q28_region_1 <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & region_1==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE))  %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "1. Latin America & the Caribbean")

q28_region_2 <- witm %>%
  filter( !is.na(q19_lose_funding) & region_2==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE))  %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "2. Western Europe & North America")





q28_region_3 <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & region_3==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE))  %>%  
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "3. Eastern, Southeast and Central Europe")

q28_region_4 <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & region_4==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE))  %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "4. Africa")


q28_region_5 <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & region_5==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "5. Asia & the Pacific")


q28_region_6 <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & region_6==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE))  %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "6. Central Asia & Caucasus")

q28_region_7 <- witm %>%
  filter( !is.na(q28_autonomous_resourcing) & region_7==1) %>%
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE))  %>% 
  pivot_longer(cols = everything(),
               names_to = "Source",
               values_to = "7. South West Asia/Middle East & North Africa")



q28_region_total <- q28_region_1 %>%
  left_join(q28_region_2, by = "Source") %>%
  left_join(q28_region_3, by = "Source") %>%
  left_join(q28_region_4, by = "Source") %>%
  left_join(q28_region_5, by = "Source") %>%
  left_join(q28_region_6, by = "Source") %>%
  left_join(q28_region_7, by = "Source")



q28 <- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_region")
writeData(q28, sheet = "q28_region", x = q28_region_total)
saveWorkbook(q28, archivo, overwrite = TRUE)

####



#Cruce por q9

q28_q9<-witm %>% 
  filter(!is.na(q28_autonomous_resourcing)) %>%
  group_by(q9_year_formation_agrup) %>% 
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) 


q28<- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_q9")
writeData(q28, sheet = "q28_q9", x = q28_q9)
saveWorkbook(q28, archivo, overwrite = TRUE)


###


#Cruce por q10

# Crear q10_2021_q28
q10_2021_q28 <- witm %>% 
  filter(q9_year_formation < 2022 & !is.na(q28_autonomous_resourcing)) %>% 
  group_by(q10_budget_grp_2021) %>% 
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q28
q10_2022_q28 <- witm %>% 
  filter(q9_year_formation < 2023 & !is.na(q28_autonomous_resourcing)) %>% 
  group_by(q10_budget_grp_2022) %>% 
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>%   
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q28
q10_2023_q28 <- witm %>% 
  filter(!is.na(q10_budget_year_2023) & !is.na(q28_autonomous_resourcing)) %>%
  group_by(q10_budget_grp_2023) %>% 
  summarise(Total= round(n(),0),
            None = sum(q28_autonomous_resourcing.none == 1, na.rm = TRUE),
            Volunteering = sum(q28_autonomous_resourcing.volunteering == 1, na.rm = TRUE),
            Membership_fees = sum(q28_autonomous_resourcing.membership_fees == 1, na.rm = TRUE),
            Product_sales = sum(q28_autonomous_resourcing.product_sales == 1, na.rm = TRUE),
            Service_sales = sum(q28_autonomous_resourcing.service_sales == 1, na.rm = TRUE),
            Property_rent = sum(q28_autonomous_resourcing.property_rent_or_lease == 1, na.rm = TRUE),
            Donations = sum(q28_autonomous_resourcing.donations == 1, na.rm = TRUE),
            crowdfunding = sum(q28_autonomous_resourcing.crowdfunding== 1, na.rm = TRUE),
            In_kind = sum(q28_autonomous_resourcing.in_kind_contributions== 1, na.rm = TRUE),
            Mutual_aid = sum(q28_autonomous_resourcing.mutual_aid== 1, na.rm = TRUE),
            Other = sum(q28_autonomous_resourcing.98== 1, na.rm = TRUE)) %>%  
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)




# Unir los dataframes
q10_grouped_q28 <- bind_rows(q10_2021_q28, q10_2022_q28, q10_2023_q28)

# Calcular la media entre los años para cada categoría de q15, agrupando por las categorías de q10
final_means_q28 <- q10_grouped_q28 %>%
  group_by(Annual_budget) %>%  # Agrupando por la categoría de q10
  summarise(Total= round(mean(Total, na.rm=TRUE),0),
          None = round(mean(None, na.rm=TRUE),0),
          Volunteering = round(mean(Volunteering, na.rm=TRUE),0),
          Membership_fees = round(mean(Membership_fees, na.rm=TRUE),0),
          Product_sales = round(mean(Product_sales, na.rm=TRUE),0),
          Service_sales = round(mean(Service_sales, na.rm=TRUE),0),
          Property_rent = round(mean(Property_rent, na.rm=TRUE),0),
          Donations = round(mean(Donations, na.rm=TRUE),0),
          Crowdfunding = round(mean(crowdfunding, na.rm=TRUE),0),
          In_kind = round(mean(In_kind, na.rm=TRUE),0),
          Mutual_aid = round(mean(Mutual_aid, na.rm=TRUE),0),
          Other = round(mean(Other, na.rm=TRUE),0))



q28 <- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_q10")
# Escribir las tablas en la misma hoja
writeData(q28, "q28_q10", "Table for 2021", startRow = 1, startCol = 1)
writeData(q28, "q28_q10", q10_2021_q28, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q28, "q28_q10", "Table for 2022", startRow = nrow(q10_2021_q28) + 4, startCol = 1)
writeData(q28, "q28_q10", q10_2022_q28, startRow = nrow(q10_2021_q28) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q28, "q28_q10", "Table for 2023", startRow = nrow(q10_2021_q28) + nrow(q10_2022_q28) + 8, startCol = 1)
writeData(q28, "q28_q10", q10_2023_q28, startRow = nrow(q10_2021_q28) + nrow(q10_2022_q28) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q28, archivo, overwrite = TRUE)


q28 <- loadWorkbook(archivo)
addWorksheet(q28, sheetName = "q28_q10_media")
writeData(q28, sheet = "q28_q10_media", x = final_means_q28)
saveWorkbook(q28, archivo, overwrite = TRUE)


################################################################################

#ANÁLISIS DE LA q29

archivo <- "cuadros/q29_autonomous_resources.xlsx"


#Convertir campos vacíos de la variable q13 en NA
base <- base %>%
  mutate(q29_reliance_aut_resoursing = na_if(q29_reliance_aut_resoursing, ""))


q29<-base %>%
  filter(!is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  mutate(q29_recod= case_when(
    q29_reliance_aut_resoursing=="grown_substantively" ~ "(a) Grew substantively",
    q29_reliance_aut_resoursing=="grown_slightly" ~ "(b) Grew slightly",
    q29_reliance_aut_resoursing=="stayed_same" ~ "(c) Stayed the same",
    q29_reliance_aut_resoursing=="decreased_slightly" ~ "(d) Decreased slightly",
    q29_reliance_aut_resoursing=="decreased_substantially" ~ "(e) Decreased substantially",
    TRUE ~ NA)) %>% 
  group_by(q29_recod) %>% 
  summarise(Count=n()) %>% 
  mutate(Percentage=round((Count/sum(Count))*100,1)) %>% 
  arrange(q29_recod) %>%   
  rename("Change"=q29_recod)



write.xlsx(q29, file = archivo, sheetName="q29")


###


# Cuce por q4
q29_q4agrup <- witm %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>%
  filter(!is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  group_by(q29_reliance_aut_resoursing, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Change" = q29_reliance_aut_resoursing)

q29_q4agrup <- q29_q4agrup %>%
  pivot_wider(names_from = q4_awid_focus, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Change`)


q29 <- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_q4agrup") 
writeData(q29, sheet = "q29_q4agrup", x = q29_q4agrup)
saveWorkbook(q29, archivo, overwrite = TRUE)



#cruce por q4


q29_q4 <- witm %>%
  filter( !is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable" & !is.na(q4_forms_organizing)) %>%
  group_by(q29_reliance_aut_resoursing) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1))  


q29 <- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_q4")
writeData(q29, sheet = "q29_q4", x = q29_q4)
saveWorkbook(q29, archivo, overwrite = TRUE)



#CRUCE POR q5


q29_q5 <- witm %>% 
  filter(!is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  group_by(q29_reliance_aut_resoursing, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Change" = q29_reliance_aut_resoursing)

q29_q5 <- q29_q5 %>%
  pivot_wider(names_from = q5, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Change`)


q29 <- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_q5")
writeData(q29, sheet = "q29_q5", x = q29_q5)
saveWorkbook(q29, archivo, overwrite = TRUE)



#######

#CRUCE POR q6

q29_q6 <- witm %>% 
  filter(!is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  group_by(q29_reliance_aut_resoursing, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Change" = q29_reliance_aut_resoursing)

q29_q6 <- q29_q6 %>%
  pivot_wider(names_from = q6, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Change`)


q29 <- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_q6")
writeData(q29, sheet = "q29_q6", x = q29_q6)
saveWorkbook(q29, archivo, overwrite = TRUE)
###


#cruce por región: q7

q29_region<-witm %>% 
  filter(!is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  group_by(q29_reliance_aut_resoursing) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))



q29 <- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_region")
writeData(q29, sheet = "q29_region", x = q29_region)
saveWorkbook(q29, archivo, overwrite = TRUE)

#####

#CRUCE POR q9

q29_q9 <- witm %>% 
  filter(!is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  group_by(q29_reliance_aut_resoursing, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Change" = q29_reliance_aut_resoursing)

q29_q9 <- q29_q9 %>%
  pivot_wider(names_from = q9_year_formation_agrup, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Change`)

q29 <- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_q9")
writeData(q29, sheet = "q29_q9", x = q29_q9)
saveWorkbook(q29, archivo, overwrite = TRUE)



###

#CRUCE POR q10

# Crear q10_2021_q29
q10_2021_q29 <- witm %>% 
  filter(q9_year_formation < 2022 & !is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  group_by(q10_budget_grp_2021, q29_reliance_aut_resoursing) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q30
q10_2022_q29 <- witm %>% 
  filter(q9_year_formation < 2023 & !is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>% 
  group_by(q10_budget_grp_2022, q29_reliance_aut_resoursing) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q30
q10_2023_q29 <- witm %>% 
  filter(!is.na(q10_budget_year_2023) & !is.na(q29_reliance_aut_resoursing) & q29_reliance_aut_resoursing!="not_applicable") %>%
  group_by(q10_budget_grp_2023, q29_reliance_aut_resoursing) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q29 <- bind_rows(q10_2021_q29, q10_2022_q29, q10_2023_q29)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q29 %>%
  group_by(Year, Annual_budget, q29_reliance_aut_resoursing) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q29_reliance_aut_resoursing, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q29 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q29_reliance_aut_resoursing, Media) %>%
  pivot_wider(names_from = q29_reliance_aut_resoursing, values_from = Media, values_fill = list(Media = 0))

q29 <- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_q10")
writeData(q29, sheet = "q29_q10", x = q10_table)
saveWorkbook(q29, archivo, overwrite = TRUE)

q29<- loadWorkbook(archivo)
addWorksheet(q29, sheetName = "q29_q10_media")
writeData(q29, sheet = "q29_q10_media", x = q10_media_table)
saveWorkbook(q29, archivo, overwrite = TRUE)





################################################################################

#ANÁLISIS DE LA q30

archivo <- "cuadros/q30_shifting_power.xlsx"


q30<-witm %>%
  filter(!is.na(q30_shift_priorities)) %>% 
  group_by(q30_shift_priorities) %>% 
  summarise(n=n()) 

write.xlsx(q30, file = archivo, sheetName="q30")

###


# Cuce por q4
q30_q4agrup <- witm %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>%
  filter(!is.na(q30_shift_priorities)) %>% 
  group_by(q30_shift_priorities, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("Shift priorities" = q30_shift_priorities)

q30_q4agrup <- q30_q4agrup %>%
  pivot_wider(names_from = q4_awid_focus, values_from = n, values_fill = list(n = 0)) %>%
  arrange(`Shift priorities`)


q30 <- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_q4agrup") 
writeData(q30, sheet = "q30_q4agrup", x = q30_q4agrup)
saveWorkbook(q30, archivo, overwrite = TRUE)



#cruce por q4


q30_q4 <- witm %>%
  filter( !is.na(q30_shift_priorities) & !is.na(q4_forms_organizing)) %>%
  group_by(q30_shift_priorities) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1))  


q30 <- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_q4")
writeData(q30, sheet = "q30_q4", x = q30_q4)
saveWorkbook(q30, archivo, overwrite = TRUE)



#CRUCE POR q5


q30_q5 <- witm %>% 
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

q30_q6 <- witm %>% 
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
###


#cruce por región: q7

q30_region<-witm %>% 
  filter(!is.na(q30_shift_priorities)) %>% 
  group_by(q30_shift_priorities) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))
  


q30 <- loadWorkbook(archivo)
addWorksheet(q30, sheetName = "q30_region")
writeData(q30, sheet = "q30_region", x = q30_region)
saveWorkbook(q30, archivo, overwrite = TRUE)
  
#####

#CRUCE POR q9

q30_q9 <- witm %>% 
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
q10_2021_q30 <- witm %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q30_shift_priorities) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q30
q10_2022_q30 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q30_shift_priorities) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q30
q10_2023_q30 <- witm %>% 
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

##############################################################################






#ANÁLISIS DE LA q36

archivo <- "cuadros/q36_ustainability.xlsx"


q36_2025 <- base %>%
  mutate(q36_2025=case_when(
    q36_budget_security_2025=="0" ~ "0 Zero",
    q36_budget_security_2025=="10" | q36_budget_security_2025=="20"  ~ "1 Lower than 30%",
    q36_budget_security_2025=="30" | q36_budget_security_2025=="40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025=="50" | q36_budget_security_2025=="60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025=="70" | q36_budget_security_2025=="80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025=="90" | q36_budget_security_2025=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q36_2025) %>% 
  summarise("2025" = n()) %>%
  select("q36"=1, 2)   # Elimina la columna total_count si no es necesaria

q36_2026 <- base %>%
  mutate(q36_2026=case_when(
    q36_budget_security_2026=="0" ~ "0 Zero",
    q36_budget_security_2026=="10" | q36_budget_security_2026=="20"  ~ "1 Lower than 30%",
    q36_budget_security_2026=="30" | q36_budget_security_2026=="40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026=="50" | q36_budget_security_2026=="60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026=="70" | q36_budget_security_2026=="80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026=="90" | q36_budget_security_2026=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q36_2026) %>% 
  summarise("2026" = n()) %>%
  select("q36"=1, 2)   # Elimina la columna total_count si no es necesaria


#Unir los dataframes
q36_combined <- q36_2025 %>%
  full_join(q36_2026, by = "q36")

write.xlsx(q36_combined, file = archivo, sheetName="q36")

###


#CRUCE POR q4agrup

q36_2026_q4agrup <- witm %>%
  mutate(q36_2026=case_when(
    q36_budget_security_2026=="0" ~ "0 Zero",
    q36_budget_security_2026=="10" | q36_budget_security_2026=="20"  ~ "1 Lower than 30%",
    q36_budget_security_2026=="30" | q36_budget_security_2026=="40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026=="50" | q36_budget_security_2026=="60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026=="70" | q36_budget_security_2026=="80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026=="90" | q36_budget_security_2026=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  group_by(q36_2026, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36"=1) %>% 
  mutate(Year = 2026)

q36_2025_q4agrup <- witm %>%
  mutate(q36_2025=case_when(
    q36_budget_security_2025=="0" ~ "0 Zero",
    q36_budget_security_2025=="10" | q36_budget_security_2025=="20"  ~ "1 Lower than 30%",
    q36_budget_security_2025=="30" | q36_budget_security_2025=="40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025=="50" | q36_budget_security_2025=="60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025=="70" | q36_budget_security_2025=="80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025=="90" | q36_budget_security_2025=="100"~ "5 Higher than 80%",
    TRUE ~ NA))  %>% 
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  group_by(q36_2025, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36"=1) %>% 
  mutate(Year = 2025)

# Unir los dataframes
q36_q4agrup <- bind_rows(q36_2025_q4agrup, q36_2026_q4agrup)



# Crear la tabla de doble entrada
q36_table <- q36_q4agrup %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q36)  # Opcional: ordenar por la columna de "q14"

# Crear la tabla de doble entrada con q14 y q5
q36_table <- q36_q4agrup %>%
  group_by(Year, q36, q4_awid_focus) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q36, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q4_awid_focus)  # Opcional: ordenar por el año y q5

q36 <- loadWorkbook(archivo)
addWorksheet(q36, sheetName = "q36_q4agrup")
writeData(q36, sheet = "q36_q4agrup", x = q36_table)
saveWorkbook(q36, archivo, overwrite = TRUE)

####


#cruce por q4

q36_2025_q4 <- witm %>%
  mutate(q36_2025=case_when(
    q36_budget_security_2025=="0" ~ "0 Zero",
    q36_budget_security_2025=="10" | q36_budget_security_2025=="20"  ~ "1 Lower than 30%",
    q36_budget_security_2025=="30" | q36_budget_security_2025=="40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025=="50" | q36_budget_security_2025=="60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025=="70" | q36_budget_security_2025=="80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025=="90" | q36_budget_security_2025=="100"~ "5 Higher than 80%",
    TRUE ~ NA))  %>%
  group_by(q36_2025) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>% 
  rename("q36"=1) %>% 
  mutate(Year=2025)


q36_2026_q4 <- witm %>%
  mutate(q36_2026=case_when(
    q36_budget_security_2026=="0" ~ "0 Zero",
    q36_budget_security_2026=="10" | q36_budget_security_2026=="20"  ~ "1 Lower than 30%",
    q36_budget_security_2026=="30" | q36_budget_security_2026=="40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026=="50" | q36_budget_security_2026=="60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026=="70" | q36_budget_security_2026=="80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026=="90" | q36_budget_security_2026=="100"~ "5 Higher than 80%",
    TRUE ~ NA)) %>% 
  group_by(q36_2026) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1)) %>% 
  rename("q36"=1) %>% 
  mutate(Year=2026)



q36_unificada<-bind_rows(q36_2025_q4, q36_2026_q4)


q36 <- loadWorkbook(archivo)
addWorksheet(q36, sheetName = "q36_q4")
writeData(q36, sheet = "q36_q4", x = q36_unificada)
saveWorkbook(q36, archivo, overwrite = TRUE)


###

#CRUCE POR q5


q36_2025_q5 <- witm %>% 
  filter(!is.na(q5)) %>% 
  mutate(q36_2025 = case_when(
    q36_budget_security_2025 == "0" ~ "0 Zero",
    q36_budget_security_2025 == "10" | q36_budget_security_2025 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2025 == "30" | q36_budget_security_2025 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025 == "50" | q36_budget_security_2025 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025 == "70" | q36_budget_security_2025 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025 == "90" | q36_budget_security_2025 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia de tipo
  )) %>% 
  group_by(q36_2025, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>%  # Mantener .groups = 'drop'
  rename("q36" = 1) %>%  # Renombrar la primera columna como q36
  mutate(Year = 2025)  # Añadir columna Year


q36_2026_q5 <- witm %>%
  filter(!is.na(q5)) %>% 
  mutate(q36_2026 = case_when(
    q36_budget_security_2026 == "0" ~ "0 Zero",
    q36_budget_security_2026 == "10" | q36_budget_security_2026 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2026 == "30" | q36_budget_security_2026 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026 == "50" | q36_budget_security_2026 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026 == "70" | q36_budget_security_2026 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026 == "90" | q36_budget_security_2026 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia
  )) %>% 
  group_by(q36_2026, q5) %>% 
  summarise(n = n(), .groups = 'drop') %>%  # Usar .groups = 'drop' para eliminar el agrupamiento residual
  rename("q36" = 1) %>%  # Renombrar la primera columna a "q36"
  mutate(Year = 2026)  # Añadir columna con el año 2026



# Unir los dataframes
q36_grouped_q5 <- bind_rows(q36_2025_q5, q36_2026_q5)



# Crear la tabla de doble entrada
q36_table <- q36_grouped_q5 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q36)  # Opcional: ordenar por la columna de "q14"

# Crear la tabla de doble entrada con q14 y q5
q36_table <- q36_grouped_q5 %>%
  group_by(Year, q36, q5) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q36, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q5)  # Opcional: ordenar por el año y q5

q36 <- loadWorkbook(archivo)
addWorksheet(q36, sheetName = "q36_q5")
writeData(q36, sheet = "q36_q5", x = q36_table)
saveWorkbook(q36, archivo, overwrite = TRUE)

###

#CRUCE POR q6


q36_2025_q6 <- witm %>%
  filter(!is.na(q6)) %>%
  mutate(q36_2025 = case_when(
    q36_budget_security_2025 == "0" ~ "0 Zero",
    q36_budget_security_2025 == "10" | q36_budget_security_2025 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2025 == "30" | q36_budget_security_2025 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025 == "50" | q36_budget_security_2025 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025 == "70" | q36_budget_security_2025 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025 == "90" | q36_budget_security_2025 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia de tipo
  )) %>% 
  group_by(q36_2025, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36"=1) %>% 
  mutate(Year = 2025)

q36_2026_q6 <- witm %>%
  filter(!is.na(q6)) %>% 
  mutate(q36_2026 = case_when(
    q36_budget_security_2026 == "0" ~ "0 Zero",
    q36_budget_security_2026 == "10" | q36_budget_security_2026 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2026 == "30" | q36_budget_security_2026 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026 == "50" | q36_budget_security_2026 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026 == "70" | q36_budget_security_2026 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026 == "90" | q36_budget_security_2026 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia
  )) %>% 
  group_by(q36_2026, q6) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36"=1) %>% 
  mutate(Year = 2026)


# Unir los dataframes
q36_grouped_q6 <- bind_rows(q36_2025_q6, q36_2026_q6)


# Crear la tabla de doble entrada con q14 y q5
q36_table <- q36_grouped_q6 %>%
  group_by(Year, q36, q6) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q36, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q6)  # Opcional: ordenar por el año y q5

q36 <- loadWorkbook(archivo)
addWorksheet(q36, sheetName = "q36_q6")
writeData(q36, sheet = "q36_q6", x = q36_table)
saveWorkbook(q36, archivo, overwrite = TRUE)

######

#cruce por región

q36_2025_region <- witm %>%
  mutate(q36_2025 = case_when(
    q36_budget_security_2025 == "0" ~ "0 Zero",
    q36_budget_security_2025 == "10" | q36_budget_security_2025 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2025 == "30" | q36_budget_security_2025 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025 == "50" | q36_budget_security_2025 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025 == "70" | q36_budget_security_2025 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025 == "90" | q36_budget_security_2025 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia de tipo
  )) %>% 
  group_by(q36_2025) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1)) %>% 
  rename("q36"=1) %>% 
  mutate(Year=2025)


q36_2026_region <- witm %>%
  mutate(q36_2026 = case_when(
    q36_budget_security_2026 == "0" ~ "0 Zero",
    q36_budget_security_2026 == "10" | q36_budget_security_2026 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2026 == "30" | q36_budget_security_2026 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026 == "50" | q36_budget_security_2026 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026 == "70" | q36_budget_security_2026 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026 == "90" | q36_budget_security_2026 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia
  )) %>%
  group_by(q36_2026) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1)) %>% 
  rename("q36"=1) %>% 
  mutate(Year=2026)



q36_region_unificada<-bind_rows(q36_2025_region, q36_2026_region)



q36 <- loadWorkbook(archivo)
addWorksheet(q36, sheetName = "q36_region")
writeData(q36, sheet = "q36_region", x = q36_region_unificada)
saveWorkbook(q36, archivo, overwrite = TRUE)


######

#CRUCE POR q9

q36_2025_q9 <- witm %>%
  mutate(q36_2025 = case_when(
    q36_budget_security_2025 == "0" ~ "0 Zero",
    q36_budget_security_2025 == "10" | q36_budget_security_2025 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2025 == "30" | q36_budget_security_2025 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025 == "50" | q36_budget_security_2025 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025 == "70" | q36_budget_security_2025 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025 == "90" | q36_budget_security_2025 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia de tipo
  )) %>% 
  group_by(q36_2025, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36"=1) %>% 
  mutate(Year = 2025)

q36_2026_q9 <- witm %>%
  mutate(q36_2026 = case_when(
    q36_budget_security_2026 == "0" ~ "0 Zero",
    q36_budget_security_2026 == "10" | q36_budget_security_2026 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2026 == "30" | q36_budget_security_2026 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026 == "50" | q36_budget_security_2026 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026 == "70" | q36_budget_security_2026 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026 == "90" | q36_budget_security_2026 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Usar NA_character_ para asegurar consistencia
  )) %>%
  group_by(q36_2026, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36"=1) %>% 
  mutate(Year = 2026)


# Unir los dataframes
q36_grouped_q9 <- bind_rows(q36_2025_q9, q36_2026_q9)


# Crear la tabla de doble entrada con q14 y q5
q36_table <- q36_grouped_q9 %>%
  group_by(Year, q36, q9_year_formation_agrup) %>%
  summarise(n = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q36, values_from = n, values_fill = list(n = 0)) %>%
  arrange(Year, q9_year_formation_agrup)  # Opcional: ordenar por el año y q5

q36 <- loadWorkbook(archivo)
addWorksheet(q36, sheetName = "q36_q9")
writeData(q36, sheet = "q36_q9", x = q36_table)
saveWorkbook(q36, archivo, overwrite = TRUE)

### 
# CRUCE POR q10

# Creación de la tabla para 2025
q36_2021_q10a <- witm %>%
  filter(q9_year_formation < 2022) %>%
  mutate(q36_2025 = case_when(
    q36_budget_security_2025 == "0" ~ "0 Zero",
    q36_budget_security_2025 == "10" | q36_budget_security_2025 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2025 == "30" | q36_budget_security_2025 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025 == "50" | q36_budget_security_2025 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025 == "70" | q36_budget_security_2025 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025 == "90" | q36_budget_security_2025 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Asegurar consistencia en el tipo
  )) %>%
  group_by(q36_2025, q10_budget_grp_2021) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36" = q36_2025) %>% 
  mutate(Year="2025")

# Creación de la tabla para 2026
q36_2021_q10b <- witm %>%
  filter(q9_year_formation < 2022) %>%
  mutate(q36_2026 = case_when(
    q36_budget_security_2026 == "0" ~ "0 Zero",
    q36_budget_security_2026 == "10" | q36_budget_security_2026 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2026 == "30" | q36_budget_security_2026 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026 == "50" | q36_budget_security_2026 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026 == "70" | q36_budget_security_2026 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026 == "90" | q36_budget_security_2026 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Asegurar consistencia en el tipo
  )) %>%
  group_by(q36_2026, q10_budget_grp_2021) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36" = q36_2026)%>% 
  mutate(Year="2026")

# Unir las dos tablas
q36_grouped_q10_2021 <- bind_rows(q36_2021_q10a, q36_2021_q10b)

q36_grouped_q10_2021_wide <- q36_grouped_q10_2021 %>%
  pivot_wider(
    names_from = q10_budget_grp_2021,  # Crear columnas a partir de los valores de q10_budget_grp_2021
    values_from = n,                   # Los valores serán los conteos de la columna n
    values_fill = 0                    # Rellenar con 0 en caso de valores faltantes
  )


#2022


# Creación de la tabla para 2025
q36_2022_q10a <- witm %>%
  filter(q9_year_formation < 2023) %>%
  mutate(q36_2025 = case_when(
    q36_budget_security_2025 == "0" ~ "0 Zero",
    q36_budget_security_2025 == "10" | q36_budget_security_2025 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2025 == "30" | q36_budget_security_2025 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025 == "50" | q36_budget_security_2025 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025 == "70" | q36_budget_security_2025 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025 == "90" | q36_budget_security_2025 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Asegurar consistencia en el tipo
  )) %>%
  group_by(q36_2025, q10_budget_grp_2022) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36" = q36_2025)%>% 
  mutate(Year="2025")

# Creación de la tabla para 2026
q36_2022_q10b <- witm %>%
  filter(q9_year_formation < 2023) %>%
  mutate(q36_2026 = case_when(
    q36_budget_security_2026 == "0" ~ "0 Zero",
    q36_budget_security_2026 == "10" | q36_budget_security_2026 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2026 == "30" | q36_budget_security_2026 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026 == "50" | q36_budget_security_2026 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026 == "70" | q36_budget_security_2026 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026 == "90" | q36_budget_security_2026 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Asegurar consistencia en el tipo
  )) %>%
  group_by(q36_2026, q10_budget_grp_2022) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36" = q36_2026)%>% 
  mutate(Year="2026")

# Unir las dos tablas
q36_grouped_q10_2022 <- bind_rows(q36_2022_q10a, q36_2022_q10b)

q36_grouped_q10_2022_wide <- q36_grouped_q10_2022 %>%
  pivot_wider(
    names_from = q10_budget_grp_2022,  # Crear columnas a partir de los valores de q10_budget_grp_2021
    values_from = n,                   # Los valores serán los conteos de la columna n
    values_fill = 0                    # Rellenar con 0 en caso de valores faltantes
  )


#2023

# Creación de la tabla para 2025
q36_2023_q10a <- witm %>%
  mutate(q36_2025 = case_when(
    q36_budget_security_2025 == "0" ~ "0 Zero",
    q36_budget_security_2025 == "10" | q36_budget_security_2025 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2025 == "30" | q36_budget_security_2025 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2025 == "50" | q36_budget_security_2025 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2025 == "70" | q36_budget_security_2025 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2025 == "90" | q36_budget_security_2025 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Asegurar consistencia en el tipo
  )) %>%
  group_by(q36_2025, q10_budget_grp_2023) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36" = q36_2025)%>% 
  mutate(Year="2025")

# Creación de la tabla para 2026
q36_2023_q10b <- witm %>%
  mutate(q36_2026 = case_when(
    q36_budget_security_2026 == "0" ~ "0 Zero",
    q36_budget_security_2026 == "10" | q36_budget_security_2026 == "20"  ~ "1 Lower than 30%",
    q36_budget_security_2026 == "30" | q36_budget_security_2026 == "40" ~ "2 Between 30% and 40%",
    q36_budget_security_2026 == "50" | q36_budget_security_2026 == "60" ~ "3 Between 50% and 60%",
    q36_budget_security_2026 == "70" | q36_budget_security_2026 == "80" ~ "4 Between 70% and 80%",
    q36_budget_security_2026 == "90" | q36_budget_security_2026 == "100" ~ "5 Higher than 80%",
    TRUE ~ NA_character_  # Asegurar consistencia en el tipo
  )) %>%
  group_by(q36_2026, q10_budget_grp_2023) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename("q36" = q36_2026)%>% 
  mutate(Year="2026")

# Unir las dos tablas
q36_grouped_q10_2023 <- bind_rows(q36_2023_q10a, q36_2023_q10b)

q36_grouped_q10_2023_wide <- q36_grouped_q10_2023 %>%
  pivot_wider(
    names_from = q10_budget_grp_2023,  # Crear columnas a partir de los valores de q10_budget_grp_2021
    values_from = n,                   # Los valores serán los conteos de la columna n
    values_fill = 0                    # Rellenar con 0 en caso de valores faltantes
  )



q36 <- loadWorkbook(archivo)
addWorksheet(q36, sheetName = "q36_q10")
# Escribir las tablas en la misma hoja
writeData(q36, "q36_q10", "Table for 2021", startRow = 1, startCol = 1)
writeData(q36, "q36_q10", q36_grouped_q10_2021_wide, startRow = 2, startCol = 1, withFilter = TRUE)
# Agregar un espacio entre tablas
writeData(q36, "q36_q10", "Table for 2022", startRow = nrow(q36_grouped_q10_2021_wide) + 4, startCol = 1)
writeData(q36, "q36_q10", q36_grouped_q10_2022_wide, startRow = nrow(q36_grouped_q10_2021_wide) + 5, startCol = 1, withFilter = TRUE)
# Agregar otro espacio
writeData(q36, "q36_q10", "Table for 2023", startRow = nrow(q36_grouped_q10_2021_wide) + nrow(q36_grouped_q10_2022_wide) + 8, startCol = 1)
writeData(q36, "q36_q10", q36_grouped_q10_2023_wide, startRow = nrow(q36_grouped_q10_2022_wide) + nrow(q36_grouped_q10_2022_wide) + 9, startCol = 1, withFilter = TRUE)
saveWorkbook(q36, archivo, overwrite = TRUE)


##########################################################################


# ANÁLISIS DE LA q45

archivo <- "cuadros/q45_responses.xlsx"



q45<-witm %>%
  filter(!is.na(q45_previous_response)) %>% 
  group_by(q45_previous_response) %>% 
  summarise(n=n()) 

write.xlsx(q45, file = archivo, sheetName="q45")

#####

#cruce por q4agrup


# Crear q13_q4agrup
q45_q4agrup <- witm %>%
  mutate(q4_awid_focus=recode(q4_awid_focus,"1"="Specific AWID subjects","0"="Other subjects")) %>% 
  filter(!is.na(q45_previous_response)) %>% 
  group_by(q45_previous_response, q4_awid_focus) %>% 
  summarise(n = n(), .groups = 'drop')

q45_q4agrup <- q45_q4agrup %>%
  pivot_wider(names_from = q4_awid_focus, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q45_previous_response)


q45 <- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_q4agrup")
writeData(q45, sheet = "q45_q4agrup", x = q45_q4agrup)
saveWorkbook(q45, archivo, overwrite = TRUE)


#cruce por q4


q45_q4 <- witm %>%
  filter( !is.na(q45_previous_response) & !is.na(q4_forms_organizing)) %>%
  group_by(q45_previous_response ) %>% 
  summarise(Total = n(),
            LGTBIQ= sum(q4_awid_LGBTIQ==1),
            Young= sum(q4_awid_young==1),
            Sex_workers=sum(q4_awid_sex==1),
            Anti_caste=sum(q4_awid_anticaste==1),
            Climate=sum(q4_awid_climate==1),
            Countering_anti=sum(q4_awid_antigender==1),
            Harm_reduction=sum(q4_awid_harm==1),
            Disability_rights=sum(q4_awid_disability==1))  


q45 <- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_q4")
writeData(q45, sheet = "q45_q4", x = q45_q4)
saveWorkbook(q45, archivo, overwrite = TRUE)



## CRUCE POR Q5



# Crear q45_q5
q45_q5 <- witm %>% 
  filter(!is.na(q45_previous_response)) %>% 
  group_by(q45_previous_response, q5) %>% 
  summarise(n = n(), .groups = 'drop') 

q45_q5 <- q45_q5 %>%
  pivot_wider(names_from = q5, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q45_previous_response)


q45 <- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_q5")
writeData(q45, sheet = "q45_q5", x = q45_q5)
saveWorkbook(q45, archivo, overwrite = TRUE)




#######

#CRUCE POR Q6

q45_q6 <- witm %>% 
  filter(!is.na(q45_previous_response)) %>% 
  group_by(q45_previous_response, q6) %>% 
  summarise(n = n(), .groups = 'drop')

q45_q6 <- q45_q6 %>%
  pivot_wider(names_from = q6, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q45_previous_response)

q45 <- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_q6")
writeData(q45, sheet = "q45_q5", x = q45_q6)
saveWorkbook(q45, archivo, overwrite = TRUE)


#####


#cruce por región: q7

q45_region<-witm %>% 
  filter(!is.na(q45_previous_response)) %>% 
  group_by(q45_previous_response) %>% 
  summarise(Total= n(),
            "1. Latin America & the Caribbean"=sum(region_1==1),
            "2. Western Europe & North America"=sum(region_2==1),
            "3. Eastern, Southeast and Central Europe"=sum(region_3==1),
            "4. Africa"= sum(region_4==1),
            "5. Asia & the Pacific"=sum(region_5==1),
            "6. Central Asia & Caucasus"=sum(region_6==1),
            "7. South West Asia/Middle East & North Africa"=sum(region_7==1))



q45 <- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_region")
writeData(q45, sheet = "q45_region", x = q45_region)
saveWorkbook(q45, archivo, overwrite = TRUE)



#####

#CRUCE POR q9

q45_q9 <- witm %>% 
  filter(!is.na(q45_previous_response)) %>% 
  group_by(q45_previous_response, q9_year_formation_agrup) %>% 
  summarise(n = n(), .groups = 'drop')

q45_q9 <- q45_q9 %>%
  pivot_wider(names_from = q9_year_formation_agrup, values_from = n, values_fill = list(n = 0)) %>%
  arrange(q45_previous_response)


q45 <- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_q9")
writeData(q45, sheet = "q45_q9", x = q45_q9)
saveWorkbook(q45, archivo, overwrite = TRUE)



###

#CRUCE POR q10

# Crear q10_2021_q13
q10_2021_q45 <- witm %>% 
  filter(q9_year_formation < 2022) %>% 
  group_by(q10_budget_grp_2021, q45_previous_response) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2021) %>% 
  mutate(Year = 2021)

# Crear q10_2022_q45
q10_2022_q45 <- witm %>% 
  filter(q9_year_formation < 2023) %>% 
  group_by(q10_budget_grp_2022, q45_previous_response) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2022) %>% 
  mutate(Year = 2022)

# Crear q10_2023_q45
q10_2023_q45 <- witm %>% 
  filter(!is.na(q10_budget_year_2023)) %>%
  group_by(q10_budget_grp_2023, q45_previous_response) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  rename(Annual_budget = q10_budget_grp_2023) %>% 
  mutate(Year = 2023)


# Unir los dataframes
q10_grouped_q45 <- bind_rows(q10_2021_q45, q10_2022_q45, q10_2023_q45)


# Crear la tabla de doble entrada con años como filas
q10_table <- q10_grouped_q45 %>%
  group_by(Year, Annual_budget, q45_previous_response) %>%
  summarise(Total = sum(n), .groups = 'drop') %>%
  pivot_wider(names_from = q45_previous_response, values_from = Total, values_fill = list(Total = 0)) %>%
  arrange(Year, Annual_budget)  # Opcional: ordenar por año y presupuesto

# Crear la tabla de doble entrada
q10_tableb <- q10_grouped_q45 %>%
  pivot_wider(names_from = Year, values_from = n, values_fill = list(n = 0))

# Calcular la media por cada categoría de Annual_budget
q10_tableb <- q10_tableb %>%
  rowwise() %>%
  mutate(Media = round(mean(c_across(c(`2021`, `2022`, `2023`)), na.rm = TRUE), 0)) %>%
  ungroup()  # Desagrupar después de la operación

# Seleccionar solo las columnas de interés para la tabla final
q10_media_table <- q10_tableb %>%
  select(Annual_budget, q45_previous_response, Media) %>%
  pivot_wider(names_from = q45_previous_response, values_from = Media, values_fill = list(Media = 0))

q45<- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_q10")
writeData(q45, sheet = "q45_q10", x = q10_tableb)
saveWorkbook(q45, archivo, overwrite = TRUE)

q45 <- loadWorkbook(archivo)
addWorksheet(q45, sheetName = "q45_q10_media")
writeData(q45, sheet = "q45_q10_media", x = q10_media_table)
saveWorkbook(q45, archivo, overwrite = TRUE)
##############################################################################

