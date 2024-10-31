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

witm <- read.csv("Data/WITM_FINAL_10102024_V2.csv", header=T, sep=",")
unique(witm$q10_ORIGINAL_2021)


#convierto dato vacío en NA
witm <- witm %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) # Convierte cadenas vacías y solo espacios a NA


#frecuencia budget

# Calcular frecuencias para cada año y nombrar explícitamente la columna de frecuencia
q10_2021 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2021)) %>% 
  group_by(q10_ORIGINAL_2021) %>% 
  summarise(frecuencia = n(), .groups = 'drop') %>%
  mutate(Año = "2021")  # Añadir columna del año

q10_2022 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2022)) %>% 
  group_by(q10_ORIGINAL_2022) %>% 
  summarise(frecuencia = n(), .groups = 'drop') %>%
  mutate(Año = "2022")  # Añadir columna del año

q10_2023 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2023)) %>% 
  group_by(q10_ORIGINAL_2023) %>% 
  summarise(frecuencia = n(), .groups = 'drop') %>%
  mutate(Año = "2023")  # Añadir columna del año

# Renombrar las columnas para poder unirlas adecuadamente
q10_2021 <- q10_2021 %>% rename(Categoria = q10_ORIGINAL_2021)
q10_2022 <- q10_2022 %>% rename(Categoria = q10_ORIGINAL_2022)
q10_2023 <- q10_2023 %>% rename(Categoria = q10_ORIGINAL_2023)

# Unir los data frames de los tres años en uno solo
frecuencias_totales <- bind_rows(q10_2021, q10_2022, q10_2023)

# Transformar la tabla para que las categorías sean filas y los años sean columnas
frecuencias_wide <- frecuencias_totales %>%
  pivot_wider(names_from = Año, values_from = frecuencia, values_fill = 0)


#promedio budget

# Calcular frecuencias para cada año y nombrar explícitamente la columna de frecuencia
q10_2021 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2021)) %>% 
  group_by(q10_ORIGINAL_2021) %>% 
  summarise(frecuencia = n())

q10_2022 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2022)) %>% 
  group_by(q10_ORIGINAL_2022) %>% 
  summarise(frecuencia = n())

q10_2023 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2023)) %>% 
  group_by(q10_ORIGINAL_2023) %>% 
  summarise(frecuencia = n())

# Crear un vector con los nombres de las categorías y sus valores representativos
categorias <- c("(a) Zero budget", "(b) <5000", "(c) 5001-10000", "(d) 10001-30000",
                "(e) 30001 -50000", "(f) 50001 - 100000", "(g) 100001 - 250000",
                "(h) 250001 - 500000", "(i) 500001 - 1000000", "(j) 1000001 - 2000000",
                "(k) 2000001 - 4000000", "(l) > 4000001")

valores_representativos <- c(0, 2500, 7500, 20000, 40000, 75000, 175000, 375000, 
                             750000, 1500000, 3000000, 4500000)

# Crear un data frame que relacione categorías con valores representativos
valores_df <- data.frame(categoria = categorias, valor_representativo = valores_representativos)

# Añadir la columna de valores representativos a los data frames de cada año mediante una unión
q10_2021 <- q10_2021 %>%
  left_join(valores_df, by = c("q10_ORIGINAL_2021" = "categoria"))

q10_2022 <- q10_2022 %>%
  left_join(valores_df, by = c("q10_ORIGINAL_2022" = "categoria"))

q10_2023 <- q10_2023 %>%
  left_join(valores_df, by = c("q10_ORIGINAL_2023" = "categoria"))

# Calcular el promedio de presupuesto para cada año
promedio_2021 <- round(sum(q10_2021$frecuencia * q10_2021$valor_representativo, na.rm = TRUE) / sum(q10_2021$frecuencia, na.rm = TRUE),0)
promedio_2022 <- round(sum(q10_2022$frecuencia * q10_2022$valor_representativo, na.rm = TRUE) / sum(q10_2022$frecuencia, na.rm = TRUE),0)
promedio_2023 <- round(sum(q10_2023$frecuencia * q10_2023$valor_representativo, na.rm = TRUE) / sum(q10_2023$frecuencia, na.rm = TRUE),0)


# Crear data frames de promedios de presupuesto para cada año
promedio_2021_df <- data.frame(Año = "2021", Promedio_Presupuesto = promedio_2021)
promedio_2022_df <- data.frame(Año = "2022", Promedio_Presupuesto = promedio_2022)
promedio_2023_df <- data.frame(Año = "2023", Promedio_Presupuesto = promedio_2023)

# Unir los data frames de promedios en una tabla única
promedios_totales_df <- bind_rows(promedio_2021_df, promedio_2022_df, promedio_2023_df)

###
#REGIÓN

# Cálculo de frecuencias y promedio para el año 2021
Q10_2021_region <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2021)) %>% 
  group_by(q10_ORIGINAL_2021) %>%
  summarise(Total = n(),
            "1. Latin America & the Caribbean" = sum(region_1 == 1),
            "2. Western Europe & North America" = sum(region_2 == 1),
            "3. Eastern, Southeast and Central Europe" = sum(region_3 == 1),
            "4. Africa" = sum(region_4 == 1),
            "5. Asia & the Pacific" = sum(region_5 == 1),
            "6. Central Asia & Caucasus" = sum(region_6 == 1),
            "7. South West Asia/Middle East & North Africa" = sum(region_7 == 1)) %>% 
  left_join(valores_df, by = c("q10_ORIGINAL_2021" = "categoria"))

# Redondear los resultados sin decimales para 2021
promedio_2021_region <- Q10_2021_region %>% 
  summarise(
    Year = 2021,  # Añadir columna para el año
    Latin_America_Caribbean = round(sum(`1. Latin America & the Caribbean` * valor_representativo, na.rm = TRUE) / sum(`1. Latin America & the Caribbean`, na.rm = TRUE)),
    Western_Europe_North_America = round(sum(`2. Western Europe & North America` * valor_representativo, na.rm = TRUE) / sum(`2. Western Europe & North America`, na.rm = TRUE)),
    Eastern_Southeast_Central_Europe = round(sum(`3. Eastern, Southeast and Central Europe` * valor_representativo, na.rm = TRUE) / sum(`3. Eastern, Southeast and Central Europe`, na.rm = TRUE)),
    Africa = round(sum(`4. Africa` * valor_representativo, na.rm = TRUE) / sum(`4. Africa`, na.rm = TRUE)),
    Asia_Pacific = round(sum(`5. Asia & the Pacific` * valor_representativo, na.rm = TRUE) / sum(`5. Asia & the Pacific`, na.rm = TRUE)),
    Central_Asia_Caucasus = round(sum(`6. Central Asia & Caucasus` * valor_representativo, na.rm = TRUE) / sum(`6. Central Asia & Caucasus`, na.rm = TRUE)),
    South_West_Asia_Middle_East_North_Africa = round(sum(`7. South West Asia/Middle East & North Africa` * valor_representativo, na.rm = TRUE) / sum(`7. South West Asia/Middle East & North Africa`, na.rm = TRUE))
  )

# Cálculo de frecuencias y promedio para el año 2022
Q10_2022_region <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2022)) %>% 
  group_by(q10_ORIGINAL_2022) %>% 
  summarise(
    Total = n(),
    "1. Latin America & the Caribbean" = sum(region_1 == 1),
    "2. Western Europe & North America" = sum(region_2 == 1),
    "3. Eastern, Southeast and Central Europe" = sum(region_3 == 1),
    "4. Africa" = sum(region_4 == 1),
    "5. Asia & the Pacific" = sum(region_5 == 1),
    "6. Central Asia & Caucasus" = sum(region_6 == 1),
    "7. South West Asia/Middle East & North Africa" = sum(region_7 == 1)
  ) %>% 
  left_join(valores_df, by = c("q10_ORIGINAL_2022" = "categoria"))

# Redondear los resultados sin decimales para 2022
promedio_2022_region <- Q10_2022_region %>% 
  summarise(
    Year = 2022,  # Añadir columna para el año
    Latin_America_Caribbean = round(sum(`1. Latin America & the Caribbean` * valor_representativo, na.rm = TRUE) / sum(`1. Latin America & the Caribbean`, na.rm = TRUE)),
    Western_Europe_North_America = round(sum(`2. Western Europe & North America` * valor_representativo, na.rm = TRUE) / sum(`2. Western Europe & North America`, na.rm = TRUE)),
    Eastern_Southeast_Central_Europe = round(sum(`3. Eastern, Southeast and Central Europe` * valor_representativo, na.rm = TRUE) / sum(`3. Eastern, Southeast and Central Europe`, na.rm = TRUE)),
    Africa = round(sum(`4. Africa` * valor_representativo, na.rm = TRUE) / sum(`4. Africa`, na.rm = TRUE)),
    Asia_Pacific = round(sum(`5. Asia & the Pacific` * valor_representativo, na.rm = TRUE) / sum(`5. Asia & the Pacific`, na.rm = TRUE)),
    Central_Asia_Caucasus = round(sum(`6. Central Asia & Caucasus` * valor_representativo, na.rm = TRUE) / sum(`6. Central Asia & Caucasus`, na.rm = TRUE)),
    South_West_Asia_Middle_East_North_Africa = round(sum(`7. South West Asia/Middle East & North Africa` * valor_representativo, na.rm = TRUE) / sum(`7. South West Asia/Middle East & North Africa`, na.rm = TRUE))
  )

# Cálculo de frecuencias y promedio para el año 2023
Q10_2023_region <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2023)) %>% 
  group_by(q10_ORIGINAL_2023) %>% 
  summarise(
    Total = n(),
    "1. Latin America & the Caribbean" = sum(region_1 == 1),
    "2. Western Europe & North America" = sum(region_2 == 1),
    "3. Eastern, Southeast and Central Europe" = sum(region_3 == 1),
    "4. Africa" = sum(region_4 == 1),
    "5. Asia & the Pacific" = sum(region_5 == 1),
    "6. Central Asia & Caucasus" = sum(region_6 == 1),
    "7. South West Asia/Middle East & North Africa" = sum(region_7 == 1)
  ) %>% 
  left_join(valores_df, by = c("q10_ORIGINAL_2023" = "categoria"))

# Redondear los resultados sin decimales para 2023
promedio_2023_region <- Q10_2023_region %>% 
  summarise(
    Year = 2023,  # Añadir columna para el año
    Latin_America_Caribbean = round(sum(`1. Latin America & the Caribbean` * valor_representativo, na.rm = TRUE) / sum(`1. Latin America & the Caribbean`, na.rm = TRUE)),
    Western_Europe_North_America = round(sum(`2. Western Europe & North America` * valor_representativo, na.rm = TRUE) / sum(`2. Western Europe & North America`, na.rm = TRUE)),
    Eastern_Southeast_Central_Europe = round(sum(`3. Eastern, Southeast and Central Europe` * valor_representativo, na.rm = TRUE) / sum(`3. Eastern, Southeast and Central Europe`, na.rm = TRUE)),
    Africa = round(sum(`4. Africa` * valor_representativo, na.rm = TRUE) / sum(`4. Africa`, na.rm = TRUE)),
    Asia_Pacific = round(sum(`5. Asia & the Pacific` * valor_representativo, na.rm = TRUE) / sum(`5. Asia & the Pacific`, na.rm = TRUE)),
    Central_Asia_Caucasus = round(sum(`6. Central Asia & Caucasus` * valor_representativo, na.rm = TRUE) / sum(`6. Central Asia & Caucasus`, na.rm = TRUE)),
    South_West_Asia_Middle_East_North_Africa = round(sum(`7. South West Asia/Middle East & North Africa` * valor_representativo, na.rm = TRUE) / sum(`7. South West Asia/Middle East & North Africa`, na.rm = TRUE))
  )

# Unir todos los promedios en una sola tabla
promedios_region <- bind_rows(promedio_2021_region, promedio_2022_region, promedio_2023_region)


###

#registro



# Filtrar y calcular frecuencias para cada año y para organizaciones registradas/no registradas
q10_2021 <- witm %>%
  filter(!is.na(q10_ORIGINAL_2021), !is.na(q5)) %>%  # Asegurarse de que q5 no sea NA
  group_by(q10_ORIGINAL_2021, q5) %>%  # Agrupar por q5 también
  summarise(frecuencia = n(), .groups = 'drop')

q10_2022 <- witm %>%
  filter(!is.na(q10_ORIGINAL_2022), !is.na(q5)) %>%  # Asegurarse de que q5 no sea NA
  group_by(q10_ORIGINAL_2022, q5) %>%  # Agrupar por q5 también
  summarise(frecuencia = n(), .groups = 'drop')

q10_2023 <- witm %>%
  filter(!is.na(q10_ORIGINAL_2023), !is.na(q5)) %>%  # Asegurarse de que q5 no sea NA
  group_by(q10_ORIGINAL_2023, q5) %>%  # Agrupar por q5 también
  summarise(frecuencia = n(), .groups = 'drop')

# Crear un vector con los nombres de las categorías y sus valores representativos
categorias <- c("(a) Zero budget", "(b) <5000", "(c) 5001-10000", "(d) 10001-30000",
                "(e) 30001 -50000", "(f) 50001 - 100000", "(g) 100001 - 250000",
                "(h) 250001 - 500000", "(i) 500001 - 1000000", "(j) 1000001 - 2000000",
                "(k) 2000001 - 4000000", "(l) > 4000001")

valores_representativos <- c(0, 2500, 7500, 20000, 40000, 75000, 175000, 375000, 
                             750000, 1500000, 3000000, 4500000)

# Crear un data frame que relacione categorías con valores representativos
valores_df <- data.frame(categoria = categorias, valor_representativo = valores_representativos)

# Añadir la columna de valores representativos a los data frames de cada año mediante una unión
q10_2021 <- q10_2021 %>%
  left_join(valores_df, by = c("q10_ORIGINAL_2021" = "categoria"))

q10_2022 <- q10_2022 %>%
  left_join(valores_df, by = c("q10_ORIGINAL_2022" = "categoria"))

q10_2023 <- q10_2023 %>%
  left_join(valores_df, by = c("q10_ORIGINAL_2023" = "categoria"))

# Calcular el promedio de presupuesto para cada año, separando por organizaciones registradas y no registradas
promedio_2021 <- q10_2021 %>%
  group_by(q5) %>%  # Agrupa por la variable de organización registrada
  summarise(
    Promedio_Presupuesto = round(sum(frecuencia * valor_representativo, na.rm = TRUE) / sum(frecuencia, na.rm = TRUE), 0),
    .groups = 'drop'
  )

promedio_2022 <- q10_2022 %>%
  group_by(q5) %>%  # Agrupa por la variable de organización registrada
  summarise(
    Promedio_Presupuesto = round(sum(frecuencia * valor_representativo, na.rm = TRUE) / sum(frecuencia, na.rm = TRUE), 0),
    .groups = 'drop'
  )

promedio_2023 <- q10_2023 %>%
  group_by(q5) %>%  # Agrupa por la variable de organización registrada
  summarise(
    Promedio_Presupuesto = round(sum(frecuencia * valor_representativo, na.rm = TRUE) / sum(frecuencia, na.rm = TRUE), 0),
    .groups = 'drop'
  )

# Añadir una columna para indicar el año
promedio_2021 <- promedio_2021 %>% mutate(Año = "2021")
promedio_2022 <- promedio_2022 %>% mutate(Año = "2022")
promedio_2023 <- promedio_2023 %>% mutate(Año = "2023")

# Unir los data frames de los tres años en uno solo
promedios_totales <- bind_rows(promedio_2021, promedio_2022, promedio_2023)

# Reordenar las columnas si es necesario
promedios_totales <- promedios_totales %>% select(Año, q5, Promedio_Presupuesto)


# Girar la tabla para que las categorías sean columnas
tabla_girada <- promedios_totales %>%
  pivot_wider(names_from = q5, values_from = Promedio_Presupuesto)


unique(witm$q42_aspirational_budget)


####


witm<-witm %>% 
  mutate(q42=case_when(
    q42_aspirational_budget == "(a) Zero budget" ~ "(a) Zero budget",
    q42_aspirational_budget == "(b) < 5,000" ~ "(b) <5000",
    q42_aspirational_budget == "(c) 5,001 - 10,000" ~ "(c) 5001-10000",
    q42_aspirational_budget == "(d) 10,001 - 30,000" ~ "(d) 10001-30000",
    q42_aspirational_budget == "(e) 30,001 - 50,000" ~ "(e) 30001 -50000",
    q42_aspirational_budget == "(f) 50,001 - 100,000" ~ "(f) 50001 - 100000",
    q42_aspirational_budget == "(g) 100,001 - 500,000" ~ "(g) 100001 - 250000", # Verificar si esto es correcto
    q42_aspirational_budget == "(h) 500,001 - 1,000,000" ~ "(h) 250001 - 500000",
    q42_aspirational_budget == "(i) > 1,000,000" ~ "(i) 500001 - 1000000", # Verificar si esta categoría es correcta
    TRUE ~ NA_character_  # Asignar NA a valores que no coincidan
    )) %>% 
  mutate(
    q42_rev = case_when(
      q42 == "(a) Zero budget" ~ 0,
      q42 == "(b) <5000" ~ 2500,
      q42 == "(c) 5001-10000" ~ 7500,
      q42 == "(d) 10001-30000" ~ 20000,
      q42 == "(e) 30001 -50000" ~ 40000,
      q42 == "(f) 50001 - 100000" ~ 75000,
      q42 == "(g) 100001 - 250000" ~ 175000, # Ajustado al valor medio de la categoría
      q42 == "(h) 250001 - 500000" ~ 375000,
      q42 == "(i) 500001 - 1000000" ~ 750000,
      q42 == "(j) 1000001 - 2000000" ~ 1500000,
      q42 == "(k) 2000001 - 4000000" ~ 3000000,
      q42 == "(l) > 4000000" ~ 4500000,
      TRUE ~ NA_real_  # Asignar NA a valores que no coincidan
    )
  ) %>% 
  mutate(
    q10_2023_rev = case_when(
      q10_ORIGINAL_2023 == "(a) Zero budget" ~ 0,
      q10_ORIGINAL_2023 == "(b) <5000" ~ 2500,
      q10_ORIGINAL_2023 == "(c) 5001-10000" ~ 7500,
      q10_ORIGINAL_2023 == "(d) 10001-30000" ~ 20000,
      q10_ORIGINAL_2023 == "(e) 30001 -50000" ~ 40000,
      q10_ORIGINAL_2023 == "(f) 50001 - 100000" ~ 75000,
      q10_ORIGINAL_2023 == "(g) 100001 - 250000" ~ 175000, # Ajustado al valor medio de la categoría
      q10_ORIGINAL_2023 == "(h) 250001 - 500000" ~ 375000,
      q10_ORIGINAL_2023 == "(i) 500001 - 1000000" ~ 750000,
      q10_ORIGINAL_2023 == "(j) 1000001 - 2000000" ~ 1500000,
      q10_ORIGINAL_2023 == "(k) 2000001 - 4000000" ~ 3000000,
      q10_ORIGINAL_2023 == "(l) > 4000000" ~ 4500000,
      TRUE ~ NA_real_  # Asignar NA a valores que no coincidan
    )
  )

###

witm <- witm %>%
  mutate(
    brecha = q42_rev - q10_2023_rev  # Calcular la brecha
  )

# Resumen de la brecha
brecha_resumen <- witm %>%
  filter(!is.na(q42_aspirational_budget) & !is.na(q10_ORIGINAL_2023)) %>% 
  summarise(
    brecha_media = mean(brecha, na.rm = TRUE),  # Media de la brecha
    brecha_minima = min(brecha, na.rm = TRUE),  # Brecha mínima
    brecha_maxima = max(brecha, na.rm = TRUE),  # Brecha máxima
    brecha_total = sum(brecha, na.rm = TRUE)     # Suma total de la brecha
  )

# Asegúrate de que q42_rev y q10_2023_rev sean numéricas
witm$q42_rev <- as.numeric(as.character(witm$q42_rev))
witm$q10_2023_rev <- as.numeric(as.character(witm$q10_2023_rev))

# Calcular la brecha entre q42_rev y q10_2023_rev para cada región
brecha_region <- witm %>%
  filter(!is.na(q42_aspirational_budget) & !is.na(q10_ORIGINAL_2023)) %>% 
  summarise(
    Brecha_LAC = mean(q42_rev[region_1 == 1] - q10_2023_rev[region_1 == 1], na.rm = TRUE),
    Brecha_WE_NA = mean(q42_rev[region_2 == 1] - q10_2023_rev[region_2 == 1], na.rm = TRUE),
    Brecha_EE_CE = mean(q42_rev[region_3 == 1] - q10_2023_rev[region_3 == 1], na.rm = TRUE),
    Brecha_Africa = mean(q42_rev[region_4 == 1] - q10_2023_rev[region_4 == 1], na.rm = TRUE),
    Brecha_AP = mean(q42_rev[region_5 == 1] - q10_2023_rev[region_5 == 1], na.rm = TRUE),
    Brecha_CAC = mean(q42_rev[region_6 == 1] - q10_2023_rev[region_6 == 1], na.rm = TRUE),
    Brecha_SWAMENA = mean(q42_rev[region_7 == 1] - q10_2023_rev[region_7 == 1], na.rm = TRUE)
  )


# Calcular la brecha media entre q42_rev y q10_2023_rev para cada nivel de registro (q5)
brecha_media_registro <- witm %>%
  filter(!is.na(q42_aspirational_budget) & !is.na(q10_ORIGINAL_2023) & !is.na(q5)) %>% 
  group_by(q5) %>%
  summarise(
    Brecha_Media = mean(q42_rev - q10_2023_rev, na.rm = TRUE)
  )
