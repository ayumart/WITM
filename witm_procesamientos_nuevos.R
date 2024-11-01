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
names(witm)

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

#valores_representativos <- c(0, 2500, 7500, 20000, 40000, 75000, 175000, 375000, 
#                             750000, 1500000, 3000000, 4500000)

valores_representativos <- c(0, 5000, 10000, 30000, 50000, 100000, 250000, 500000, 
                             1000000, 2000000, 4000000, 4500000)

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



####


witm<- witm%>%
  mutate(q42 = case_when(
    q42_ORIGINAL == "zero_budget" ~ "(a) Zero budget",
    q42_ORIGINAL == "<5000" ~ "(b) <5000",
    q42_ORIGINAL == "<10000" ~ "(c) 5001-10000",
    q42_ORIGINAL == "<30000" ~ "(d) 10001-30000",
    q42_ORIGINAL == "<50000" ~ "(e) 30001 -50000",
    q42_ORIGINAL == "<100000" ~ "(f) 50001 - 100000",
    q42_ORIGINAL == "<250000" ~ "(g) 100001 - 250000",
    q42_ORIGINAL == "<500000" ~ "(h) 250001 - 500000",
    q42_ORIGINAL == "<1000000" ~ "(i) 500001 - 1000000",
    q42_ORIGINAL == "between_2m_and_4m_usd" ~ "(k) 2000001 - 4000000",
    q42_ORIGINAL == "greater_than_4m_usd" ~ "(l) > 4000001",
    TRUE ~ NA_character_  # Valor NA para casos que no coincidan
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
  summarise(Brecha_Media = mean(q42_rev - q10_2023_rev, na.rm = TRUE))
    
    
 ###

# Calcular el porcentaje de casos con brecha > 200000 por región
resultado <- witm%>%
  filter(!is.na(q10_ORIGINAL_2023) & !is.na(q42_aspirational_budget)) %>% 
  summarize(
    total_casos = n(),
    casos_con_brecha = sum(brecha > 200000, na.rm = TRUE),
    porcentaje_brecha = (casos_con_brecha / total_casos) * 100
  )

# Calcular el porcentaje de casos con brecha > 100000 por región
resultado2 <- witm%>%
  filter(!is.na(q10_ORIGINAL_2023) & !is.na(q42_aspirational_budget)) %>% 
  summarize(
    total_casos = n(),
    casos_con_brecha = sum(brecha > 100000, na.rm = TRUE),
    porcentaje_brecha = (casos_con_brecha / total_casos) * 100
  )

# Calcular el porcentaje de casos con brecha > 100000 por región
resultado0 <- witm%>%
  filter(!is.na(q10_ORIGINAL_2023) & !is.na(q42_aspirational_budget)) %>% 
  summarize(
    total_casos = n(),
    casos_con_brecha = sum(brecha ==0, na.rm = TRUE),
    porcentaje_brecha = (casos_con_brecha / total_casos) * 100
  )


##############################


#VERSIÓN PROMEDIO DE LA MEDIANA

# Añadir la columna de valores representativos a los data frames de cada año mediante una unión
q10_2021 <- q10_2021 %>%
  left_join(data.frame(categoria = categorias, valor_representativo = valores_representativos), by = c("q10_ORIGINAL_2021" = "categoria"))

q10_2022 <- q10_2022 %>%
  left_join(data.frame(categoria = categorias, valor_representativo = valores_representativos), by = c("q10_ORIGINAL_2022" = "categoria"))

q10_2023 <- q10_2023 %>%
  left_join(data.frame(categoria = categorias, valor_representativo = valores_representativos), by = c("q10_ORIGINAL_2023" = "categoria"))

# Filtrar las categorías (c) y (d) para cada año
q10_2021_cd <- q10_2021 %>% filter(q10_ORIGINAL_2021 %in% c("(c) 5001-10000", "(d) 10001-30000"))
q10_2022_cd <- q10_2022 %>% filter(q10_ORIGINAL_2022 %in% c("(c) 5001-10000", "(d) 10001-30000"))
q10_2023_cd <- q10_2023 %>% filter(q10_ORIGINAL_2023 %in% c("(c) 5001-10000", "(d) 10001-30000"))

# Calcular el promedio ponderado de presupuesto para las categorías (c) y (d) de cada año
promedio_2021_cd <- round(sum(q10_2021_cd$frecuencia * q10_2021_cd$valor_representativo, na.rm = TRUE) / sum(q10_2021_cd$frecuencia, na.rm = TRUE), 0)
promedio_2022_cd <- round(sum(q10_2022_cd$frecuencia * q10_2022_cd$valor_representativo, na.rm = TRUE) / sum(q10_2022_cd$frecuencia, na.rm = TRUE), 0)
promedio_2023_cd <- round(sum(q10_2023_cd$frecuencia * q10_2023_cd$valor_representativo, na.rm = TRUE) / sum(q10_2023_cd$frecuencia, na.rm = TRUE), 0)

# Mostrar los resultados
promedio_2021_cd
promedio_2022_cd
promedio_2023_cd


##REGIÓN

# Cálculo de frecuencias y promedio para el año 2021
Q10_2021_region <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2021)) %>% 
  group_by(q10_ORIGINAL_2021) %>%
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
  left_join(valores_df, by = c("q10_ORIGINAL_2021" = "categoria"))

# Filtrar solo las categorías (c) y (d)
Q10_2021_region_cd <- Q10_2021_region %>% filter(q10_ORIGINAL_2021 %in% c("(c) 5001-10000", "(d) 10001-30000"))

# Calcular el promedio ponderado de presupuesto para cada región
promedios_2021_region <- Q10_2021_region_cd %>%
  summarise(
    across(
      starts_with("1.") | starts_with("2.") | starts_with("3.") | 
        starts_with("4.") | starts_with("5.") | starts_with("6.") | 
        starts_with("7."), 
      ~ round(sum(. * valor_representativo, na.rm = TRUE) / sum(., na.rm = TRUE), 0),
      .names = "Promedio_{col}"
    )
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

# Filtrar solo las categorías (c) y (d)
Q10_2022_region_cd <- Q10_2022_region %>% filter(q10_ORIGINAL_2022 %in% c("(c) 5001-10000", "(d) 10001-30000"))

# Calcular el promedio ponderado de presupuesto para cada región
promedios_2022_region <- Q10_2022_region_cd %>%
  summarise(
    across(
      starts_with("1.") | starts_with("2.") | starts_with("3.") | 
        starts_with("4.") | starts_with("5.") | starts_with("6.") | 
        starts_with("7."), 
      ~ round(sum(. * valor_representativo, na.rm = TRUE) / sum(., na.rm = TRUE), 0),
      .names = "Promedio_{col}"
    )
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

# Filtrar solo las categorías (c) y (d)
Q10_2023_region_cd <- Q10_2023_region %>% filter(q10_ORIGINAL_2023 %in% c("(c) 5001-10000", "(d) 10001-30000"))

# Calcular el promedio ponderado de presupuesto para cada región
promedios_2023_region <- Q10_2023_region_cd %>%
  summarise(
    across(
      starts_with("1.") | starts_with("2.") | starts_with("3.") | 
        starts_with("4.") | starts_with("5.") | starts_with("6.") | 
        starts_with("7."), 
      ~ round(sum(. * valor_representativo, na.rm = TRUE) / sum(., na.rm = TRUE), 0),
      .names = "Promedio_{col}"
    )
  )

# Agregar la columna de Año a cada tabla de resultados
promedios_2021_region$Año <- "2021"
promedios_2022_region$Año <- "2022"
promedios_2023_region$Año <- "2023"

# Unir las tablas en una sola tabla de resultados
promedios_totales_region <- bind_rows(promedios_2021_region, promedios_2022_region, promedios_2023_region)

# Reorganizar la tabla para que la columna "Año" esté al inicio
promedios_totales_region <- promedios_totales_region %>%
  select(Año, everything())


###

#REGISTRO



# Cálculo de frecuencias y promedio para el año 2021
Q10_2021 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2021), !is.na(q5)) %>%  # Filtrar NA en q5 también
  group_by(q10_ORIGINAL_2021, q5) %>%  
  summarise(
    Total = n(),
    .groups = 'drop'
  ) %>% 
  left_join(valores_df, by = c("q10_ORIGINAL_2021" = "categoria"))

# Filtrar solo las categorías (c) y (d)
Q10_2021_cd <- Q10_2021 %>% filter(q10_ORIGINAL_2021 %in% c("(c) 5001-10000", "(d) 10001-30000"))

# Calcular el promedio ponderado de presupuesto para cada grupo de q5
promedios_2021 <- Q10_2021_cd %>%
  summarise(
    Promedio_Presupuesto = round(sum(Total * valor_representativo, na.rm = TRUE) / 
                                   sum(Total, na.rm = TRUE), 0),
    .by = "q5"
  )

# Cálculo de frecuencias y promedio para el año 2022
Q10_2022 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2022), !is.na(q5)) %>% 
  group_by(q10_ORIGINAL_2022, q5) %>% 
  summarise(
    Total = n(),
    .groups = 'drop'
  ) %>% 
  left_join(valores_df, by = c("q10_ORIGINAL_2022" = "categoria"))

# Filtrar solo las categorías (c) y (d)
Q10_2022_cd <- Q10_2022 %>% filter(q10_ORIGINAL_2022 %in% c("(c) 5001-10000", "(d) 10001-30000"))

# Calcular el promedio ponderado de presupuesto para cada grupo de q5
promedios_2022 <- Q10_2022_cd %>%
  summarise(
    Promedio_Presupuesto = round(sum(Total * valor_representativo, na.rm = TRUE) / 
                                   sum(Total, na.rm = TRUE), 0),
    .by = "q5"
  )

# Cálculo de frecuencias y promedio para el año 2023
Q10_2023 <- witm %>% 
  filter(!is.na(q10_ORIGINAL_2023), !is.na(q5)) %>% 
  group_by(q10_ORIGINAL_2023, q5) %>% 
  summarise(
    Total = n(),
    .groups = 'drop'
  ) %>% 
  left_join(valores_df, by = c("q10_ORIGINAL_2023" = "categoria"))

# Filtrar solo las categorías (c) y (d)
Q10_2023_cd <- Q10_2023 %>% filter(q10_ORIGINAL_2023 %in% c("(c) 5001-10000", "(d) 10001-30000"))

# Calcular el promedio ponderado de presupuesto para cada grupo de q5
promedios_2023 <- Q10_2023_cd %>%
  summarise(
    Promedio_Presupuesto = round(sum(Total * valor_representativo, na.rm = TRUE) / 
                                   sum(Total, na.rm = TRUE), 0),
    .by = "q5"
  )

# Agregar la columna de Año a cada tabla de resultados
promedios_2021$Año <- "2021"
promedios_2022$Año <- "2022"
promedios_2023$Año <- "2023"

# Unir las tablas en una sola tabla de resultados
promedios_totales <- bind_rows(promedios_2021, promedios_2022, promedios_2023)

# Reorganizar la tabla para que los años estén en las filas y las categorías de q5 en las columnas
promedios_final <- promedios_totales %>%
  pivot_wider(names_from = q5, values_from = Promedio_Presupuesto, 
              values_fill = list(Promedio_Presupuesto = NA)) # Rellenar con NA si no hay datos



####

# Definir las categorías y sus valores representativos
categorias <- c("(a) Zero budget", "(b) <5000", "(c) 5001-10000", "(d) 10001-30000",
                "(e) 30001 -50000", "(f) 50001 - 100000", "(g) 100001 - 250000",
                "(h) 250001 - 500000", "(i) 500001 - 1000000", "(j) 1000001 - 2000000",
                "(k) 2000001 - 4000000", "(l) > 4000001")

valores_representativos <- c(0, 5000, 10000, 30000, 50000, 100000, 250000, 500000, 
                             1000000, 2000000, 4000000, 4500000)

# Crear un data frame para asociar categorías con valores
categorias_df <- data.frame(categorias, valores_representativos)

# Calcular la frecuencia de organizaciones y la suma de presupuestos por región
frecuencias_region <- witm %>%
  mutate(presupuesto_numerico = case_when(
    q10_ORIGINAL_2023 == "(a) Zero budget" ~ valores_representativos[1],
    q10_ORIGINAL_2023 == "(b) <5000" ~ valores_representativos[2],
    q10_ORIGINAL_2023 == "(c) 5001-10000" ~ valores_representativos[3],
    q10_ORIGINAL_2023 == "(d) 10001-30000" ~ valores_representativos[4],
    q10_ORIGINAL_2023 == "(e) 30001 -50000" ~ valores_representativos[5],
    q10_ORIGINAL_2023 == "(f) 50001 - 100000" ~ valores_representativos[6],
    q10_ORIGINAL_2023 == "(g) 100001 - 250000" ~ valores_representativos[7],
    q10_ORIGINAL_2023 == "(h) 250001 - 500000" ~ valores_representativos[8],
    q10_ORIGINAL_2023 == "(i) 500001 - 1000000" ~ valores_representativos[9],
    q10_ORIGINAL_2023 == "(j) 1000001 - 2000000" ~ valores_representativos[10],
    q10_ORIGINAL_2023 == "(k) 2000001 - 4000000" ~ valores_representativos[11],
    q10_ORIGINAL_2023 == "(l) > 4000001" ~ valores_representativos[12],
    TRUE ~ NA_real_
  )) %>%
  group_by(q10_ORIGINAL_2023) %>% 
  summarise(
    "Total Organizations" = n(),
    "Total Budget" = sum(presupuesto_numerico, na.rm = TRUE),
    "1. South America" = sum(replace_na(samerica, 0) == 1),
    "2. Central America & Mexico" = sum(replace_na(camerica, 0) == 1),
    "3. North America" = sum(replace_na(namerica, 0) == 1),
    "4. Caribbean" = sum(replace_na(caribbean, 0) == 1),
    "5. South Asia" = sum(replace_na(sasia, 0) == 1),
    "6. Southeast Asia" = sum(replace_na(seasia, 0) == 1),
    "7. East Asia" = sum(replace_na(easia, 0) == 1),
    "8. The Pacific" = sum(replace_na(pacific, 0) == 1),
    "9. South West Asia/Middle East" = sum(replace_na(swasiamiddleeast, 0) == 1),
    "10. Central Asia & Caucasus" = sum(replace_na(casia, 0) == 1),
    "11. Eastern Europe, Southeast Europe and Central Europe" = sum(replace_na(caucasuseurope, 0) == 1),
    "12. Western Europe" = sum(replace_na(weurope, 0) == 1),
    "13. West Africa" = sum(replace_na(wafrica, 0) == 1),
    "14. East Africa" = sum(replace_na(eafrica, 0) == 1),
    "15. Southern Africa" = sum(replace_na(safrica, 0) == 1),
    "16. Central Africa" = sum(replace_na(cafrica, 0) == 1),
    "17. North Africa" = sum(replace_na(nafrica, 0) == 1)
  ) %>%
  mutate(Year = 2023)

mutate(result = ifelse(score >= 60, "Aprobado", "Reprobado"))

#nueva variable region 2011

witm<-witm %>% 
  mutate("1. Latin America"=ifelse((samerica=1 | camerica==1), 1,0),
         "2. Caribbean"= ifelse(caribbean==1,1,0),
         "3. Western Europe"=ifelse(weurope==1,1,0),
         "4. North America"=ifelse(namerica==1,1,0),
         "5. South Central, Eeastern Europe"=ifelse(caucasuseurope==1,1,0),
         "6. Sub Saharan Africa"= ifelse((eafrica==1 | wafrica==1 | cafrica==1 | safrica==1),1,0),
         "7. South and South East Asia"=ifelse((sasia==1 | seasia==1),1,0),
         "8. Pacific"=ifelse(pacific==1,1,0),
         "9. Eastern Asia"=ifelse(easia==1,1,0),
         "10. Central Asia y Cuacasus"=ifelse(casia==1,1,0),
         "11. MENA"=ifelse((swasiamiddleeast==1 | nafrica==1),1,0)
         ) 
  

###

# Crear un vector con los valores representativos
valores_representativos <- c(0, 5000, 10000, 30000, 50000, 100000, 250000, 500000, 
                             1000000, 2000000, 4000000, 4500000)

# Calcular la frecuencia de organizaciones y la suma de presupuestos por región
frecuencias_region <- witm %>%
  mutate(presupuesto_numerico = case_when(
    q10_ORIGINAL_2023 == "(a) Zero budget" ~ valores_representativos[1],
    q10_ORIGINAL_2023 == "(b) <5000" ~ valores_representativos[2],
    q10_ORIGINAL_2023 == "(c) 5001-10000" ~ valores_representativos[3],
    q10_ORIGINAL_2023 == "(d) 10001-30000" ~ valores_representativos[4],
    q10_ORIGINAL_2023 == "(e) 30001 -50000" ~ valores_representativos[5],
    q10_ORIGINAL_2023 == "(f) 50001 - 100000" ~ valores_representativos[6],
    q10_ORIGINAL_2023 == "(g) 100001 - 250000" ~ valores_representativos[7],
    q10_ORIGINAL_2023 == "(h) 250001 - 500000" ~ valores_representativos[8],
    q10_ORIGINAL_2023 == "(i) 500001 - 1000000" ~ valores_representativos[9],
    q10_ORIGINAL_2023 == "(j) 1000001 - 2000000" ~ valores_representativos[10],
    q10_ORIGINAL_2023 == "(k) 2000001 - 4000000" ~ valores_representativos[11],
    q10_ORIGINAL_2023 == "(l) > 4000001" ~ valores_representativos[12],
    TRUE ~ NA_real_
  )) %>%
  group_by(q10_ORIGINAL_2023) %>%
  summarise(
    "Total Organizations" = n(),
    "Total Budget" = sum(presupuesto_numerico, na.rm = TRUE),
    "1. Latin America" = sum(replace_na(samerica, 0) == 1),
    "2. Caribbean" = sum(replace_na(caribbean, 0) == 1),
    "3. Western Europe" = sum(replace_na(weurope, 0) == 1),
    "4. North America" = sum(replace_na(namerica, 0) == 1),
    "5. South Central, Eastern Europe" = sum(replace_na(caucasuseurope, 0) == 1),
    "6. Sub Saharan Africa" = sum(replace_na(safrica, 0) + replace_na(eafrica, 0) + replace_na(wafrica, 0) + replace_na(cafrica, 0)),
    "7. South and South East Asia" = sum(replace_na(sasia, 0) + replace_na(seasia, 0)),
    "8. Pacific" = sum(replace_na(pacific, 0) == 1),
    "9. Eastern Asia" = sum(replace_na(easia, 0) == 1),
    "10. Central Asia y Caucasus" = sum(replace_na(casia, 0) == 1),
    "11. MENA" = sum(replace_na(swasiamiddleeast, 0) + replace_na(nafrica, 0))
  ) %>%
  mutate(Year = 2023)


###

# Crear un vector con los valores representativos
valores_representativos <- c(0, 5000, 10000, 30000, 50000, 100000, 250000, 500000, 
                             1000000, 2000000, 4000000, 4500000)

# Calcular la frecuencia de organizaciones y la suma de presupuestos por región
resultados_region <- witm %>%
  mutate(presupuesto_numerico = case_when(
    q10_ORIGINAL_2023 == "(a) Zero budget" ~ valores_representativos[1],
    q10_ORIGINAL_2023 == "(b) <5000" ~ valores_representativos[2],
    q10_ORIGINAL_2023 == "(c) 5001-10000" ~ valores_representativos[3],
    q10_ORIGINAL_2023 == "(d) 10001-30000" ~ valores_representativos[4],
    q10_ORIGINAL_2023 == "(e) 30001 -50000" ~ valores_representativos[5],
    q10_ORIGINAL_2023 == "(f) 50001 - 100000" ~ valores_representativos[6],
    q10_ORIGINAL_2023 == "(g) 100001 - 250000" ~ valores_representativos[7],
    q10_ORIGINAL_2023 == "(h) 250001 - 500000" ~ valores_representativos[8],
    q10_ORIGINAL_2023 == "(i) 500001 - 1000000" ~ valores_representativos[9],
    q10_ORIGINAL_2023 == "(j) 1000001 - 2000000" ~ valores_representativos[10],
    q10_ORIGINAL_2023 == "(k) 2000001 - 4000000" ~ valores_representativos[11],
    q10_ORIGINAL_2023 == "(l) > 4000001" ~ valores_representativos[12],
    TRUE ~ NA_real_
  )) %>%
  # Agrupar y resumir los datos
  group_by(q10_ORIGINAL_2023) %>%
  summarise(
    "Total Organizations" = n(),
    "Total Budget" = sum(presupuesto_numerico, na.rm = TRUE),
    "1. Latin America" = sum(replace_na(samerica, 0) + replace_na(camerica, 0) == 1),
    "2. Caribbean" = sum(replace_na(caribbean, 0)  == 1),
    "3. Western Europe" = sum(replace_na(weurope, 0) == 1),
    "4. North America" = sum(replace_na(namerica, 0) == 1),
    "5. South Central, Eastern Europe" = sum(replace_na(caucasuseurope, 0) == 1),
    "6. Sub Saharan Africa" = sum(replace_na(safrica, 0) + replace_na(eafrica, 0) + replace_na(wafrica, 0) + replace_na(cafrica, 0)),
    "7. South and South East Asia" = sum(replace_na(sasia, 0) + replace_na(seasia, 0)),
    "8. Pacific" = sum(replace_na(pacific, 0) == 1),
    "9. Eastern Asia" = sum(replace_na(easia, 0) == 1),
    "10. Central Asia y Caucasus" = sum(replace_na(casia, 0) == 1),
    "11. MENA" = sum(replace_na(swasiamiddleeast, 0) + replace_na(nafrica, 0)),
    .groups = "drop"
  ) 


witm<-witm %>% 
  mutate("1. Latin America"=ifelse((samerica=1 | camerica==1), 1,0),
         "2. Caribbean"= ifelse(caribbean==1,1,0),
         "3. Western Europe"=ifelse(weurope==1,1,0),
         "4. North America"=ifelse(namerica==1,1,0),
         "5. South Central, Eeastern Europe"=ifelse(caucasuseurope==1,1,0),
         "6. Sub Saharan Africa"= ifelse((eafrica==1 | wafrica==1 | cafrica==1 | safrica==1),1,0),
         "7. South and South East Asia"=ifelse((sasia==1 | seasia==1),1,0),
         "8. Pacific"=ifelse(pacific==1,1,0),
         "9. Eastern Asia"=ifelse(easia==1,1,0),
         "10. Central Asia y Cuacasus"=ifelse(casia==1,1,0),
         "11. MENA"=ifelse((swasiamiddleeast==1 | nafrica==1),1,0)
  ) 

####

# Filtrar y calcular frecuencias para cada año y para organizaciones registradas/no registradas
q10_2021 <- witm %>%
  filter(!is.na(q10_ORIGINAL_2021), !is.na(q5)) %>%
  group_by(q10_ORIGINAL_2021, q5) %>%
  summarise(frecuencia = n(), .groups = 'drop') %>%
  mutate(año = 2021, 
         q10 = q10_ORIGINAL_2021) %>%
  select(año, q5, q10, frecuencia)

q10_2022 <- witm %>%
  filter(!is.na(q10_ORIGINAL_2022), !is.na(q5)) %>%
  group_by(q10_ORIGINAL_2022, q5) %>%
  summarise(frecuencia = n(), .groups = 'drop') %>%
  mutate(año = 2022, 
         q10 = q10_ORIGINAL_2022) %>%
  select(año, q5, q10, frecuencia)

q10_2023 <- witm %>%
  filter(!is.na(q10_ORIGINAL_2023), !is.na(q5)) %>%
  group_by(q10_ORIGINAL_2023, q5) %>%
  summarise(frecuencia = n(), .groups = 'drop') %>%
  mutate(año = 2023, 
         q10 = q10_ORIGINAL_2023) %>%
  select(año, q5, q10, frecuencia)

# Combinar los resultados de los tres años en un solo dataframe
resultados_totales <- bind_rows(q10_2021, q10_2022, q10_2023)
# Crear la tabla de doble entrada
tabla_doble_entrada <- resultados_totales %>%
  pivot_wider(
    names_from = q5,  # Convertir q5 en columnas
    values_from = frecuencia,  # Usar la frecuencia como valores
    values_fill = list(frecuencia = 0)  # Rellenar con 0 donde no hay datos
  )
