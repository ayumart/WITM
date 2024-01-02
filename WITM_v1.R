


##limpiar ambiente, si es necesario
rm(list = ls())

##instalación de paquetes

#install.packages("ggplot2")
#install.packages("datapasta")
#install.packages('visdat')
#install.packages("remotes")
#install.packages("DiagrammeR")
#install.packages('dplyr')
#remotes::install_github("dickoa/robotoolbox")
#install.packages("eph")
#install.packages("remotes")
#remotes::install_github("matherion/userfriendlyscience", dependencies = FALSE, force = TRUE)



##cargar librerías
library(userfriendlyscience)
library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(robotoolbox)
library(labelled)
library(remotes)
library(dm)
library(janitor)
library(visdat)
library(dplyr)
library(writexl)
library(fontawesome)
library(knitr)
library(eph)
library("datapasta")


####se puede importar la base directamente de KOBO

###Insertar nombre de usuario y contraseña de KOBO (ESTO YA LO HICE Y ME DIO EL CÓDIGO QUE AGREGUÉ EN LA PRÓXIMA LÍNEA POR ESO LO DEJO COMENTADO)


#kobo_token(username = "XXX",
 #          password = "XXX",
  #         url = "https://eu.kobotoolbox.org")

###Se recibe un token de kobo que hay que insertar a continuación

kobo_setup(url = "https://eu.kobotoolbox.org",
           token = "236044df4e55ab2c3ea3b190445979cc13d13c38")

###Veo listas de encuestas
asset_list <- kobo_asset_list()

asset_list


##Selecciono la encuesta que deseo analizar

uid <- filter(asset_list, name == "Where is the Money for Feminist Organizing? (Pilot v12)") |> ## change the name accordingly
  pull(uid)

###Veo número de envíos y nombre

asset <- kobo_asset(uid)
asset


### Se ve el data frame directamente desde kobo
df <- kobo_data(asset)
df



### Elimino datasets que no encesito

rm(asset,asset_list, uid)



###Step 1. Check duplicates

duplicated(df) # Check if there are any duplicates
sum(duplicated(df)) # Number of duplicates
get_dupes(df)



#limpio y renombro variables
df2<-df %>% 
  select(!contains(c("info", "section", "note", "dollars", "titulo", "subtitulo", "q19_t", 
                     "q19_cf", "q19_pf", "q19_ltf", "q19_erf", "b4a", "b4b")))
  
names(df2)  
df2<-df2 %>% 
  rename(q16a ="_Q16_Between_2021_a_eceived_funding_from", q22="Q22_If_you_received_to_with_these_funds",
         q23="Q22_If_you_received_to_with_these_funds", 
         q23_a= "Q22_If_you_received_to_with_these_funds_outbreak_of_conflict__war__and_violence",
         q23_b= "Q22_If_you_received_to_with_these_funds_health_related_emergencies__disease_outb",
         q23_c= "Q22_If_you_received_to_with_these_funds_funds_for_covid_19_pandemic",
         q23_d= "Q22_If_you_received_to_with_these_funds_disaster_risk_reduction__drr__and_or_cli",
         q23_e="Q22_If_you_received_to_with_these_funds_physical_security_threats_against_staff_",
         q23_f="Q22_If_you_received_to_with_these_funds_crack_down_on_civic_space_and_anti_right",
         q23_g="Q22_If_you_received_to_with_these_funds_holistic_safety__protection_or_collectiv",
         q23_h="Q22_If_you_received_to_with_these_funds_other")


####### Creo función que convierte en numéricos valores de carácter 

labelled_chr2dbl <- function(x) {
  varlab <- var_label(x)
  vallab <- val_labels(x)
  vallab <- setNames(as.numeric(vallab),
                     names(vallab))
  x <- as.numeric(as.character(x))
  var_label(x) <- varlab
  val_labels(x) <- vallab
  x
}


#convierto en numérico variables (falta agregar variables!!!)
df2$q5 <- labelled_chr2dbl(df2$q5)
df2$q10 <- labelled_chr2dbl(df2$q10)



## INFORMACIÓN VARIABLES QUE SE DISTRIBUYEN POR AÑO

#######BUDGET

df2$q9_a <- labelled_chr2dbl(df2$q9_a)
df2$q9_b <- labelled_chr2dbl(df2$q9_b)
df2$q9_c <- labelled_chr2dbl(df2$q9_c)

budget_2023<-calculate_tabulates(base=df2,
                    x="q9_a")
names(budget_2023)[2]<-"2023"
names(budget_2023)[1]<-"cod"

budget_2022<-calculate_tabulates(base=df2,
                                 x="q9_b")
names(budget_2022)[2]<-"2022"
names(budget_2022)[1]<-"cod"

budget_2021<-calculate_tabulates(base=df2,
                                 x="q9_c")
names(budget_2021)[2]<-"2021"
names(budget_2021)[1]<-"cod"

budget_2021
budget_2022
budget_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

budget_total <- budget_2023 %>% full_join(budget_2022, by = "cod")
budget_total <- budget_total %>% full_join(budget_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="1" ~ 1,
                        cod=="2" ~ 2,
                        cod=="3" ~ 3,
                        cod=="4" ~ 4,
                        cod=="5" ~ 5,
                        cod=="6" ~ 6,
                        cod=="7" ~ 7,
                        cod=="8" ~ 8,
                        cod=="9" ~ 9)) %>% 
  arrange(cod)

print(budget_total)


#efecto pandemia
calculate_tabulates(base=df2,
                    x="qx1",
                    y="q8") ## se sugiere no hacer esta pregunta a aquellas organizaciones que se crearon post-2020.



## contabilizo respuestas que confirman haber tenido financiamiento externo entre 2021-2023

calculate_tabulates(base=df2,
                    x="q10")



##CREO BASES SEGÚN RECIBEN FINANCIAMIENTO EXTERNO
df3<-df2 %>% 
  filter(q10==1)


#financiamiento

financiamiento_2021<-calculate_tabulates(base=df3,
                                         x="q12a")
names(financiamiento_2021)[2]<-"2021"
names(financiamiento_2021)[1]<-"cod"

financiamiento_2022<-calculate_tabulates(base=df3,
                                         x="q12b")
names(financiamiento_2022)[2]<-"2022"
names(financiamiento_2022)[1]<-"cod"

financiamiento_2023<-calculate_tabulates(base=df3,
                                         x="q12c")
names(financiamiento_2023)[2]<-"2023"
names(financiamiento_2023)[1]<-"cod"

financiamiento_2021
financiamiento_2022
financiamiento_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

financiamiento_total <- financiamiento_2023 %>% full_join(financiamiento_2022, by = "cod")
financiamiento_total <- financiamiento_total %>% full_join(financiamiento_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(financiamiento_total)


#####MULTIRATERAL


multirateral_2021<-calculate_tabulates(base=df3,
                                         x="q14b_a")
names(multirateral_2021)[2]<-"2021"
names(multirateral_2021)[1]<-"cod"

multirateral_2022<-calculate_tabulates(base=df3,
                                       x="q14c_a")
names(multirateral_2022)[2]<-"2022"
names(multirateral_2022)[1]<-"cod"

multirateral_2023<-calculate_tabulates(base=df3,
                                       x="q14d_a")
names(multirateral_2023)[2]<-"2023"
names(multirateral_2023)[1]<-"cod"

multirateral_2021
multirateral_2022
multirateral_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

multirateral_total <- multirateral_2023 %>% full_join(multirateral_2022, by = "cod")
multirateral_total <- multirateral_total %>% full_join(multirateral_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(multirateral_total)


#####Bilateral funders (including embassies)

bilateral_2021<-calculate_tabulates(base=df3,
                                       x="q14b_b")
names(bilateral_2021)[2]<-"2021"
names(bilateral_2021)[1]<-"cod"

bilateral_2022<-calculate_tabulates(base=df3,
                                       x="q14c_b")
names(bilateral_2022)[2]<-"2022"
names(bilateral_2022)[1]<-"cod"

bilateral_2023<-calculate_tabulates(base=df3,
                                       x="q14d_b")
names(bilateral_2023)[2]<-"2023"
names(bilateral_2023)[1]<-"cod"

bilateral_2021
bilateral_2022
bilateral_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

bilateral_total <- bilateral_2023 %>% full_join(bilateral_2022, by = "cod")
bilateral_total <- bilateral_total %>% full_join(bilateral_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(bilateral_total)


#####Philanthropic Foundations

phil_2021<-calculate_tabulates(base=df3,
                                    x="q14b_c")
names(phil_2021)[2]<-"2021"
names(phil_2021)[1]<-"cod"

phil_2022<-calculate_tabulates(base=df3,
                                    x="q14c_c")
names(phil_2022)[2]<-"2022"
names(phil_2022)[1]<-"cod"

phil_2023<-calculate_tabulates(base=df3,
                                    x="q14d_c")
names(phil_2023)[2]<-"2023"
names(phil_2023)[1]<-"cod"

phil_2021
phil_2022
phil_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

phil_total <- phil_2023 %>% full_join(phil_2022, by = "cod")
phil_total <- phil_total %>% full_join(phil_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(phil_total)



#####National and local goverment or bodies

gov_2021<-calculate_tabulates(base=df3,
                               x="q14b_d")
names(gov_2021)[2]<-"2021"
names(gov_2021)[1]<-"cod"

gov_2022<-calculate_tabulates(base=df3,
                               x="q14c_d")
names(gov_2022)[2]<-"2022"
names(gov_2022)[1]<-"cod"

gov_2023<-calculate_tabulates(base=df3,
                               x="q14d_d")
names(gov_2023)[2]<-"2023"
names(gov_2023)[1]<-"cod"


gov_2021
gov_2022
gov_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

gov_total <- gov_2023 %>% full_join(gov_2022, by = "cod")
gov_total <- gov_total %>% full_join(gov_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(gov_total)


#####Women’s and feminist funds

women_2021<-calculate_tabulates(base=df3,
                               x="q14b_f")
names(women_2021)[2]<-"2021"
names(women_2021)[1]<-"cod"

women_2022<-calculate_tabulates(base=df3,
                               x="q14c_f")
names(women_2022)[2]<-"2022"
names(women_2022)[1]<-"cod"

women_2023<-calculate_tabulates(base=df3,
                               x="q14d_f")
names(women_2023)[2]<-"2023"
names(women_2023)[1]<-"cod"

women_2021
women_2022
women_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

women_total <- women_2023 %>% full_join(women_2022, by = "cod")
women_total <- women_total %>% full_join(women_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(women_total)


#####INGOs

ingo_2021<-calculate_tabulates(base=df3,
                                x="q14b_f")
names(ingo_2021)[2]<-"2021"
names(ingo_2021)[1]<-"cod"

ingo_2022<-calculate_tabulates(base=df3,
                                x="q14c_f")
names(ingo_2022)[2]<-"2022"
names(ingo_2022)[1]<-"cod"

ingo_2023<-calculate_tabulates(base=df3,
                                x="q14d_f")
names(ingo_2023)[2]<-"2023"
names(ingo_2023)[1]<-"cod"


ingo_2021
ingo_2022
ingo_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

ingo_total <- ingo_2023 %>% full_join(ingo_2022, by = "cod")
ingo_total <- ingo_total %>% full_join(ingo_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(ingo_total)


#####Private sector, including corporate foundations

private_2021<-calculate_tabulates(base=df3,
                               x="q14b_f")
names(private_2021)[2]<-"2021"
names(private_2021)[1]<-"cod"

private_2022<-calculate_tabulates(base=df3,
                               x="q14c_f")
names(private_2022)[2]<-"2022"
names(private_2022)[1]<-"cod"

private_2023<-calculate_tabulates(base=df3,
                               x="q14d_f")
names(private_2023)[2]<-"2023"
names(private_2023)[1]<-"cod"


private_2021
private_2022
private_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

private_total <- private_2023 %>% full_join(private_2022, by = "cod")
private_total <- private_total %>% full_join(private_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(private_total)

#####individual donors

ind_2021<-calculate_tabulates(base=df3,
                                  x="q14b_f")
names(ind_2021)[2]<-"2021"
names(ind_2021)[1]<-"cod"

ind_2022<-calculate_tabulates(base=df3,
                                  x="q14c_f")
names(ind_2022)[2]<-"2022"
names(ind_2022)[1]<-"cod"

ind_2023<-calculate_tabulates(base=df3,
                                  x="q14d_f")
names(ind_2023)[2]<-"2023"
names(ind_2023)[1]<-"cod"

ind_2021
ind_2022
ind_2023

#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

ind_total <- ind_2023 %>% full_join(ind_2022, by = "cod")
ind_total <- ind_total %>% full_join(ind_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(ind_total)

#####Other

otro_2021<-calculate_tabulates(base=df3,
                              x="q14b_f")
names(otro_2021)[2]<-"2021"
names(otro_2021)[1]<-"cod"

otro_2022<-calculate_tabulates(base=df3,
                              x="q14c_f")
names(otro_2022)[2]<-"2022"
names(otro_2022)[1]<-"cod"

otro_2023<-calculate_tabulates(base=df3,
                              x="q14d_f")
names(otro_2023)[2]<-"2023"
names(otro_2023)[1]<-"cod"


otro_2021
otro_2022
otro_2023


#UNIFICO LOS RESULTADOS DE LOS TRES AÑOS

otro_total <- otro_2023 %>% full_join(otro_2022, by = "cod")
otro_total <- otro_total %>% full_join(otro_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(otro_total)

########

#FONDOS 2023

core_2023<- calculate_tabulates(base=df3,
                                x="q19_a")
names(core_2023)[2]<-"core"
names(core_2023)[1]<-"cod"

proy_2023<-calculate_tabulates(base=df3,
                               x="q19_b")
names(proy_2023)[2]<-"proy"
names(proy_2023)[1]<-"cod"

emergency_2023<-calculate_tabulates(base=df3,
                               x="q19_c")
names(emergency_2023)[2]<-"emer"
names(emergency_2023)[1]<-"cod"

core_2023
proy_2023
emergency_2023

#UNIFICO LOS RESULTADOS DEL 2023

total_2023 <- core_2023 %>% full_join(proy_2023, by = "cod")
total_2023 <- total_2023 %>% full_join(emergency_2023, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(total_2023)


#FONDOS 2022

core_2022<- calculate_tabulates(base=df3,
                                x="q19a_a")
names(core_2022)[2]<-"core"
names(core_2022)[1]<-"cod"

proy_2022<-calculate_tabulates(base=df3,
                               x="q19a_b")
names(proy_2022)[2]<-"proy"
names(proy_2022)[1]<-"cod"

emergency_2022<-calculate_tabulates(base=df3,
                                    x="q19a_c")
names(emergency_2022)[2]<-"emer"
names(emergency_2022)[1]<-"cod"


core_2022
proy_2022
emergency_2022

#UNIFICO LOS RESULTADOS DEL 2022

total_2022 <- core_2022 %>% full_join(proy_2022, by = "cod")
total_2022 <- total_2022 %>% full_join(emergency_2022, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(total_2022)

#FONDOS 2021

core_2021<- calculate_tabulates(base=df3,
                                x="q19b_a")
names(core_2021)[2]<-"core"
names(core_2021)[1]<-"cod"

proy_2021<-calculate_tabulates(base=df3,
                               x="q19b_b")
names(proy_2021)[2]<-"proy"
names(proy_2021)[1]<-"cod"

emergency_2021<-calculate_tabulates(base=df3,
                                    x="q19b_c")
names(emergency_2021)[2]<-"emer"
names(emergency_2021)[1]<-"cod"

core_2021
proy_2021
emergency_2021

#UNIFICO LOS RESULTADOS DEL 2021

total_2021 <- core_2021 %>% full_join(proy_2021, by = "cod")
total_2021 <- total_2021 %>% full_join(emergency_2021, by = "cod") %>% 
  mutate(cod= case_when(cod=="0" ~ 0,
                        cod=="10" ~ 10,
                        cod=="20" ~ 20,
                        cod=="30" ~ 30,
                        cod=="40" ~ 40,
                        cod=="50" ~ 50,
                        cod=="60" ~ 60,
                        cod=="70" ~ 70,
                        cod=="80" ~ 80,
                        cod=="90" ~ 90,
                        cod=="100" ~ 100)) %>% 
  arrange(cod)

print(total_2021)

###RESPUESTA MÚLTIPLE

#convierto en numérico variables respuesta múltiple: EJ Q13

df3$q13_1 <- labelled_chr2dbl(df3$q13_1)
df3$q13_2 <- labelled_chr2dbl(df3$q13_2)
df3$q13_3 <- labelled_chr2dbl(df3$q13_3)
df3$q13_4 <- labelled_chr2dbl(df3$q13_4)
df3$q13_6 <- labelled_chr2dbl(df3$q13_6)
df3$q13_7 <- labelled_chr2dbl(df3$q13_7)
df3$q13_8 <- labelled_chr2dbl(df3$q13_8)
df3$q13_9 <- labelled_chr2dbl(df3$q13_9)
df3$q13_98 <- labelled_chr2dbl(df3$q13_98)


### RESULTADOS RESPUESTA MULTIPLE

resultados_q13<-multiResponse(df3, items = c("q13_1", "q13_2", "q13_3", "q13_4",
                                         "q13_6", "q13_7", "q13_8", "q13_9",
                                         "q13_98"),
                          endorsedOption = 1)

resultados_q13
