

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


#witm <- read.csv("Data/WITM_final_cleaned_08_sept.csv")


#Cambio formato fecha
#2024-05-23 08:37:41.178000-04:00
witm$date <-  as.Date(witm$end,'%Y-%m-%d')

witm$date_s <-  paste(format(as.Date(witm$date,format="%Y-%m-%d"), format = "%m"), format(as.Date(witm$date,format="%Y-%m-%d"), format = "%d"), sep ="-", collapse=NULL)


#Renombro variables
witm<-witm %>% 
  rename("q1_consent"=q0_consent, "q2_description"=q1_description, "q3_name"=q2_name, "q3_name_lower"=q2_name_lower, "q3_is_na"=is_na_q2_name) %>% 
  select(-non_missing_count, -is_best)

#Selecciono solo los que dieron consentimiento
witm<-witm %>% 
  filter(q1_consent=="yes")

#Recodifico q4 agrupada
witm<- witm %>%
  mutate(q4_awid_focus=case_when
         (
           (q4_forms_organizing.lesbian_bisexual_queer_rights==1 |
              q4_forms_organizing.sex_workers_rights==1  |
              q4_forms_organizing.trans_non_binary_rights==1 |
              q4_forms_organizing.anti_caste==1 |
              q4_forms_organizing.climate_justice==1 |
              q4_forms_organizing.countering_anti_gender==1 |
              q4_forms_organizing.anti_militarization==1 |
              q4_forms_organizing.holistic_safety_protection_collective_care==1) ~1, TRUE ~0))  

#base<- base %>%
#  mutate(q4_awid_focus_no=case_when
#         (q4_awid_focus!=1 ~ 1, TRUE ~0))  



witm<- witm %>%
  mutate(q4_grouped_feminist=case_when
         (
           (q4_forms_organizing.womens_rights==1 |
              q4_forms_organizing.human_rights==1  |
              q4_forms_organizing.end_gender_violence ==1  |
              q4_forms_organizing.srhr_bodily_autonomy==1) ~1, TRUE ~0)) %>%  
  
  mutate(q4_grouped_minorities=case_when
         (
           (
             q4_forms_organizing.young_feminists==1 |
               q4_forms_organizing.girls_movements ==1 |
               q4_forms_organizing.disability_rights==1 |
               q4_forms_organizing.lesbian_bisexual_queer_rights==1 |
               q4_forms_organizing.sex_workers_rights==1 |
               q4_forms_organizing.rights_for_people_living_with_hiv==1 |
               q4_forms_organizing.displaced_migrant_refugee_rights==1 |
               q4_forms_organizing.trans_non_binary_rights==1 |
               q4_forms_organizing.intersex_rights==1 |
               q4_forms_organizing.labour_rights==1 |
               q4_forms_organizing.indigenous_rights==1 |
               q4_forms_organizing.black_rights==1 |
               q4_forms_organizing.anti_caste==1 |
               q4_forms_organizing.hr_defenders_at_risk==1 |
               q4_forms_organizing.religious_ethnic_minority_rights==1 ) ~1, TRUE ~0)) %>% 
  
  
  mutate(q4_grouped_focus=case_when
         (
           (
             q4_forms_organizing.climate_justice==1 |
               q4_forms_organizing.countering_anti_gender==1 |
               q4_forms_organizing.anti_militarization==1 |
               q4_forms_organizing.holistic_safety_protection_collective_care==1 |
               q4_forms_organizing.resisting_war_on_drugs==1 |
               q4_forms_organizing.economic_justice==1 |
               q4_forms_organizing.crisis_response==1 |
               q4_forms_organizing.digital_rights==1 |
               q4_forms_organizing.racial_justice==1 |
               q4_forms_organizing.information_media_freedom==1 |
               q4_forms_organizing.harm_reduction==1 |
               q4_forms_organizing.pleasure_bodily_care==1) ~1, TRUE ~0)) 


#ante-último pedido de agregación
#base<-base %>% 
#  mutate(q4_awid_LGBTIQ=case_when(q4_forms_organizing.lesbian_bisexual_queer_rights==1 | q4_forms_organizing.trans_non_binary_rights==1 |
#                                    q4_forms_organizing.intersex_rights==1 ~1, TRUE ~0)) %>% 
#  mutate(q4_awid_young=case_when(q4_forms_organizing.young_feminists==1 | q4_forms_organizing.girls_movements==1 ~1, TRUE ~0)) %>%
#  mutate(q4_awid_sex=case_when(q4_forms_organizing.sex_workers_rights==1 ~1, TRUE ~0)) %>%
#  mutate(q4_awid_nin=case_when(q4_forms_organizing.religious_ethnic_minority_rights==1 |q4_forms_organizing.anti_caste==1~1, TRUE ~0)) %>%
#  mutate(q4_awid_climate=case_when(q4_forms_organizing.climate_justice ==1 ~1, TRUE ~0)) %>%
#  mutate(q4_awid_antig=case_when(q4_forms_organizing.countering_anti_gender==1 ~1, TRUE ~0)) %>%
#  mutate(q4_awid_crisis=case_when(q4_forms_organizing.anti_militarization==1 | q4_forms_organizing.crisis_response==1 ~1, TRUE ~0)) %>%
#  mutate(q4_awid_harm=case_when(q4_forms_organizing.harm_reduction==1 |q4_forms_organizing.resisting_war_on_drugs==1  ~1, TRUE ~0))



####

##q4 last update

witm<-witm %>% 
  mutate(q4_awid_LGBTIQ=case_when(q4_forms_organizing.lesbian_bisexual_queer_rights==1 | q4_forms_organizing.trans_non_binary_rights==1 |
                                    q4_forms_organizing.intersex_rights==1 ~1, TRUE ~0)) %>% 
  mutate(q4_awid_young=case_when(q4_forms_organizing.young_feminists==1 | q4_forms_organizing.girls_movements==1 ~1, TRUE ~0)) %>%
  mutate(q4_awid_sex=case_when(q4_forms_organizing.sex_workers_rights==1 ~1, TRUE ~0)) %>%
  mutate(q4_awid_anticaste=case_when(q4_forms_organizing.anti_caste==1~1, TRUE ~0)) %>%
  mutate(q4_awid_climate=case_when(q4_forms_organizing.climate_justice ==1 ~1, TRUE ~0)) %>%
  mutate(q4_awid_antigender=case_when(q4_forms_organizing.countering_anti_gender==1 ~1, TRUE ~0)) %>%
  mutate(q4_awid_harm=case_when(q4_forms_organizing.harm_reduction==1 | q4_forms_organizing.resisting_war_on_drugs==1 ~1, TRUE ~0 )) %>% 
  mutate(q4_awid_disability=case_when(q4_forms_organizing.disability_rights== 1 ~1, TRUE ~0))

#revisión q5
witm<-witm %>% mutate(q5 = case_when(
  q5_registered == "n_registered" ~ "No",
  q5_registered == "y_registered" ~ "Yes",
  q5_registered == "98" ~ "Other",
  TRUE ~ NA_character_
)) %>% 
  mutate(q6 = case_when(
  q6_geo_scope=="globally_focused" ~ "Global",
  q6_geo_scope=="transnationally_focused" ~ "Transnational",
  q6_geo_scope=="regionally_focused" ~ "Regional",
  q6_geo_scope=="diaspora_and_or_exile" ~ "Diaspora or exile",
  q6_geo_scope=="nationally_focused" ~ "National",
  q6_geo_scope=="locally_focused" ~ "Local",
  TRUE ~ NA_character_
))


#recodificación q9


# witm<-witm %>% 
#   mutate(q9_year_formation_agrup=case_when(
#     q9_year_formation>=2021 & q9_year_formation<=2023 ~  "(1) 2021-2023",
#     q9_year_formation<2021 & q9_year_formation>2014 ~  "(2) From 2015 to 2020",
#     q9_year_formation<2015 & q9_year_formation>2009 ~  "(3) From 2010 to 2014",
#     q9_year_formation<2010 & q9_year_formation>1999 ~  "(4) From 2000 to 2009",
#     q9_year_formation<=2000 & q9_year_formation>1945 ~  "(5) Before 2000",
#     is.na(q9_year_formation) ~ NA,
#     TRUE ~  NA)) 
# 


#recodificación q10

witm<- witm %>%
  mutate(q10_budget_grp_2021=case_when
         (
           (q10_budget_year_2021=="(g) 100001 - 250000"  |
              q10_budget_year_2021=="(h) 250001 - 500000") ~ "(g) 100,001 – 500,001 USD",
           (q10_budget_year_2021=="(i) 500001 - 1000000"  |
              q10_budget_year_2021=="(j) 1000001 - 2000000") ~ "(h) 500,001 – $1,000,000 USD",
           (q10_budget_year_2021=="(k) 2000001 - 4000000" |
              q10_budget_year_2021=="(l) > 4000001") ~ "(i) 1,000,001 + USD",
           TRUE ~ q10_budget_year_2021))  

witm<- witm %>%
  mutate(q10_budget_grp_2022=case_when
         (
           (q10_budget_year_2022=="(g) 100001 - 250000"  |
              q10_budget_year_2022=="(h) 250001 - 500000") ~ "(g) 100,001 – 500,001 USD",
           (q10_budget_year_2022=="(i) 500001 - 1000000"  |
              q10_budget_year_2022=="(j) 1000001 - 2000000") ~ "(h) 500,001 – $1,000,000 USD",
           (q10_budget_year_2022=="(k) 2000001 - 4000000" |
              q10_budget_year_2022=="(l) > 4000001") ~ "(i) 1,000,001 + USD",
           TRUE ~ q10_budget_year_2022))  

witm<- witm %>%
  mutate(q10_budget_grp_2023=case_when
         (
           (q10_budget_year_2023=="(g) 100001 - 250000"  |
              q10_budget_year_2023=="(h) 250001 - 500000") ~ "(g) 100,001 – 500,001 USD",
           (q10_budget_year_2023=="(i) 500001 - 1000000"  |
              q10_budget_year_2023=="(j) 1000001 - 2000000") ~ "(h) 500,001 – $1,000,000 USD",
           (q10_budget_year_2023=="(k) 2000001 - 4000000" |
              q10_budget_year_2023=="(l) > 4000001") ~ "(i) 1,000,001 + USD",
           TRUE ~ q10_budget_year_2023))  

#agrupación regiones q7


witm<-witm %>% 
  #Convertir campos vacíos de la variable q7 en NA
  mutate(q7_region_one = na_if(q7_region_one, ""),
         q7_region_multiple = na_if(q7_region_multiple, "")) %>% 
  #Unifico REGIÓN en variables únicas
  mutate(samerica= ifelse(q7_region_one=="samerica"| q7_region_multiple.samerica==1, 1, 0),
         camerica= ifelse(q7_region_one=="camerica"| q7_region_multiple.camerica==1, 1, 0),
         namerica=ifelse(q7_region_one=="namerica"| q7_region_multiple.namerica==1, 1, 0),
         caribbean=ifelse(q7_region_one=="caribbean"| q7_region_multiple.caribbean==1, 1, 0),
         sasia=ifelse(q7_region_one=="sasia"| q7_region_multiple.sasia==1, 1, 0),
         seasia=ifelse(q7_region_one=="seasia"| q7_region_multiple.seasia==1, 1, 0),
         pacific=ifelse(q7_region_one=="pacific"| q7_region_multiple.pacific==1, 1, 0),
         easia=ifelse(q7_region_one=="easia"| q7_region_multiple.easia==1, 1, 0),
         casia=ifelse(q7_region_one=="casia"| q7_region_multiple.casia==1, 1, 0),
         swasiamiddleeast=ifelse(q7_region_one=="swasiamiddleeast"| q7_region_multiple.swasiamiddleeast==1, 1, 0),
         caucasuseurope=ifelse(q7_region_one=="caucasuseurope"| q7_region_multiple.caucasuseurope==1, 1, 0),
         weurope=ifelse(q7_region_one=="weurope"| q7_region_multiple.weurope==1, 1, 0),
         eafrica=ifelse(q7_region_one=="eafrica"| q7_region_multiple.eafrica==1, 1, 0),
         wafrica=ifelse(q7_region_one=="wafrica"| q7_region_multiple.wafrica==1, 1, 0),
         safrica=ifelse(q7_region_one=="safrica"| q7_region_multiple.safrica==1, 1, 0),
         cafrica=ifelse(q7_region_one=="cafrica"| q7_region_multiple.cafrica==1, 1, 0),
         nafrica=ifelse(q7_region_one=="nafrica"| q7_region_multiple.nafrica==1, 1, 0)) %>% 
  #agrupo según pedido
  mutate(region_1= case_when(samerica==1 | camerica==1 | caribbean==1 ~1,
                             TRUE ~ 0)) %>% 
  mutate(region_2=case_when(namerica==1 | weurope==1 ~1,
                            TRUE ~ 0)) %>%
  mutate(region_3=case_when(caucasuseurope==1 ~1,
                             TRUE ~ 0)) %>%
  mutate(region_4=case_when(cafrica==1 | eafrica==1 | wafrica==1 | safrica==1 ~1,
                            TRUE ~ 0)) %>%
  mutate(region_5=case_when(sasia==1 | seasia==1 | easia==1 | pacific==1 ~1,
                            TRUE ~ 0)) %>%
  mutate(region_6=case_when(casia==1 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(region_7=case_when(swasiamiddleeast==1 | nafrica==1 ~1,
                            TRUE ~ 0))


