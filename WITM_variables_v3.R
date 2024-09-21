

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

witm <- read.csv("Data/WITM_final_cleaned_08_sept.csv")


#Cambio formato fecha
#2024-05-23 08:37:41.178000-04:00
witm$date <-  as.Date(witm$end,'%Y-%m-%d')


witm$date_s <-  paste(format(as.Date(witm$date,format="%Y-%m-%d"), format = "%m"), format(as.Date(witm$date,format="%Y-%m-%d"), format = "%d"), sep ="-", collapse=NULL)


#Renombro variables
witm<-witm %>% 
  rename("q1_consent"=q0_consent, "q2_description"=q1_description, "q3_name"=q2_name, "q3_name_lower"=q2_name_lower, "q3_is_na"=is_na_q2_name) %>% 
  select(-non_missing_count, -is_best)

#Selecciono solo los que dieron consentimiento
base<-witm %>% 
  filter(q1_consent=="yes")

#Recodifico q4 agrupada
base<- base %>%
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



base<- base %>%
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

base<-base %>% 
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
base<-base %>% mutate(q5 = case_when(
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

base<- base %>%
  mutate(q10_budget_grp_2021=case_when
         (
           (q10_budget_year_2021=="(g) 100001 - 250000"  |
              q10_budget_year_2021=="(h) 250001 - 500000") ~ "(g) 100,001 – 500,001 USD",
           (q10_budget_year_2021=="(i) 500001 - 1000000"  |
              q10_budget_year_2021=="(j) 1000001 - 2000000") ~ "(h) 500,001 – $1,000,000 USD",
           (q10_budget_year_2021=="(k) 2000001 - 4000000" |
              q10_budget_year_2021=="(l) > 4000001") ~ "(i) 1,000,001 + USD",
           TRUE ~ q10_budget_year_2021))  

base<- base %>%
  mutate(q10_budget_grp_2022=case_when
         (
           (q10_budget_year_2022=="(g) 100001 - 250000"  |
              q10_budget_year_2022=="(h) 250001 - 500000") ~ "(g) 100,001 – 500,001 USD",
           (q10_budget_year_2022=="(i) 500001 - 1000000"  |
              q10_budget_year_2022=="(j) 1000001 - 2000000") ~ "(h) 500,001 – $1,000,000 USD",
           (q10_budget_year_2022=="(k) 2000001 - 4000000" |
              q10_budget_year_2022=="(l) > 4000001") ~ "(i) 1,000,001 + USD",
           TRUE ~ q10_budget_year_2022))  

base<- base %>%
  mutate(q10_budget_grp_2023=case_when
         (
           (q10_budget_year_2023=="(g) 100001 - 250000"  |
              q10_budget_year_2023=="(h) 250001 - 500000") ~ "(g) 100,001 – 500,001 USD",
           (q10_budget_year_2023=="(i) 500001 - 1000000"  |
              q10_budget_year_2023=="(j) 1000001 - 2000000") ~ "(h) 500,001 – $1,000,000 USD",
           (q10_budget_year_2023=="(k) 2000001 - 4000000" |
              q10_budget_year_2023=="(l) > 4000001") ~ "(i) 1,000,001 + USD",
           TRUE ~ q10_budget_year_2023))  

