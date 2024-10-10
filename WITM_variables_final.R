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

witm <- read.csv("Data/WITM_cleaned_09232024.csv")

names(witm)

witm$date <-  as.Date(witm$end,'%Y-%m-%d')

witm$date_s <-  paste(format(as.Date(witm$date,format="%Y-%m-%d"), format = "%m"), format(as.Date(witm$date,format="%Y-%m-%d"), format = "%d"), sep ="-", collapse=NULL)


#Renombro variables
witm<-witm %>% 
  rename("q1_consent"=q0_consent, "q2_description"=q1_description, "q3_name"=q2_name, "q3_name_lower"=q2_name_lower, "q3_is_na"=is_na_q2_name, q5=q5_registered, q6=q6_geo_scope) %>% 
  select(-non_missing_count, -is_best)



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

#nombre categorías q5
witm<-witm %>% 
  mutate(q5 = case_when(
  q5 == "n_registered" ~ "No",
  q5 == "y_registered" ~ "Yes",
  q5 == "98" ~ "Other",
  TRUE ~ NA_character_
)) %>% 
  mutate(q6 = case_when(
    q6=="globally_focused" ~ "Global",
    q6=="transnationally_focused" ~ "Transnational",
    q6=="regionally_focused" ~ "Regional",
    q6=="diaspora_and_or_exile" ~ "Diaspora or exile",
    q6=="nationally_focused" ~ "National",
    q6=="locally_focused" ~ "Local",
    TRUE ~ NA_character_
  ))


#recodificación q9


witm<-witm %>% 
  mutate(q9_year_formation_agrup=case_when(
     q9_year_formation>=2021 & q9_year_formation<=2023 ~  "(1) 2021-2023",
     q9_year_formation<2021 & q9_year_formation>2014 ~  "(2) From 2015 to 2020",
     q9_year_formation<2015 & q9_year_formation>2009 ~  "(3) From 2010 to 2014",
     q9_year_formation<2010 & q9_year_formation>1999 ~  "(4) From 2000 to 2009",
     q9_year_formation<=2000 & q9_year_formation>1945 ~  "(5) Before 2000",
     is.na(q9_year_formation) ~ NA,
     TRUE ~  NA)) 


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


#Renombro categorías de país
witm<-witm %>% 
  mutate(country_var=case_when(q8_country=="afghanistan"~"Afghanistan", q8_country=="albania"~"Albania", q8_country=="algeria"~"Algeria", q8_country=="american_samoa"~"American Samoa", 
                             q8_country=="andorra"~"Andorra", q8_country=="angola"~"Angola", q8_country=="anguilla"~"Anguilla", q8_country=="antigua_and_barbuda"~"Antigua and Barbuda", 
                             q8_country=="argentina"~"Argentina", q8_country=="armenia"~"Armenia", q8_country=="aruba"~"Aruba", q8_country=="australia"~"Australia", q8_country=="austria"~"Austria", 
                             q8_country=="azerbaijan"~"Azerbaijan", q8_country=="bahamas"~"Bahamas", q8_country=="bahrain"~"Bahrain", q8_country=="bangladesh"~"Bangladesh", 
                             q8_country=="barbados"~"Barbados", q8_country=="belarus"~"Belarus", q8_country=="belgium"~"Belgium", q8_country=="belize"~"Belize", q8_country=="benin"~"Benin", 
                             q8_country=="bhutan"~"Bhutan", q8_country=="bolivia"~"Bolivia", q8_country=="bonaire_sint_eustatius_saba"~"Bonaire, Sint Eustatius and Saba", 
                             q8_country=="bosnia_and_herzegovina"~"Bosnia and Herzegovina", q8_country=="botswana"~"Botswana", q8_country=="brazil"~"Brazil", 
                             q8_country=="british_virgin_islands"~"British Virgin Islands", q8_country=="brunei_darussalam"~"Brunei Darussalam", q8_country=="bulgaria"~"Bulgaria", 
                             q8_country=="cabo_verde"~"Cabo Verde", q8_country=="burundi"~"Burundi", q8_country=="cambodia"~"Cambodia", q8_country=="burkina_faso"~"Burkina Faso", 
                             q8_country=="cameroon"~"Cameroon", q8_country=="canada"~"Canada", q8_country=="cayman_islands"~"Cayman Islands", q8_country=="cafrican_republic"~"Central African Republic", 
                             q8_country=="chad"~"Chad", q8_country=="chile"~"Chile", q8_country=="china"~"China", q8_country=="colombia"~"Colombia", q8_country=="comoros"~"Comoros", 
                             q8_country=="congo"~"Congo", q8_country=="cook_islands"~"Cook Islands", q8_country=="costa_rica"~"Costa Rica", q8_country=="côte_divoire"~"Côte d'Ivoire", 
                             q8_country=="croatia"~"Croatia", q8_country=="cuba"~"Cuba", q8_country=="curaçao"~"Curaçao", q8_country=="cyprus"~"Cyprus", q8_country=="czech_republic"~"Czech Republic", 
                             q8_country=="dem_peoples_rep_of_korea"~"Dem. People's Republic of Korea", q8_country=="democratic_republic_of_the_congo"~"Democratic Republic of the Congo", 
                             q8_country=="denmark"~"Denmark", q8_country=="djibouti"~"Djibouti", q8_country=="dominica"~"Dominica", q8_country=="dominican_republic"~"Dominican Republic", 
                             q8_country=="ecuador"~"Ecuador", q8_country=="egypt"~"Egypt", q8_country=="el_salvador"~"El Salvador", q8_country=="equatorial_guinea"~"Equatorial Guinea", 
                             q8_country=="eritrea"~"Eritrea", q8_country=="estonia"~"Estonia", q8_country=="eswatini"~"Eswatini", q8_country=="ethiopia"~"Ethiopia", q8_country=="fiji"~"Fiji", 
                             q8_country=="finland"~"Finland", q8_country=="french_guiana"~"French Guiana", q8_country=="france"~"France", q8_country=="french_polynesia"~"French Polynesia", 
                             q8_country=="gabon"~"Gabon", q8_country=="gambia"~"Gambia", q8_country=="germany"~"Germany", q8_country=="ghana"~"Ghana", q8_country=="greece"~"Greece", 
                             q8_country=="greenland"~"Greenland", q8_country=="grenada"~"Grenada", q8_country=="guadeloupe"~"Guadeloupe", q8_country=="guam"~"Guam", q8_country=="guatemala"~"Guatemala", 
                             q8_country=="guinea"~"Guinea", q8_country=="guinea_bissau"~"Guinea-Bissau", q8_country=="guyana"~"Guyana", q8_country=="haiti"~"Haiti", q8_country=="honduras"~"Honduras", 
                             q8_country=="hong_kong"~"Hong Kong", q8_country=="hungary"~"Hungary", q8_country=="iceland"~"Iceland", q8_country=="india"~"India", q8_country=="indonesia"~"Indonesia", 
                             q8_country=="iran"~"Iran", q8_country=="iraq"~"Iraq", q8_country=="ireland"~"Ireland", q8_country=="israel"~"Israel", q8_country=="italy"~"Italy", 
                             q8_country=="jamaica"~"Jamaica", q8_country=="japan"~"Japan", q8_country=="jordan"~"Jordan", q8_country=="kazakhstan"~"Kazakhstan", q8_country=="kenya"~"Kenya", 
                             q8_country=="kiribati"~"Kiribati", q8_country=="kuwait"~"Kuwait", q8_country=="kyrgyzstan"~"Kyrgyzstan", 
                             q8_country=="lao_peoples_democratic_republic"~"Lao People's Democratic Republic", q8_country=="latvia"~"Latvia", q8_country=="lebanon"~"Lebanon", q8_country=="lesotho"~"Lesotho", 
                             q8_country=="liberia"~"Liberia", q8_country=="libya"~"Libya", q8_country=="lithuania"~"Lithuania", q8_country=="liechtenstein"~"Liechtenstein", q8_country=="luxembourg"~"Luxembourg", 
                             q8_country=="macao"~"Macao", q8_country=="madagascar"~"Madagascar", q8_country=="malawi"~"Malawi", q8_country=="malaysia"~"Malaysia", q8_country=="maldives"~"Maldives", 
                             q8_country=="mali"~"Mali", q8_country==""~"Marshall", q8_country=="malta"~"Malta", q8_country==","~"=", q8_country=="martinique"~"Martinique", q8_country=="mauritania"~"Mauritania", 
                             q8_country=="mauritius"~"Mauritius", q8_country=="mayotte"~"Mayotte", q8_country=="mexico"~"Mexico", q8_country=="micronesia_fed_states_of"~"Micronesia (Fed. States of)", 
                             q8_country=="monaco"~"Monaco", q8_country=="mongolia"~"Mongolia", q8_country=="montenegro"~"Montenegro", q8_country=="montserrat"~"Montserrat", q8_country=="morocco"~"Morocco", 
                             q8_country=="mozambique"~"Mozambique", q8_country=="myanmar"~"Myanmar", q8_country=="namibia"~"Namibia", q8_country=="nauru"~"Nauru", q8_country=="nepal"~"Nepal", 
                             q8_country=="netherlands"~"Netherlands", q8_country=="new_caledonia"~"New Caledonia", q8_country=="new_zealand"~"New Zealand", q8_country=="nicaragua"~"Nicaragua", 
                             q8_country=="niger"~"Niger", q8_country=="nigeria"~"Nigeria", q8_country=="niue"~"Niue", q8_country=="north_macedonia"~"North Macedonia", 
                             q8_country=="northern_mariana_islands"~"Northern Mariana Islands", q8_country=="norway"~"Norway", q8_country=="oman"~"Oman", q8_country=="pakistan"~"Pakistan", 
                             q8_country=="palau"~"Palau", q8_country=="palestine"~"Palestine", q8_country=="panama"~"Panama", q8_country=="paraguay"~"Paraguay", q8_country=="papua_new_guinea"~"Papua New Guinea", 
                             q8_country=="peru"~"Peru", q8_country=="philippines"~"Philippines", q8_country=="poland"~"Poland", q8_country=="portugal"~"Portugal", q8_country=="puerto_rico"~"Puerto Rico", 
                             q8_country=="qatar"~"Qatar", q8_country=="republic_of_korea"~"Republic of Korea", q8_country=="republic_of_moldova"~"Republic of Moldova", q8_country=="reunion"~"Réunion", 
                             q8_country=="romania"~"Romania", q8_country=="rwanda"~"Rwanda", q8_country=="russia"~"Russia", q8_country=="saint_barthelemy"~"Saint Barthélemy", 
                             q8_country=="saint_helena"~"Saint Helena", q8_country=="saint_kitts_and_nevis"~"Saint Kitts and Nevis", q8_country=="saint_lucia"~"Saint Lucia",
                             q8_country=="saint_vincent_and_the_grenadines"~"Saint Vincent and the Grenadines", q8_country=="saint_martin_french_part"~"Saint Martin (French part)", 
                             q8_country=="samoa"~"Samoa", q8_country=="san_marino"~"San Marino", q8_country=="sao_tome_and_principe"~"Sao Tome and Principe", q8_country=="saudi_arabia"~"Saudi Arabia", 
                             q8_country=="senegal"~"Senegal", q8_country=="serbia"~"Serbia", q8_country=="sierra_leone"~"Sierra Leone", q8_country=="seychelles"~"Seychelles", q8_country=="singapore"~"Singapore", 
                             q8_country=="sint_maarten_dutch_part"~"Sint Maarten (Dutch part)", q8_country=="slovakia"~"Slovakia", q8_country=="slovenia"~"Slovenia", q8_country=="solomon_islands"~"Solomon Islands",
                             q8_country=="somalia"~"Somalia", q8_country=="south_sudan"~"South Sudan", q8_country=="south_africa"~"South Africa", q8_country=="sri_lanka"~"Sri Lanka", q8_country=="spain"~"Spain",
                             q8_country=="sudan"~"Sudan", q8_country=="suriname"~"Suriname", q8_country=="sweden"~"Sweden", q8_country=="switzerland"~"Switzerland",
                             q8_country=="syrian_arab_republic"~"Syrian Arab Republic", q8_country=="taiwan"~"Taiwan", q8_country=="tajikistan"~"Tajikistan", q8_country=="tanzania"~"Tanzania", 
                             q8_country=="thailand"~"Thailand", q8_country=="timor_leste"~"Timor-Leste", q8_country=="togo"~"Togo", q8_country=="tokelau"~"Tokelau", q8_country=="tonga"~"Tonga", 
                             q8_country=="trinidad_and_tobago"~"Trinidad and Tobago", q8_country=="tunisia"~"Tunisia", q8_country=="turkey"~"Turkey", q8_country=="turkmenistan"~"Turkmenistan",
                             q8_country=="turks_and_caicos_islands"~"Turks and Caicos Islands", q8_country=="tuvalu"~"Tuvalu", q8_country=="uganda"~"Uganda", q8_country=="ukraine"~"Ukraine", 
                             q8_country=="united_arab_emirates"~"United Arab Emirates", q8_country=="united_kingdom"~"United Kingdom", q8_country=="united_states_of_america"~"United States of America", 
                             q8_country=="uruguay"~"Uruguay", q8_country=="uzbekistan"~"Uzbekistan", q8_country=="vanuatu"~"Vanuatu", 
                             q8_country=="venezuela_bolivarian_republic_of"~"Venezuela (Bolivarian Republic of)", q8_country=="viet_nam"~"Viet Nam", 
                             q8_country=="wallis_and_futuna_islands"~"Wallis and Futuna Islands", q8_country=="western_sahara"~"Western Sahara", q8_country=="yemen"~"Yemen", q8_country=="zambia"~"Zambia", 
                             q8_country=="zimbabwe"~"Zimbabwe", q8_country=="country_other2"~"Other Central America & Mexico", q8_country=="country_other1"~"Other South America", 
                             q8_country=="country_other3"~"Other North America", q8_country=="country_other4"~"Other Caribbean", q8_country=="country_other5"~"Other South Asia",
                             q8_country=="country_other6"~"Other Southeast Asia", q8_country=="country_other7"~"Other East Asia", q8_country=="country_other8"~"Other The Pacific",
                             q8_country=="country_other9"~"Other South West Asia/Middle East", q8_country=="country_other10"~"Other Central Asia & Caucasus", q8_country=="country_other11"~"Other Eastern Europe, Southeast Europe and Central Europe",
                             q8_country=="country_other13"~"Other Western Europe", q8_country=="country_other14"~"Other West Africa", q8_country=="country_other15"~"Other East Africa",
                             q8_country=="country_other16"~"Other Southern Africa", q8_country=="country_other17"~"Other Central Africa", q8_country=="country_other18"~"Other North Africa"))


#recodigico q42
witm <- witm %>%
  mutate(q42_aspirational_budget = case_when(
    q42_aspirational_budget == "zero_budget" ~ "(a) Zero budget",
    q42_aspirational_budget == "<5000" ~ "(b) < 5,000",
    q42_aspirational_budget == "<10000" ~ "(c) 5,001 - 10,000",
    q42_aspirational_budget == "<30000" ~ "(d) 10,001 - 30,000",
    q42_aspirational_budget == "<50000" ~ "(e) 30,001 - 50,000",
    q42_aspirational_budget == "<100000" ~ "(f) 50,001 - 100,000",
    q42_aspirational_budget == "<250000" | q42_aspirational_budget == "<500000" ~ "(g) 100,001 - 500,000",
    q42_aspirational_budget == "<1000000" ~ "(h) 500,001 - 1,000,000",
    q42_aspirational_budget == "between_2m_and_4m_usd" | q42_aspirational_budget == "greater_than_4m_usd" ~ "(i) > 1,000,000",
    TRUE ~ NA_character_
  ))


#guardo base con nuevas variables
write.csv2(witm, file = "Data/WITM_FINAL_10102024.csv", row.names = FALSE)

write.xlsx(witm, file = "Data/WITM_FINAL_10102024.xlsx", sheetName="db")
