


#open data and variables construction




#2024-05-23 08:37:41.178000-04:00
witm$date <-  as.Date(witm$end,'%Y-%m-%d')


witm$date_s <-  paste(format(as.Date(witm$date,format="%Y-%m-%d"), format = "%m"), format(as.Date(witm$date,format="%Y-%m-%d"), format = "%d"), sep ="-", collapse=NULL)




witm<-witm %>% 
  mutate(q9_year_formation_agrup=case_when(
    q9_year_formation>=2021 & q9_year_formation<=2023 ~  "(1) 2021-2023",
    q9_year_formation<2021 & q9_year_formation>2014 ~  "(2) From 2015 to 2020",
    q9_year_formation<2015 & q9_year_formation>2009 ~  "(3) From 2010 to 2014",
    q9_year_formation<2010 & q9_year_formation>1999 ~  "(4) From 2000 to 2009",
    q9_year_formation<=2000 & q9_year_formation>1945 ~  "(5) Before 2000",
    is.na(q9_year_formation) ~ NA,
    TRUE ~  NA)) 


witm<-witm %>%
  mutate(q10_budget_year_2021=case_when(
    q10_budget_year_2021=="zero_budget" ~  "(a) Zero budget",
    q10_budget_year_2021=="less_than_5000_usd" ~  "(b) <5000",
    q10_budget_year_2021=="between_5001_and_10000_usd" ~  "(c) 5001-10000",
    q10_budget_year_2021=="between_10001_and_30000_usd" ~  "(d) 10001-30000",
    q10_budget_year_2021=="between_30001_and_50000_usd" ~  "(e) 30001 -50000",
    q10_budget_year_2021=="between_50001_and_100000_usd" ~  "(f) 50001 - 100000",
    q10_budget_year_2021=="between_100001_and_250000_usd" ~  "(g) 100001 - 250000",
    q10_budget_year_2021=="between_250001_and_500000_usd" ~  "(h) 250001 - 500000",
    q10_budget_year_2021=="between_500001_and_1000000_usd" ~  "(i) 500001 - 1000000",
    q10_budget_year_2021=="between_1000001_and_2000000_usd" ~  "(j) 1000001 - 2000000",
    q10_budget_year_2021=="between_2000001_and_4000000_usd" ~  "(k) 2000001 - 4000000",
    q10_budget_year_2021=="greater_than_4000001_usd" ~  "(l) > 4000001",
    TRUE ~  NA))

test<-witm %>% 
  group_by(q10_budget_year_2021) %>% 
  summarise(n=n())
test



witm<-witm %>%
  mutate(q10_budget_year_2022=case_when(
    q10_budget_year_2022=="zero_budget" ~  "(a) Zero budget",
    q10_budget_year_2022=="less_than_5000_usd" ~  "(b) <5000",
    q10_budget_year_2022=="between_5001_and_10000_usd" ~  "(c) 5001-10000",
    q10_budget_year_2022=="between_10001_and_30000_usd" ~  "(d) 10001-30000",
    q10_budget_year_2022=="between_30001_and_50000_usd" ~  "(e) 30001 -50000",
    q10_budget_year_2022=="between_50001_and_100000_usd" ~  "(f) 50001 - 100000",
    q10_budget_year_2022=="between_100001_and_250000_usd" ~  "(g) 100001 - 250000",
    q10_budget_year_2022=="between_250001_and_500000_usd" ~  "(h) 250001 - 500000",
    q10_budget_year_2022=="between_500001_and_1000000_usd" ~  "(i) 500001 - 1000000",
    q10_budget_year_2022=="between_1000001_and_2000000_usd" ~  "(j) 1000001 - 2000000",
    q10_budget_year_2022=="between_2000001_and_4000000_usd" ~  "(k) 2000001 - 4000000",
    q10_budget_year_2022=="greater_than_4000001_usd" ~  "(l) > 4000001",
    TRUE ~  NA))

witm<-witm %>%
  mutate(q10_budget_year_2023=case_when(
    q10_budget_year_2023=="zero_budget" ~  "(a) Zero budget",
    q10_budget_year_2023=="less_than_5000_usd" ~  "(b) <5000",
    q10_budget_year_2023=="between_5001_and_10000_usd" ~  "(c) 5001-10000",
    q10_budget_year_2023=="between_10001_and_30000_usd" ~  "(d) 10001-30000",
    q10_budget_year_2023=="between_30001_and_50000_usd" ~  "(e) 30001 -50000",
    q10_budget_year_2023=="between_50001_and_100000_usd" ~  "(f) 50001 - 100000",
    q10_budget_year_2023=="between_100001_and_250000_usd" ~  "(g) 100001 - 250000",
    q10_budget_year_2023=="between_250001_and_500000_usd" ~  "(h) 250001 - 500000",
    q10_budget_year_2023=="between_500001_and_1000000_usd" ~  "(i) 500001 - 1000000",
    q10_budget_year_2023=="between_1000001_and_2000000_usd" ~  "(j) 1000001 - 2000000",
    q10_budget_year_2023=="between_2000001_and_4000000_usd" ~  "(k) 2000001 - 4000000",
    q10_budget_year_2023=="greater_than_4000001_usd" ~  "(l) > 4000001",
    TRUE ~  NA))





base<-witm %>% 
  filter(q0_consent=="yes")

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

base<- base %>%
  mutate(q4_awid_focus_no=case_when
         (q4_awid_focus!=1 ~ 1, TRUE ~0))  



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


##we will use this ranges
# 0 budget
# > 5000 USD 
# 5001 USD – 10,000 USD
# 10,000 – 30,000 USD
# 30,001 – 50,000 USD
# 50,0001 – 100,000 USD
# 100,001 – 500,001 USD
# 500,001 – $1,000,000 USD 
# 1,000,001+ USD


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


rm(witm)
