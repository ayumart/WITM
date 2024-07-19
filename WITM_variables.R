


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


