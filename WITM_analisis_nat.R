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
library(xlsx)

rm(list = ls())

witm <- read.csv2("Data/WITM_csv_last.csv")

source(file="WITM_variables.R") 
