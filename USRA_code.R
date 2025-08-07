#USRA 2025
#Jocelyn Heywood
#Batillaria density project 

#load packages ----
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)

#load density data ----
setwd("C:/Users/jocel/Desktop/Honours/Jocelyn_Heywood_Honours/USRA/USRA_data")
blackie <- read_csv("2025_06_24_blackie_batillaria.csv")
crescent <- read_csv("2025_06_25_crescent_batillaria.csv")
centennial <- read_csv("2025_06_26_centennial_batillaria.csv")
hornby <- read_csv("2025_07_09_hornby_batillaria.csv")
deep_bay <- read_csv("")
rathtrevor <- read_csv("")
saltspring <- read_csv("")
page_lagoon <- read_csv("")
valdes <- read_csv("")
ladysmith <- read_csv("")

#load max density data ----
blackie_max_density <- read_csv("max_density_blackie_2025.csv")
crescent_max_density <- read_csv("max_density_crescent_2025.csv")
#####centennial_max_density ????
hornby_max_density <- read_csv("max_density_hornby_2025.csv")
deep_bay_max_density <- read_csv("")
rathtrevor_max_density <- read_csv("")
saltspring_max_density <- read_csv("")
page_lagoon_max_density <- read_csv("")
valdes_max_density <- read_csv("")
ladysmith_max_density <- read_csv("")

#manipulate data ----
