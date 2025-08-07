#USRA 2025
#Jocelyn Heywood
#Batillaria density project 

#load packages ----
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)

######THIS IS PREVIOUS DATA########

#load previous density data ----
setwd("C:/Users/jocel/Desktop/Honours/Jocelyn_Heywood_Honours")
em_data <- read_csv("em_batillaria_data/em_transect_data.csv")
em_size_data <- read_csv("em_batillaria_data/em_batillaria_size_prev_data/em_batillaria_size_prev_data.csv")
isabelle_data <- read_csv("isabelle_batillaria_data/isabelle_batillaria_data.csv")
owen_data <- read_csv("owen_batillaria_data/owen_batillaria_data.csv")
site_info <- read_csv("site_info_data1.csv")

#manipulate previous site info ----
site_info1 <- site_info %>%
  rename(site = sites,
         date = dates,
         method = methods) %>%
  mutate(owner = case_when(owner == "Em" ~ "em",
                           owner == "Isabelle" ~ "isabelle",
                           owner == "Owen" ~ "owen",
                           owner == "Kieran" ~ "kieran",
                           TRUE ~ as.character(NA))) %>%
  select(-coordinates)
# Manipulate em_data ----
em_data1 <- em_data %>%
  rename(site = beach, 
         snail_number = number_snails,                         
         dist_from_shore_m = position) %>%
  mutate(method = "A",
         quadrat_area_m2 = case_when(quadrat_size == "L" ~ 0.2401,
                                     quadrat_size == "S" ~ 0.0961,
                                     TRUE ~ as.numeric(NA)),
         owner = "em",
         snail_density = snail_number / quadrat_area_m2) %>%     
  select(-c(ph, prev, substrate, quadrat_size)) %>%              
  left_join(site_info1 %>% filter(owner == "em") %>%
              select(site, date), 
            by = "site") %>%
  left_join(site_info1 %>% select(site, owner, lat, long), by = c("site", "owner")) %>%  
  select(site, date, lat, long, method, owner, snail_number, quadrat_area_m2, snail_density, transect, quadrat, dist_from_shore_m) 

# Manipulate em_size_data ----
em_size_data1 <- em_size_data %>%
  rename(site = beach,                                           #renaming columns
         dist_from_shore_m = position) %>%
  select(-infected, -Infection) %>%                              #getting rid of columns em had
  group_by(site, transect, quadrat) %>%                          #groups the data by the combination of site, transect, and quadrat. all subsequent operations like mutate or distinct are performed within each group 
  summarise(size_mm_avg = mean(size)) %>%
  ungroup()



# Adding size_mm_avg from em_size_data1 to em_data1 ----

em_data2 <- em_data1 %>%
  left_join(em_size_data1, 
            by = c("site", "transect", "quadrat"))

#column name organizing  ----
names <- colnames(em_data2)

# Manipulate isabelle_data ----
isabelle_data1 <- isabelle_data %>%
  rename(transect = Transect,
         snail_number = total_num,
         dist_from_shore_m = distance_from_marsh,
         size = length) %>%
  select(-barnacled, -num_barnacled, -`Collect_date (dd.mm.yy)`,-distance_from_water) %>%
  mutate(site = "blackie",
         method = "B",
         owner = "isabelle",
         quadrat_area_m2 = 0.04,
         transect = case_when(transect == "IMC" ~ 1,
                              transect == "KK" ~ 2,
                              transect == "AG" ~ 3,
                              TRUE ~ as.numeric(NA))) %>% 
  left_join(site_info1 %>% select(site, date, owner, lat, long), by = c("site", "owner"))

isabelle_add_quadrat <- isabelle_data1 %>%
  count(transect, dist_from_shore_m) %>%
  group_by(transect) %>%
  mutate(quadrat = 1:length(transect))

isabelle_data2 <- isabelle_data1 %>%
  left_join(isabelle_add_quadrat, by = c("transect", "dist_from_shore_m") ) %>%
  select(-n) 

isabelle_size_data <- isabelle_data2 %>%
  group_by(transect, quadrat, dist_from_shore_m) %>%
  summarise(size_mm_avg = mean(size)) %>%
  ungroup()

isabelle_data3 <- isabelle_data2%>%
  left_join(isabelle_size_data, by = c("transect", "quadrat", "dist_from_shore_m")) %>%
  select(-size) %>%
  mutate(snail_density = snail_number / quadrat_area_m2) %>%
  select(names)


# Manipulate owen_data ----
owen_data1 <- owen_data %>%
  rename(site = Site,
         transect = Transect,
         dist_from_shore_m = Point,
         size = `Snail Size (mm)`,
         snail_number = Total) %>%
  select(-`Range (m)`, -`Snail ID`) %>%
  mutate(site = case_when(site == "Blackie Spit East Side" ~ "blackie",
                          site == "Crescent Beach West Side" ~ "crescent"),
         owner = "owen",
         method = "C",
         quadrat_area_m2 = 0.0625) %>%
  left_join(site_info1 %>% select(site, date, owner, lat, long), by = c("site", "owner"))

owen_add_quadrat <- owen_data1 %>%
  count(transect, dist_from_shore_m) %>%
  group_by(transect) %>%
  mutate(quadrat = 1:length(transect))

owen_data2 <- owen_data1 %>%
  left_join(owen_add_quadrat, by = c("transect", "dist_from_shore_m")) %>%
  select(-n) 

owen_size_data <- owen_data2 %>%
  group_by(transect, quadrat, dist_from_shore_m) %>%
  summarise(size_mm_avg = mean(size)) %>%
  ungroup()

owen_data3 <- owen_data2 %>%
  left_join(owen_size_data, by = c("transect", "quadrat", "dist_from_shore_m")) %>%
  mutate(snail_density = snail_number / quadrat_area_m2) %>%
  select(names)


# Combining data sets ----

combined_previous_data <-rbind(em_data2, isabelle_data3, owen_data3)

######PREVIOUS DATA DONE######

#load density data 2025 ----
setwd("C:/Users/jocel/Desktop/Honours/Jocelyn_Heywood_Honours/USRA/USRA_data")
blackie <- read_csv("2025_06_24_blackie_batillaria.csv")
crescent <- read_csv("2025_06_25_crescent_batillaria.csv")
centennial <- read_csv("2025_06_26_centennial_batillaria.csv")
hornby <- read_csv("2025_07_09_hornby_batillaria.csv")
deep_bay <- read_csv("2025_07_10_deep_bay_batillaria.csv")
rathtrevor <- read_csv("2025_07_11_rathtrevor_batillaria.csv")
saltspring <- read_csv("") #laser level is not done yet 
page_lagoon <- read_csv("") #snail number is not done yet 
valdes <- read_csv("") #laser level is not done yet 
ladysmith <- read_csv("") #snail number is not done yet 





#manipulate density data from 2025 ----
blackie_2025 <- blackie %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)

crescent_2025 <- crescent %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)

centennial_2025 <- centennial %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)

hornby_2025 <- hornby %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)

deep_bay_2025 <- deep_bay %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)


#combine density data from 2025 ----

combined_data_2025 <-rbind(blackie_2025, crescent_2025, centennial_2025, hornby_2025, deep_bay_2025)

#manipulating combined previous density data 

combined_previous_data1 <- combined_previous_data %>%
  select(site, date, transect, quadrat, dist_from_shore_m, snail_number, quadrat_area_m2)
