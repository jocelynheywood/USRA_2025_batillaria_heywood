#Combining combined_previous_data and combined_data_2025
#Jocelyn Heywood
#2025-08-07

#load packages ----
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

#load data ----
present_data <- read_csv("output_data/combined_data_2025.csv")
past_data <- read_csv("output_data/combined_previous_data.csv")

#combinig past and present batillaria data 
present_data1 <- present_data %>%
  mutate(quadrat_area_m2 = case_when(
    quadrat_size == "s" ~ 0.3*0.3,
    quadrat_size == "l" ~ 0.5*0.5),
    snail_density = snail_number/quadrat_area_m2,
    transect = case_when(
      transect == "l" ~ "1",
      transect == "m" ~ "2",
      transect == "r" ~ "3"),
    date = ymd(gsub("_","-", date))) %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_area_m2, snail_density)

past_data1 <- past_data %>%
  rename(vertical_dist = dist_from_shore_m) %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_area_m2, snail_density)

past_present_combo <- rbind(past_data1, present_data1)

#saving past_present_combo as a csv file 
write.csv(past_present_combo, file = "output_data/past_present_combo.csv", row.names = FALSE)



