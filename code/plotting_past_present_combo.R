#plotting past_present_combo
#Jocelyn Heywood
#2025-08-14

#load packages ----
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

#load data ----
past_present_combo <- read_csv("output_data/past_present_combo.csv")

#adding a year column ----
past_present_combo1 <- past_present_combo %>%
  mutate(year = year(date))

#changing rath to rathtrever ----
past_present_combo1 <- past_present_combo1 %>%
  mutate(site = case_when(
    site == "blackie" ~ "blackie_spit",
    site == "deep" ~ "deep_bay",
    site == "rath" ~ "rathtrevor",
    site == "Valdes" ~ "valdes", TRUE ~ as.character(site)))

#dropping padilla, lund and centennial because they only have one time point ----
past_present_combo2 <- past_present_combo1 %>%
  filter(!site %in% c("centennial_beach", "padilla", "lund"), !is.na(site)) %>%
  filter(!year %in% c("2024"))

#getting rid of the NAs in Isabelle's blackie data because there were extra rows for size ----
past_present_combo3 <- past_present_combo2 %>%
  filter(!is.na(snail_number))

#adding a past and present column
past_present_combo4 <- past_present_combo3 %>%
  mutate(past_or_present = case_when(
    year == "2018"
  ))


#plotting vertical distance by snail density per year per site ----
ggplot(past_present_combo3,
       aes(x = vertical_dist, y = snail_density, color = as.factor(year))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ site) +
  labs(x = "vertical_distance",
       y = "snail density",
       color = "year") +
  theme_minimal()





