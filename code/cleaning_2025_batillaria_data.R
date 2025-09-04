#Cleaning 2025 batillaria data
#Jocelyn Heywood
#2025-08-07

#load packages ----
library(dplyr)
library(readr)

#load in combined_previous_data
historical_data <- read_csv("output_data/combined_previous_data.csv") #I made this in cleaning_historical_batillaria_data


#load density data 2025 ----
blackie <- read_csv("data/2025_06_24_blackie_batillaria.csv")
crescent <- read_csv("data/2025_06_25_crescent_batillaria.csv")
centennial <- read_csv("data/2025_06_26_centennial_batillaria.csv")
hornby <- read_csv("data/2025_07_09_hornby_batillaria.csv")
deep_bay <- read_csv("data/2025_07_10_deep_bay_batillaria.csv")
rathtrevor <- read_csv("data/2025_07_11_rathtrevor_batillaria.csv")
saltspring <- read_csv("data/2025_07_23_saltspring_batillaria.csv") 
page_lagoon <- read_csv("data/2025_07_24_page_batillaria.csv")  
valdes <- read_csv("data/2025_07_25_valdes_batillaria.csv")
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

rathtrevor_2025 <- rathtrevor %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)
  
saltspring_2025 <- saltspring %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)

page_lagoon_2025 <- page_lagoon %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)

valdes_2025 <- valdes %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)

ladysmith_2025 <- ladysmith %>%
  select(site, date, transect, quadrat, vertical_dist, snail_number, quadrat_size)


#combine density data from 2025 ----

combined_data_2025 <-rbind(blackie_2025, crescent_2025, centennial_2025, hornby_2025, deep_bay_2025, rathtrevor_2025, saltspring_2025, page_lagoon_2025, valdes_2025, ladysmith_2025)

#saving combined_data_2025 into a csv file to put in output_data

write.csv(combined_data_2025, file = "output_data/combined_data_2025.csv", row.names = FALSE)