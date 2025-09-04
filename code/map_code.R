#Map
#Jocelyn Heywood



install.packages(c('tidyverse', 'sf', 'stars', 'rcartocolor'), dependencies = TRUE)
install.packages("ggrepel")

knitr::opts_knit$set(root.dir = "/Desktop/Honours/Jocelyn_Heywood_Honours/USRA/")
setwd("C:/Users/jocel/Desktop/Honours/Jocelyn_Heywood_Honours")


library(tidyverse)
library(sf)
library(stars)
library(rcartocolor)
library(ggplot2)
library(ggrepel)

site_data <- read_csv('USRA/site_info_data1.csv')

site_data <- site_data %>%
  select(-c(coordinates)) 

site_data_sf <- site_data %>%
  bind_rows(tibble(
    sites = "rocky",
    lat = 49.28151,
    long = -122.8497)) %>%
  mutate(sites = case_when(
    sites == "quadra" ~ "Quadra Island",
    sites == "lund" ~ "Lund",
    sites == "baynes" ~ "Baynes Sound",
    sites == "deep" ~ "Deep Bay",
    sites == "hornby" ~ "Hornby Island",
    sites == "rath" ~ "Rathtrevor",
    sites == "page" ~ "Page Lagoon",
    sites == "valdes" ~ "Valdes Island",
    sites == "ladysmith" ~ "Ladysmith",
    sites == "saltspring" ~ "Salt Spring Island",
    sites == "blackie" ~ "Blackie Spit",
    sites == "crescent" ~ "Crescent Beach",
    sites == "rocky" ~ "Rocky Point",
    TRUE ~ sites
  )) %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs = 3005) %>%
  group_by(geometry) %>%
  summarise(sites = paste(unique(sites), collapse = ",")) %>%
  filter(sites != "padilla")

site_data_sf


ggplot() + 
  geom_sf(data = site_data_sf, 
          colour = 'skyblue3')

ggplot() + 
  geom_sf(data = site_data_sf, 
          colour = 'skyblue3') +
  theme_classic()

coastline <- read_sf('coastline.gpkg') %>% 
  st_transform(crs = 3005)

ggplot() + 
  geom_sf(data = coastline, 
          fill = 'grey22') + 
  geom_sf(data = site_data_sf, 
          colour = 'skyblue3') +
  theme_classic()

site_data_bbox <- site_data_sf %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf()

site_data_bbox

coastline_subset <- st_intersection(coastline, site_data_bbox)

ggplot() + 
  geom_sf(data = coastline_subset, 
          fill = 'grey22') + 
  geom_sf(data = site_data_sf, 
          colour = 'skyblue3') +
  theme_classic()

site_data_plot <- site_data_sf %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_buffer(dist = 50000) %>% 
  st_bbox()

site_data_plot

coastline_subset <- st_intersection(coastline, 
                                    st_as_sf(st_as_sfc(site_data_plot)))
#this is the map 
ggplot() + 
  geom_sf(data = coastline_subset, 
          fill = 'lightblue') + 
  geom_sf(data = site_data_sf, 
          colour = 'red') + 
  coord_sf(xlim = c(site_data_plot[[1]], site_data_plot[[3]]),
           ylim = c(site_data_plot[[2]], site_data_plot[[4]]), 
           expand = FALSE) +
  theme_classic()

ggplot() + 
  geom_sf(data = coastline_subset, 
          fill = 'lightblue') + 
  geom_sf(data = site_data_sf, 
          colour = 'red') + 
  geom_text(data = site_data_sf,
            aes(label = sites, geometry = geometry),
            stat = "sf_coordinates",
            colour = "black",
            size = 3, 
            hjust = -0.1, 
            vjust = -0.5) +
  coord_sf(xlim = c(site_data_plot[[1]], site_data_plot[[3]]),
           ylim = c(site_data_plot[[2]], site_data_plot[[4]]), 
           expand = FALSE) +
  theme_classic()

ggplot() + 
  geom_sf(data = coastline_subset, fill = 'seagreen') + 
  geom_sf(data = site_data_sf, colour = 'red') + 
  geom_text_repel(data = site_data_sf, 
                  aes(label = sites, geometry = geometry), 
                  stat = "sf_coordinates", 
                  size = 3, 
                  fontface = "bold",
                  colour = "black") +
  coord_sf(xlim = c(site_data_plot[[1]], site_data_plot[[3]]),
           ylim = c(site_data_plot[[2]], site_data_plot[[4]]), 
           expand = FALSE) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "lightblue", colour = NA))+
  labs(x = NULL, y = NULL)

ggsave("snail_map.png", width = 9, height =8 )
