#Blackie methods comparison
#June 12 2025
#Jocelyn Heywood 

# Load Packages  ----
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(ggeffects)
library(brglm)                                                                                                                 
library(visreg)
library(RColorBrewer)

# Load Data ----
setwd("C:/Users/jocel/Desktop/Honours/Jocelyn_Heywood_Honours")
methods <- read_csv("USRA/blackie_methods_day.csv")


# Manipulating Data ----

methods1 <- methods %>%
  #changing dist to distance from the water
  mutate(dist_from_water = case_when(
    transect == 3 ~ (dist - 159) * (-1),
    transect == 2 ~ (dist - 183) * (-1),
    transect == 1 ~ (dist - 174) * (-1),
    TRUE ~ as.numeric(127- y_m)),
    # distance from shore
    dist_from_high_tide = ifelse( 
      method == "Kieran", y_m + 56, dist),
    # now calc density
    density = snail_number / quad_m2,
    #naming kieran's data transect 4
    transect = ifelse(
      method == "Kieran", 4, transect),
    transect = as.factor(transect)) 

isa <- methods1 %>%
  filter(method == "Isa",
         side == "R") %>%
  mutate( method = "Isa_half")


em_double <- methods1 %>%
  filter(method == "Em" | method == "Owen") %>%
  mutate(method = "em_double")

methods2 <-rbind(isa, methods1, em_double)

#Plotting methods 1----


#distance from water plot 
ggplot(methods1,
       aes(x = dist_from_water,
           y = density,
           color = transect)) +
  theme_classic() +
  geom_jitter(alpha = 1) +
  #separating all of the transects per method
  facet_wrap(~ transect + method, ncol = 3) +
  theme(legend.position = "null")

#distance from water plot not faceted
ggplot(methods1,
       aes(x = dist_from_water,
           y = density,
           color = transect)) +
  theme_classic() +
  geom_jitter(alpha = 1) 

#distance from high tide line plot 
ggplot(methods1,
       aes(x = dist_from_high_tide,
           y = density,
           color = transect)) +
  theme_classic() +
  geom_jitter(alpha = 1) +
  #separating all of the transects per method
  facet_wrap(~ transect + method, ncol = 3) +
  theme(legend.position = "null")

#distance from high tide line not faceted
ggplot(methods1,
       aes(x = dist_from_high_tide,
           y = density,
           color = transect)) +
  theme_classic() +
  geom_jitter(alpha = 1) 


#density faceted by method from water - show this 
ggplot(methods1,
       aes(x = dist_from_water,
           y = density)) +
  theme_classic() +
  geom_jitter(alpha = 1, aes(color = transect)) +
  #separating all of the transects per method
  facet_wrap(~ method, ncol = 4) +
  geom_smooth(method = lm)

#density faceted by method from high tide - show this
ggplot(methods1,
       aes(x = dist_from_high_tide,
           y = density)) +
  theme_classic() +
  geom_jitter(alpha = 1, aes(color = transect)) +
  #separating all of the transects per method
  facet_wrap(~ method, ncol = 4) +
  geom_smooth(method = lm)

#just method separation no difference of transect 
ggplot(methods1,
       aes(x = dist_from_water,
           y = density)) +
  theme_classic() +
  geom_jitter(alpha = 1) +
  geom_smooth(method = lm) +
  #separating all of the transects per method
  facet_wrap(~ method, ncol = 2)


#this is showing density mean per method - show this - dot whisker :D
ggplot(data = methods1, 
       aes(x = method, y = density, color = method)) + 
  geom_jitter(aes(y = density), width = 0.1, alpha = 0.4) +
  stat_summary(fun = mean, size = 1) +
  stat_summary(geom = "errorbar", width = 0.3, linewidth = 0.8) +
  theme_classic() +
  theme(legend.position = "null")

#Plotting methods 2----

#distance from water plot
ggplot(methods2,
       aes(x = dist_from_water,
           y = density,
           color = transect)) +
  theme_classic() +
  geom_jitter(alpha = 1) +
  #separating all of the transects per method
  facet_wrap(~ transect + method, ncol = 5) +
  theme(legend.position = "null")

#distance from high tide plot
ggplot(methods2,
       aes(x = dist_from_high_tide,
           y = density,
           color = transect)) +
  theme_classic() +
  geom_jitter(alpha = 1) +
  #separating all of the transects per method
  facet_wrap(~ transect + method, ncol = 5) +
  theme(legend.position = "null")


#density faceted by method from water - show this 
ggplot(methods2,
       aes(x = dist_from_water,
           y = density)) +
  theme_classic() +
  geom_jitter(alpha = 1, aes(color = transect)) +
  #separating all of the transects per method
  facet_wrap(~ method, ncol = 2) +
  geom_smooth(method = lm)

#density faceted by method from high tide - show this
ggplot(methods2,
       aes(x = dist_from_high_tide,
           y = density)) +
  theme_classic() +
  geom_jitter(alpha = 1, aes(color = transect)) +
  #separating all of the transects per method
  facet_wrap(~ method, ncol = 2) +
  geom_smooth(method = lm)

#just method separation no difference of transect 
ggplot(methods2,
       aes(x = dist_from_water,
           y = density)) +
  theme_classic() +
  geom_jitter(alpha = 1) +
  geom_smooth(method = lm) +
  #separating all of the transects per method
  facet_wrap(~ method, ncol = 2)


#this is showing density mean per method - show this - dot whisker :D
ggplot(data = methods2, 
       aes(x = method, y = density, color = method)) + 
  geom_jitter(aes(y = density), width = 0.1, alpha = 0.4) +
  stat_summary(fun = mean, size = 1) +
  stat_summary(geom = "errorbar", width = 0.3, linewidth = 0.8) +
  theme_classic() +
  theme(legend.position = "null")

#Em did this - comparing em and owen's methods 
methods2 %>%
  filter(method == "Em" | method == "Owen") %>%
  ggplot(aes(x = dist_from_high_tide, y = density, color = method)) + 
  geom_jitter() +
  geom_smooth(method = lm) +
  theme_classic()

d <- methods1 %>% count(method)

#Model time ----

hist(methods1$density)

#methods 1

#distance from low tide (water) model 
mod1 <- glmmTMB(density ~ method*dist_from_water + (1|transect), 
                family = tweedie(link = "log"),
                data = methods1)
plot(simulateResiduals(mod1))
summary(mod1)

#comparing all together emmeans 
#emmeans::emtrends(model, pairwise ~ categorical_variable, var = "continuous_variable")

emmeans::emtrends(mod1, pairwise ~ method, var = "dist_from_water")



#distance from high tide model - more interesting - show this 
mod2 <- glmmTMB(density ~ method*dist_from_high_tide +(1|transect),
               family = tweedie(link = "log"),
               data = methods1)
plot(simulateResiduals(mod2))
summary(mod2)

emmeans::emtrends(mod2, pairwise ~ method, var = "dist_from_high_tide")

#model without distance (total avg snails on beach)
mod3 <- glmmTMB(density ~ method + (1|transect), 
                family = tweedie(link = "log"),
                data = methods1)
plot(simulateResiduals(mod3))
summary(mod3)

#methods 2

#distance from low tide (water) model 
mod1 <- glmmTMB(density ~ method*dist_from_water + (1|transect), 
                family = tweedie(link = "log"),
                data = methods2)
plot(simulateResiduals(mod1))
summary(mod1)

#comparing all together emmeans 
#emmeans::emtrends(model, pairwise ~ categorical_variable, var = "continuous_variable")

emmeans::emtrends(mod1, pairwise ~ method, var = "dist_from_water")



#distance from high tide model - more interesting - show this 
mod2 <- glmmTMB(density ~ method*dist_from_high_tide +(1|transect),
                family = tweedie(link = "log"),
                data = methods2)
plot(simulateResiduals(mod2))
summary(mod2)

emmeans::emtrends(mod2, pairwise ~ method, var = "dist_from_high_tide")

#model without distance (total avg snails on beach)
mod3 <- glmmTMB(density ~ method + (1|transect), 
                family = tweedie(link = "log"),
                data = methods2)
plot(simulateResiduals(mod3))
summary(mod3)
