library(tidyverse)
library(mosaic)
library(GGally)
library(performance)

options(scipen = 999) # disable scientific notation

# import the data set
co2_kwh <- read_csv(here::here("data", "co2_kwh.csv"))

co2_kwh %>% 
  select(co2percap, per_capita_electricity, GDPpercap) %>% 
  ggpairs()

model1 <- lm(co2percap ~ per_capita_electricity, data=co2_kwh)
msummary(model1)

model2 <- lm(co2percap ~ per_capita_electricity + continent, data=co2_kwh)
msummary(model2)

model3 <- lm(co2percap ~ per_capita_electricity + continent + I(year-min(year)), data=co2_kwh)
msummary(model3)

model4 <- lm(co2percap ~ per_capita_electricity + continent + I(year-min(year)) + GDPpercap, data=co2_kwh)
msummary(model4)

check_model(model3)
check_model(model4)
