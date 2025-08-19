#Clear environment
rm(list = ls())

#Attach packages
library(palmerpenguins)
library(tidyverse)
library(lubridate) #work with dates

#Data wrangling refresher

#Show only penguins at Briscoe and Dream Islands
#Remove the year and sex variables
#Add a new column called 'body_mass_kg' with penguin mass converted from g to kg
#Rename the island variable to location

unique(penguins$island)
penguins_sub <- penguins %>% 
  filter(island == c('Biscoe', 'Dream')) %>% 
  select(-c(year, sex)) %>% 
  mutate('body_mass_kg' = body_mass_g / 100) %>% 
  rename(location = island)

# Only Adelie penguins
# Remove NA observations in flipper length column
# Group the data by sex
# Find mean, sd, sample size of flipper lengths
penguins_sub2 <- penguins %>% 
  filter(species == 'Adelie') %>% 
  filter(!is.na(flipper_length_mm)) %>% #remove rows that are NA
  drop_na(sex) %>% #remove observations that are NA
  group_by(sex) %>% 
  summarise(mean_length = mean(flipper_length_mm),
            sd_length = sd(flipper_length_mm),
            sample_size = n())