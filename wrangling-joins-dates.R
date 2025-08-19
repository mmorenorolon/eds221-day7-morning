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

#Interactive Session Animals and Sites dataframes
animals <- data.frame(
  stringsAsFactors = FALSE,
          location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

#MUTATING JOINS

#full_join() example
  #keeps all rows and adds all columns

library(dplyr)
full_join_example <- full_join(animals, sites)

#left_join()

left_join_example <- left_join(animals, sites)

#right_join()

right_join_example <- right_join(animals, sites)

#inner_join()

inner_join_example <- inner_join(animals, sites)

#FILTERING JOINS

#semi_join()

semi_join_example <- semi_join(animals, sites)

animals %>% 
  filter(location %in% sites$location)

#anti_join()

anti_join_example <- anti_join(animals, sites)

#Lubridate practice
my_date <- "03-12-1998"
lubridate::mdy(my_date)
  #"1998-03-12"

#new format
my_date <- "08-Jun-1974"
lubridate::dmy(my_date)
  #"1974-06-08"

#different format
my_date <- "19160518"
lubridate::ymd(my_date)
  #1916-05-18"

#Incorrect method
lubridate::mdy("1942-08-30")
  #Warning message:
  #All formats failed to parse. No formats found.

#date-times
library(lubridate)
time <- "2020-08-12 11:18"
time <- ymd_hm(time)
  #"2020-08-12 11:18:00 UTC"

 #convert to PDT
with_tz(time, tz = "America/Los_Angeles")
"2020-08-12 04:18:00 PDT"

#extract info from dates
week(time)
year(time)
day(time)
hour(time)


Sys.time #get current time