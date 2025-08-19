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

#Lubridate within a dataframe
urchin_counts <- tribble(
  ~date, ~species, ~size_mm,
  "10/3/2020", "purple", 55,
  "10/4/2020", "red", 48,
  "11/17/2020", "red", 67
)

#DATE PIECES AS NEW COLUMNS
urchin_counts_ymd <- urchin_counts %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date))
    # And then we could use group_by() to find different summary values by group, for example. 

#Find the duration of time 
day_1 <- lubridate::ymd("2020-01-06")
day_2 <- lubridate::ymd("2020-05-18")
day_3 <- lubridate::ymd("2020-05-19")

  # Create a time interval
time_interval <- interval(day_1, day_2)

  # Check the length in weeks
time_length(time_interval, "week")

  # Check the length in years
time_length(time_interval, "year")

# Use str_detect() to detect a string pattern
# Returns TRUE or FALSE based on whether the pattern is or is not detected.

my_string <- "Teddy loves eating salmon and socks"

my_string %>% 
  stringr::str_detect('love')

my_string %>% 
  stringr::str_detect('pup')

my_string <- c("burrito", "fish taco", "Taco salad")

# Does the vector element contain the pattern "fish"?
my_string %>% stringr::str_detect("fish")

# powerful in combination with dplyr functions

starwars %>% 
  filter(stringr::str_detect(name, 'Skywalker'))

firewalkers <- starwars %>% 
  mutate(name = stringr::str_replace(name, pattern = "Sky", replacement = "Fire"))

head(firewalkers)

#cleaning up white space
feedback <- c("I ate  some  nachos", "Wednesday Morning   ")
    #remove the leading, trailing and duplicate spaces
stringr::str_squish(feedback)

    #remove just leading and trailing spaces
stringr::str_trim(feedback)

#Convert cases
stringr::str_to_lower(feedback) #janitor::clean_names() alternative
stringr::str_to_upper(feedback)
stringr::str_to_sentence(feedback)
stringr::str_to_title(feedback)

#Count the number of matches in a string
stringr::str_count(feedback, pattern = 'nachos')
