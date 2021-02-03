# set up
# load packages 
library(here)
library(tidyverse)
library(readxl)
library(lubridate)

# read data 
brfss <- read_excel("data/raw/BRFSS_Screening.xlsx")

####
# start here
brfss_2019 <- read_excel("data/raw/BRFSS_Screening.xlsx", sheet = 2)
####

# inspect
glimpse(brfss)

# gather by county 
# Pima county 
brfss_pima <- brfss %>%
  select(`...1`:`...5`) %>%
  rename("label" = `...1`, 
         "all" = `Pima`, 
         "nhw" = `...3`, 
         "hispanic" = `...4`, 
         "aian" = `...5`) %>%
  slice(2:34) %>%
  mutate(county = "Pima")

# Pinal 
brfss_pinal <- brfss %>%
  select(`...1`, `Pinal`:`...9`) %>%
  rename("label" = `...1`, 
         "all" = `Pinal`, 
         "nhw" = `...7`, 
         "hispanic" = `...8`, 
         "aian" = `...9`) %>%
  slice(2:34) %>%
  mutate(county = "Pinal")

# Cochise & Santa Cruz 
brfss_cochise <- brfss %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee`:`...13`) %>%
  rename("label" = `...1`, 
         "all" = `Santa Cruz, Cochise, Graham, Greenlee`, 
         "nhw" = `...11`, 
         "hispanic" = `...12`, 
         "aian" = `...13`) %>%
  slice(2:34) %>%
  mutate(county = "Santa Cruz, Cochise, Graham, Greenlee")

# Yuma 
brfss_yuma <- brfss %>%
  select(`...1`, `Yuma, La Paz, Mohave`:`...17`) %>%
  rename("label" = `...1`, 
         "all" = `Yuma, La Paz, Mohave`, 
         "nhw" = `...15`, 
         "hispanic" = `...16`, 
         "aian" = `...17`) %>%
  slice(2:34) %>%
  mutate(county = "Yuma, La Paz, Mohave")

# combine all together
brfss <- brfss_cochise %>%
  full_join(brfss_pima) %>%
  full_join(brfss_pinal) %>%
  full_join(brfss_yuma)

brfss <- brfss %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
               ) %>%
  mutate(value = as.numeric(value),
         year = 2016)

# # correct values from character to numeric
# brfss <- mutate(brfss, all = as.numeric(all))
# brfss <- mutate(brfss, nhw = as.numeric(nhw))
# brfss <- mutate(brfss, hispanic = as.numeric(hispanic))
# brfss <- mutate(brfss, aian = as.numeric(aian))

# inspect
glimpse(brfss)
distinct(brfss, county)
distinct(brfss, race)

# isolate the variables 
variables <- distinct(brfss, label)

# exploratory
brfss %>%
  filter(label == "Had a mammogram") %>%
  ggplot(mapping = aes(x = county, y = value, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "",
       y = "County",
       fill = "Race / Ethnicity",
       title = "Breast Cancer Screening",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS")
