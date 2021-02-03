# set up
# load packages 
library(here)
library(tidyverse)
library(readxl)
library(lubridate)

# read data 
brfss_2016 <- read_excel("data/raw/BRFSS_Screening.xlsx")
brfss_2018 <- read_excel("data/raw/BRFSS_Screening.xlsx", sheet = 2)
####

# inspect
glimpse(brfss_2016)
glimpse(brfss_2018)

# gather by county 
# Pima county 
brfss_16_pima <- brfss_2016 %>%
  select(`...1`:`...5`) %>%
  rename("label" = `...1`, 
         "all" = `Pima`, 
         "nhw" = `...3`, 
         "hispanic" = `...4`, 
         "aian" = `...5`) %>%
  slice(2:34) %>%
  mutate(county = "Pima")

#2018
brfss_19_pima <- brfss_2018 %>%
  select(`...1`:`...5`) %>%
  rename("label" = `...1`, 
         "all" = `Pima`, 
         "nhw" = `...3`, 
         "hispanic" = `...4`, 
         "aian" = `...5`) %>%
  slice(2:34) %>%
  mutate(county = "Pima")

# Pinal 
brfss_16_pinal <- brfss_2016 %>%
  select(`...1`, `Pinal`:`...9`) %>%
  rename("label" = `...1`, 
         "all" = `Pinal`, 
         "nhw" = `...7`, 
         "hispanic" = `...8`, 
         "aian" = `...9`) %>%
  slice(2:34) %>%
  mutate(county = "Pinal")

# 2018
brfss_19_pinal <- brfss_2018 %>%
  select(`...1`, `Pinal`:`...9`) %>%
  rename("label" = `...1`, 
         "all" = `Pinal`, 
         "nhw" = `...7`, 
         "hispanic" = `...8`, 
         "aian" = `...9`) %>%
  slice(2:34) %>%
  mutate(county = "Pinal")

# Cochise & Santa Cruz 
brfss_16_cochise <- brfss_2016 %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee`:`...13`) %>%
  rename("label" = `...1`, 
         "all" = `Santa Cruz, Cochise, Graham, Greenlee`, 
         "nhw" = `...11`, 
         "hispanic" = `...12`, 
         "aian" = `...13`) %>%
  slice(2:34) %>%
  mutate(county = "Santa Cruz, Cochise, Graham, Greenlee")

# 2018
brfss_19_cochise <- brfss_2018 %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee`:`...13`) %>%
  rename("label" = `...1`, 
         "all" = `Santa Cruz, Cochise, Graham, Greenlee`, 
         "nhw" = `...11`, 
         "hispanic" = `...12`, 
         "aian" = `...13`) %>%
  slice(2:34) %>%
  mutate(county = "Santa Cruz, Cochise, Graham, Greenlee")

# Yuma 
brfss_16_yuma <- brfss_2016 %>%
  select(`...1`, `Yuma, La Paz, Mohave`:`...17`) %>%
  rename("label" = `...1`, 
         "all" = `Yuma, La Paz, Mohave`, 
         "nhw" = `...15`, 
         "hispanic" = `...16`, 
         "aian" = `...17`) %>%
  slice(2:34) %>%
  mutate(county = "Yuma, La Paz, Mohave")

# 2018
brfss_19_yuma <- brfss_2018 %>%
  select(`...1`, `Yuma, La Paz, Mohave`:`...17`) %>%
  rename("label" = `...1`, 
         "all" = `Yuma, La Paz, Mohave`, 
         "nhw" = `...15`, 
         "hispanic" = `...16`, 
         "aian" = `...17`) %>%
  slice(2:34) %>%
  mutate(county = "Yuma, La Paz, Mohave")

# combine all together
brfss_2016 <- brfss_16_cochise %>%
  full_join(brfss_16_pima) %>%
  full_join(brfss_16_pinal) %>%
  full_join(brfss_16_yuma)

# 2018 
brfss_2018 <- brfss_19_cochise %>%
  full_join(brfss_19_pima) %>%
  full_join(brfss_19_pinal) %>%
  full_join(brfss_19_yuma)

# gather
brfss_2016 <- brfss_2016 %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
               ) %>%
  mutate(value = as.numeric(value),
         year = 2016,
         year = as.integer(year))

# 2018 
brfss_2018 <- brfss_2018 %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value),
         year = 2018,
         year = as.integer(year))

# combine both datasets 
brfss <- full_join(brfss_2016, brfss_2018)

# inspect
glimpse(brfss)
str(brfss)
distinct(brfss, county)
distinct(brfss, race)
distinct(brfss, year)

# isolate the variables 
variables <- distinct(brfss, label)

# exploratory
brfss %>%
  filter(label == "Had a mammogram") %>%
  ggplot(mapping = aes(x = county, y = value, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(y = "",
       x = "County",
       fill = "Race / Ethnicity",
       title = "Breast Cancer Screening",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS") +
  facet_wrap(~year)

brfss %>%
  filter(label == "Had a mammogram") %>%
  ggplot(mapping = aes(x = year, y = value, color = race)) +
  geom_line() +
  geom_point() +
  facet_wrap(~county) +
  xlim(2010, 2020) +
  labs(y = "",
       x = "Year",
       color = "Race / Ethnicity",
       title = "Breast Cancer Screening",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS")


brfss %>%
  filter(label == "Ever had sigmoidoscopy/colonoscopy") %>%
  ggplot(mapping = aes(x = county, y = value, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(y = "",
       x = "County",
       fill = "Race / Ethnicity",
       title = "Colorectal Cancer Screening",
       subtitle = "Ever had sigmoidoscopy/colonoscopy?",
       caption = "Source: AZ BRFSS") +
  facet_wrap(~year)

brfss %>%
  filter(label == "Ever had sigmoidoscopy/colonoscopy") %>%
  ggplot(mapping = aes(x = year, y = value, color = race)) +
  geom_line() +
  geom_point() +
  facet_wrap(~county) +
  xlim(2010, 2020) +
  labs(y = "",
       x = "Year",
       color = "Race / Ethnicity",
       title = "Colorectal Cancer Screening",
       subtitle = "Ever had sigmoidoscopy/colonoscopy?",
       caption = "Source: AZ BRFSS")
  