# set up 
# load packages ----
library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

# read data ----
brfss_2014 <- read_excel("data/raw/BRFSS_Screening_20210302.xlsx", sheet = 1) %>%
  janitor::clean_names()
brfss_2016 <- read_excel("data/raw/BRFSS_Screening_20210302.xlsx", sheet = 2) %>%
  janitor::clean_names()
brfss_2018 <- read_excel("data/raw/BRFSS_Screening_20210302.xlsx", sheet = 3) %>%
  janitor::clean_names()

# 2014 ----
# inspect
glimpse(brfss_2014)

# tidy the 2014 dataset ----
names(brfss_2014) <- paste(names(brfss_2014), as.character(brfss_2014[1,]), as.character(brfss_2014[2,]), sep = "+")

# make tidy 
# for the next part
cols_to_pivot <- c(str_subset(names(brfss_2014), "^uacc.*"),
                   str_subset(names(brfss_2014), "^pima.*"),
                   str_subset(names(brfss_2014), "^pinal.*"),
                   str_subset(names(brfss_2014), "^santa.*"),
                   str_subset(names(brfss_2014), "^yuma.*")) 

# make a vector of pretty names for output 
pretty_names <- c("uacc" = "UAZCC Catchment",
                  "pima" = "Pima County",
                  "pinal" = "Pinal County",
                  "santa" = "Santa Cruz County",
                  "yuma" = "Yuma County")

brfss_2014 <- brfss_2014 %>% 
  slice(-(1:4)) %>% # remove the na header rows 
  rename(variable = 1) %>% # rename column 1
  pivot_longer(all_of(cols_to_pivot)) %>% # gather / pivot the areas
  separate(name, into = c("area", "demographic", "statistic"), sep = "\\+") # separate the name variable to actual 

# clean up the area names 
brfss_2014 <- brfss_2014 %>%
  mutate(
    area = str_extract(brfss_2014$area, "^[a-z]+"),
    area_name = pretty_names[area]
  ) %>%
  mutate(year = 2014) # add the year

# 2016 ----
# inspect
glimpse(brfss_2016)

# tidy the 2016 dataset ----
names(brfss_2016) <- paste(names(brfss_2016), as.character(brfss_2016[1,]), as.character(brfss_2016[2,]), sep = "+")

# make tidy 
# for the next part
cols_to_pivot <- c(str_subset(names(brfss_2016), "^uacc.*"),
                   str_subset(names(brfss_2016), "^pima.*"),
                   str_subset(names(brfss_2016), "^pinal.*"),
                   str_subset(names(brfss_2016), "^santa.*"),
                   str_subset(names(brfss_2016), "^yuma.*")) 

brfss_2016 <- brfss_2016 %>% 
  slice(-(1:4)) %>% # remove the na header rows 
  rename(variable = 1) %>% # rename column 1
  pivot_longer(all_of(cols_to_pivot)) %>% # gather / pivot the areas
  separate(name, into = c("area", "demographic", "statistic"), sep = "\\+") # separate the name variable to actual 

# clean up the area names 
brfss_2016 <- brfss_2016 %>%
  mutate(
    area = str_extract(brfss_2016$area, "^[a-z]+"),
    area_name = pretty_names[area]
  ) %>%
  mutate(year = 2016) # add the year

# 2018 ----
# inspect
glimpse(brfss_2018)

# tidy the 2018 dataset ----
names(brfss_2018) <- paste(names(brfss_2018), as.character(brfss_2018[1,]), as.character(brfss_2018[2,]), sep = "+")

# make tidy 
# for the next part
cols_to_pivot <- c(str_subset(names(brfss_2018), "^uacc.*"),
                   str_subset(names(brfss_2018), "^pima.*"),
                   str_subset(names(brfss_2018), "^pinal.*"),
                   str_subset(names(brfss_2018), "^santa.*"),
                   str_subset(names(brfss_2018), "^yuma.*")) 



brfss_2018 <- brfss_2018 %>% 
  slice(-(1:4)) %>% # remove the na header rows 
  rename(variable = 1) %>% # rename column 1
  pivot_longer(all_of(cols_to_pivot)) %>% # gather / pivot the areas
  separate(name, into = c("area", "demographic", "statistic"), sep = "\\+") # separate the name variable to actual 

# clean up the area names 
brfss_2018 <- brfss_2018 %>%
  mutate(
    area = str_extract(brfss_2018$area, "^[a-z]+"),
    area_name = pretty_names[area]
  ) %>%
  mutate(year = 2018) # add the year

# join everything all together 
brfss_uazcc <- brfss_2014 %>%
  full_join(brfss_2016) %>%
  full_join(brfss_2018)

# save to disk for processing 
write_rds(brfss_uazcc, "data/tidy/brfss_az_catchment.rds")
