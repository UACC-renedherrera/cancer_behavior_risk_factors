# set up 
# load packages ----
library(here)
library(tidyverse)
library(ggthemes)

# read data ---- 
brfss_age <- read_rds("data/tidy/brfss_az_catchment_age.rds")
brfss_race <- read_rds("data/tidy/brfss_az_catchment_race.rds")
brfss_sex <- read_rds("data/tidy/brfss_az_catchment_sex.rds")

# breast ----
# breast x age 
brfss_age %>%
  filter(label == "Had a mammogram") %>%
  ggplot(mapping = aes(x = year, y = value, color = age)) +
  geom_point() +
  geom_line() +
  facet_wrap(~county) +
  theme_base() +
  labs(y = "Proportion of Responses",
       x = "Year",
       color = "Age",
       title = "Breast Cancer Screening",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS")

# breast x race
brfss_race %>%
  filter(label == "Had a mammogram") %>%
  ggplot(mapping = aes(x = year, y = value, color = race)) +
  geom_point() +
  geom_line() +
  facet_wrap(~county) +
  theme_base() +
  labs(y = "Proportion of Responses",
       x = "Year",
       color = "Race / Ethnicity",
       title = "Breast Cancer Screening",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS")

# colorectal ----
# colorectal x age
brfss_age %>%
  filter(label == "Ever had sigmoidoscopy/colonoscopy") %>%
  ggplot(mapping = aes(x = year, y = value, color = age)) +
  geom_point() +
  geom_line() +
  facet_wrap(~county) +
  theme_base() +
  labs(y = "Proportion of Responses",
       x = "Year",
       color = "Age",
       title = "Colorectal Cancer Screening",
       subtitle = "Ever had sigmoidoscopy/colonoscopy?",
       caption = "Source: AZ BRFSS")

# colorectal x race
brfss_race %>%
  filter(label == "Ever had sigmoidoscopy/colonoscopy") %>%
  ggplot(mapping = aes(x = year, y = value, color = race)) +
  geom_point() +
  geom_line() +
  facet_wrap(~county) +
  theme_base() +
  labs(y = "Proportion of Responses",
       x = "Year",
       color = "Race / Ethnicity",
       title = "Colorectal Cancer Screening",
       subtitle = "Ever had sigmoidoscopy/colonoscopy?",
       caption = "Source: AZ BRFSS")

# colorectal x sex
brfss_sex %>%
  filter(label == "Ever had sigmoidoscopy/colonoscopy") %>%
  ggplot(mapping = aes(x = year, y = value, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~county) +
  theme_base() +
  labs(y = "Proportion of Responses",
       x = "Year",
       color = "Sex",
       title = "Colorectal Cancer Screening",
       subtitle = "Ever had sigmoidoscopy/colonoscopy?",
       caption = "Source: AZ BRFSS")

# cervical ----
# cervical x age
brfss_age %>%
  filter(label == "Had a Pap test") %>%
  ggplot(mapping = aes(x = year, y = value, color = age)) +
  geom_point() +
  geom_line() +
  facet_wrap(~county) +
  theme_base() +
  labs(y = "Proportion of Responses",
       x = "Year",
       color = "Age",
       title = "Cervical Cancer Screening",
       subtitle = "Had a Pap test?",
       caption = "Source: AZ BRFSS")

# cervical x race
brfss_race %>%
  filter(label == "Had a Pap test") %>%
  ggplot(mapping = aes(x = year, y = value, color = race)) +
  geom_point() +
  geom_line() +
  facet_wrap(~county) +
  theme_base() +
  labs(y = "Proportion of Responses",
       x = "Year",
       color = "Race / Ethnicity",
       title = "Cervical Cancer Screening",
       subtitle = "Had a Pap test?",
       caption = "Source: AZ BRFSS")

