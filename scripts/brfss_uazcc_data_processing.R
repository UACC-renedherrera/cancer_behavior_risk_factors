# set up 
# load packages ----
library(here)
library(tidyverse)
library(ggthemes)

# read brfss data ---- 
uazcc <- read_rds("data/tidy/brfss_az_catchment.rds")

# color palettes 
palette_5_cat <- c("#386cb0",
                   "#ffff99",
                   "#fdc086",
                   "#beaed4",
                   "#7fc97f")

palette_4_cat <- c("#ffff99",
                   "#fdc086",
                   "#beaed4",
                   "#7fc97f")

palette_3_cat <- c("#fdc086",
                   "#beaed4",
                   "#7fc97f")




glimpse(uazcc)
str(uazcc$variable)
#dput(uazcc$variable)

# breast ----
# breast x area x all 
uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean") %>%
  ggplot(mapping = aes(x = year, y = value, color = area_name)) +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  #geom_label(aes(label = round(value, digits = 1))) +
  ylim(40,90) +
  theme_bw() +
  labs(y = "Percentage of Responses",
       x = "Year",
       color = "Area",
       title = "Breast Cancer Screening: UAZCC Catchment area",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS") +
  scale_color_colorblind(breaks = c("UAZCC Catchment", "Pima County", "Pinal County", "Santa Cruz County", "Yuma County")) +
  theme(legend.position = "bottom", legend.title = element_blank()) 

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

