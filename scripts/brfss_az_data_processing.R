# tidy data from brfss export
# set up
# load packages ----
library(here)
library(tidyverse)
library(ggthemes)

# read data ----
brfss_az <- read_rds("data/tidy/brfss_az.RDS")
glimpse(brfss_az)

# exploratory
# proportion had mammogram
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADMAM) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic")) %>%
  group_by(YEAR, X_RACE) %>%
  count(HADMAM) %>%
  mutate(prop = n/sum(n))
  
# plot proportion had mammogram over time
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADMAM) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic")) %>%
  group_by(YEAR, X_RACE) %>%
  count(HADMAM) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADMAM)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_RACE)

# proportion had pap test
brfss_az %>%
  drop_na(HADPAP2) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic")) %>%
  group_by(YEAR, X_RACE) %>%
  count(HADPAP2) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADPAP2)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_RACE)
