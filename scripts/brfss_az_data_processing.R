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
# breast cancer screening ----
# proportion had mammogram ----
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADMAM) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic")) %>%
  group_by(YEAR, X_RACE) %>%
  count(HADMAM) %>%
  mutate(prop = n/sum(n))
  
# plot proportion had mammogram over time by race
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADMAM) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female") %>%
  group_by(YEAR, X_RACE) %>%
  count(HADMAM) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADMAM)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = 0.811, color = "2020 Objective")) +
  facet_wrap(~X_RACE) +
  theme_bw() + 
  labs(title = "Breast Cancer Screening by Race / Ethnicity",
       subtitle = "AZ: Have You Ever Had a Mammogram?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# plot proportion had mammogram over time by age
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADMAM) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female",
         X_AGE65YR %in% c("18-64", ">=65")) %>%
  group_by(YEAR, X_AGE65YR) %>%
  count(HADMAM) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADMAM)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = 0.811, color = "2020 Objective")) +
  facet_wrap(~X_AGE65YR) +
  theme_bw() + 
  labs(title = "Breast Cancer Screening by Age Group",
       subtitle = "AZ: Have You Ever Had a Mammogram?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# cervical cancer screening ---- 
# proportion had pap test ----
# by race 
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADPAP2) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female") %>%
  group_by(YEAR, X_RACE) %>%
  count(HADPAP2) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADPAP2)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = 0.93, color = "2020 Objective")) +
  facet_wrap(~X_RACE) +
  theme_bw() + 
  labs(title = "Cervical Cancer Screening by Race / Ethnicity",
       subtitle = "AZ: Have You Ever Had a Pap Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# plot proportion had pap test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADPAP2) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female",
         X_AGE65YR %in% c("18-64", ">=65")) %>%
  group_by(YEAR, X_AGE65YR) %>%
  count(HADPAP2) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADPAP2)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = 0.93, color = "2020 Objective")) +
  facet_wrap(~X_AGE65YR) +
  theme_bw() + 
  labs(title = "Cervical Cancer Screening by Age Group",
       subtitle = "AZ: Have You Ever Had a Pap Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# proportion had hpv test ----
# by race 
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HPVTEST) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female") %>%
  group_by(YEAR, X_RACE) %>%
  count(HPVTEST) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HPVTEST)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_RACE) +
  theme_bw() + 
  labs(title = "Cervical Cancer Screening by Race / Ethnicity",
       subtitle = "AZ: Have You Ever Had an HPV Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# plot proportion had hpv test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HPVTEST) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female",
         X_AGE65YR %in% c("18-64", ">=65")) %>%
  group_by(YEAR, X_AGE65YR) %>%
  count(HPVTEST) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HPVTEST)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_AGE65YR) +
  theme_bw() + 
  labs(title = "Cervical Cancer Screening by Age Group",
       subtitle = "AZ: Have You Ever Had an HPV Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# colorectal cancer screening ---- 
# blood stool test ----
# proportion by race 
# only for non hispanic white, american indian, and hispanic
# male and female
brfss_az %>%
  drop_na(BLDSTOOL) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic")) %>%
  group_by(YEAR, X_RACE) %>%
  count(BLDSTOOL) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = BLDSTOOL)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_RACE) +
  theme_bw() + 
  labs(title = "Colorectal Cancer Screening by Race / Ethnicity",
       subtitle = "AZ: Ever had a blood stool test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# plot proportion had hpv test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(BLDSTOOL) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         X_AGE65YR %in% c("18-64", ">=65")) %>%
  group_by(YEAR, X_AGE65YR) %>%
  count(BLDSTOOL) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = BLDSTOOL)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_AGE65YR) +
  theme_bw() + 
  labs(title = "Colorectal Cancer Screening by Age Group",
       subtitle = "AZ: Ever had a blood stool test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# plot proportion had hpv test over time by sex
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(BLDSTOOL) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX %in% c("Male", "Female")) %>%
  group_by(YEAR, SEX) %>%
  count(BLDSTOOL) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = BLDSTOOL)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SEX) +
  theme_bw() + 
  labs(title = "Colorectal Cancer Screening by Sex",
       subtitle = "AZ: Ever had a blood stool test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# sigmoidoscopy colonoscopy ----
# proportion by race 
# only for non hispanic white, american indian, and hispanic
# male and female
brfss_az %>%
  drop_na(HADSIGM3) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic")) %>%
  group_by(YEAR, X_RACE) %>%
  count(HADSIGM3) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADSIGM3)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_RACE) +
  theme_bw() + 
  labs(title = "Colorectal Cancer Screening by Race / Ethnicity",
       subtitle = "AZ: Ever had a sigmoidoscopy / colonoscopy?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# plot proportion had hpv test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADSIGM3) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         X_AGE65YR %in% c("18-64", ">=65")) %>%
  group_by(YEAR, X_AGE65YR) %>%
  count(HADSIGM3) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADSIGM3)) +
  geom_point() +
  geom_line() +
  facet_wrap(~X_AGE65YR) +
  theme_bw() + 
  labs(title = "Colorectal Cancer Screening by Age Group",
       subtitle = "AZ: Ever had a sigmoidoscopy / colonoscopy?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

# plot proportion had hpv test over time by sex
# only for non hispanic white, american indian, and hispanic
brfss_az %>%
  drop_na(HADSIGM3) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX %in% c("Male", "Female")) %>%
  group_by(YEAR, SEX) %>%
  count(HADSIGM3) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADSIGM3)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SEX) +
  theme_bw() + 
  labs(title = "Colorectal Cancer Screening by Sex",
       subtitle = "AZ: Ever had a sigmoidoscopy / colonoscopy?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")
