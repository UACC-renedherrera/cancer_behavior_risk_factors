# tidy data from brfss export
# set up
# load packages ----
library(here)
library(tidyverse)
library(ggthemes)
library(ggrepel)

# read data ----
brfss_breast <- read_rds("data/tidy/brfss_usa_breast.RDS")
brfss_cervical <- read_rds("data/tidy/brfss_usa_cervical.RDS")
brfss_colorectal <- read_rds("data/tidy/brfss_usa_colorectal.RDS")

palette_4_cat <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")
palette_3_cat <- c("#a6cee3", "#1f78b4", "#b2df8a")

# breast cancer screening ----
# plot proportion had mammogram over time by race
# only for non hispanic white, american indian, and hispanic
brfss_breast %>%
  drop_na(HADMAM) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female") %>%
  group_by(YEAR, X_RACE) %>%
  count(HADMAM) %>%
  mutate(prop = n/sum(n)) %>%
  filter(HADMAM == "Yes") %>%
  mutate(prop = prop*100) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = X_RACE)) +
  geom_point() +
  geom_line(size = 1.25) +
  geom_hline(size = 1.25, aes(yintercept = 81.1, color = "2020 Objective 81.1%")) +
  geom_label(aes(label = round(prop, digits = 1))) +
  ylim(25,100) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(breaks = c("2020 Objective 81.1%", "AIAN", "Hispanic", "NHW"), values = palette_4_cat) +
  labs(title = "Breast Cancer Screening by Race / Ethnicity",
       subtitle = "USA: Have You Ever Had a Mammogram?",
       x = "Year",
       y = "Percentage % of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_breast_race.png",
       dpi = 300)

# plot proportion had mammogram over time by age
# only for non hispanic white, american indian, and hispanic
brfss_breast %>%
  drop_na(HADMAM) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female",
         X_AGE65YR %in% c("18-64", ">=65")) %>%
  group_by(YEAR, X_AGE65YR) %>%
  count(HADMAM) %>%
  mutate(prop = n/sum(n)) %>%
  filter(HADMAM == "Yes") %>%
  mutate(prop = prop*100) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = X_AGE65YR)) +
  geom_point() +
  geom_line(size = 1.25) +
  geom_hline(size = 1.25, aes(yintercept = 081.1, color = "2020 Objective 81.1%")) +
  geom_label(aes(label = round(prop, digits = 1))) +
  ylim(25,100) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(breaks = c("2020 Objective 81.1%", "18-64", ">=65"), values = palette_3_cat) +
  labs(title = "Breast Cancer Screening by Age Group",
       subtitle = "USA: Have You Ever Had a Mammogram?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_breast_age.png",
       dpi = 300)

# cervical cancer screening ---- 
# proportion had pap test ----
# by race 
# only for non hispanic white, american indian, and hispanic
brfss_cervical %>%
  drop_na(HADPAP2) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX == "Female") %>%
  group_by(YEAR, X_RACE) %>%
  count(HADPAP2) %>%
  mutate(prop = n/sum(n)) %>%
  filter(HADPAP2 == "Yes") %>%
  mutate(prop = prop*100) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = X_RACE)) +
  geom_point() +
  geom_line(size = 1.25) +
  geom_hline(size = 1.25, aes(yintercept = 93, color = "2020 Objective")) +
  theme_bw() + 
  labs(title = "Cervical Cancer Screening by Race / Ethnicity",
       subtitle = "USA: Have You Ever Had a Pap Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_cervical_pap_race.png",
       dpi = 300)

# plot proportion had pap test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_cervical %>%
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
       subtitle = "USA: Have You Ever Had a Pap Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_cervical__pap_age.png",
       dpi = 300)

# proportion had hpv test ----
# by race 
# only for non hispanic white, american indian, and hispanic
brfss_cervical %>%
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
       subtitle = "USA: Have You Ever Had an HPV Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_cervical_hpv_race.png",
       dpi = 300)

# plot proportion had hpv test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_cervical %>%
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
       subtitle = "USA: Have You Ever Had an HPV Test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_cervical_hpv_age.png",
       dpi = 300)

# colorectal cancer screening ---- 
# blood stool test ----
# proportion by race 
# only for non hispanic white, american indian, and hispanic
# male and female
brfss_colorectal %>%
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
       subtitle = "USA: Ever had a blood stool test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_colorectal_blood_race.png",
       dpi = 300)

# plot proportion had hpv test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_colorectal %>%
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
       subtitle = "USA: Ever had a blood stool test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_colorectal_blood_age.png",
       dpi = 300)

# plot proportion had hpv test over time by sex
# only for non hispanic white, american indian, and hispanic
brfss_colorectal %>%
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
       subtitle = "USA: Ever had a blood stool test?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_colorectal_blood_sex.png",
       dpi = 300)

# sigmoidoscopy colonoscopy ----
# proportion by race 
# only for non hispanic white, american indian, and hispanic
# male and female
brfss_colorectal %>%
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
       subtitle = "USA: Ever had a sigmoidoscopy / colonoscopy?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_colorectal_colonoscopy_race.png",
       dpi = 300)

# plot proportion had hpv test over time by age
# only for non hispanic white, american indian, and hispanic
brfss_colorectal %>%
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
       subtitle = "USA: Ever had a sigmoidoscopy / colonoscopy?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_colorectal_colonoscopy_age.png",
       dpi = 300)

# plot proportion had hpv test over time by sex
# only for non hispanic white, american indian, and hispanic
# testing confidence intervals still need to see if this is correct
brfss_colorectal %>%
  drop_na(HADSIGM3) %>%
  filter(X_RACE %in% c("NHW", "AIAN", "Hispanic"),
         SEX %in% c("Male", "Female")) %>%
  group_by(YEAR, SEX) %>%
  count(HADSIGM3) %>%
  mutate(prop = n/sum(n),
         margin = qnorm(0.975)*sqrt(prop*(1-prop)/n), # confidence intervals, need to confirm first
         low = prop - margin,
         high = prop + margin) %>%
  ggplot(mapping = aes(x = YEAR, y = prop, color = HADSIGM3)) +
  geom_point() +
  geom_line() +
  geom_errorbar(mapping = aes(ymin = low, ymax = high), alpha = .75) +
  facet_wrap(~SEX) +
  theme_bw() + 
  labs(title = "Colorectal Cancer Screening by Sex",
       subtitle = "USA: Ever had a sigmoidoscopy / colonoscopy?",
       x = "Year",
       y = "Proportion of Responses",
       caption = "Source: 2014-2018 CDC BRFSS")

ggsave("figures/charts/brfss_usa_colorectal_colonoscopy_sex.png",
       dpi = 300)
