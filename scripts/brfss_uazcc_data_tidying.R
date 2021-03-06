# set up 
# load packages ----
library(here)
library(tidyverse)
library(readxl)
library(lubridate)

# read data ----
brfss_2014 <- read_excel("data/raw/BRFSS_Screening_20210302.xlsx", sheet = 1)
brfss_2016 <- read_excel("data/raw/BRFSS_Screening_20210302.xlsx", sheet = 2)
brfss_2018 <- read_excel("data/raw/BRFSS_Screening_20210302.xlsx", sheet = 3)

# inspect
glimpse(brfss_2014)
glimpse(brfss_2016)
glimpse(brfss_2018)

# add year to each dataset 
brfss_2014 <- mutate(brfss_2014, year = 2014)
brfss_2016 <- mutate(brfss_2016, year = 2016)
brfss_2018 <- mutate(brfss_2018, year = 2018)

# tidy the 2014 dataset ----
brfss_2014 

# 2014 catchment ---- all catchment counties together
brfss_14_uazcc <- brfss_2014 %>%
  select(`...1`, 
         starts_with("UACC"),
         year)

glimpse(brfss_2014)

brfss_14_uazcc <- brfss_14_uazcc %>%
  slice(5:8, 10:42) %>%
  rename( # new = old
    "variable" = "...1",
    "all" = "UACC Catchment...2",
    "all_95_ci_min" = "UACC Catchment...3",
    "all_95_ci_max" = "UACC Catchment...4",
    "nhw" = "UACC Catchment...5",
    "nhw_95_ci_min" = "UACC Catchment...6",
    "nhw_95_ci_max" = "UACC Catchment...7",
    "hisp" = "UACC Catchment...8",     
    "hisp_95_ci_min" = "UACC Catchment...9",    
    "hisp_95_ci_max" = "UACC Catchment...10",
    "aian" = "UACC Catchment...11",     
    "aian_95_ci_min" = "UACC Catchment...12",     
    "aian_95_ci_max" = "UACC Catchment...13",     
    "male" = "UACC Catchment...14",     
    "male_95_ci_min" = "UACC Catchment...15",
    "male_95_ci_max" = "UACC Catchment...16",     
    "female" = "UACC Catchment...17",     
    "female_95_ci_min" = "UACC Catchment...18",     
    "female_95_ci_max" = "UACC Catchment...19",     
    "age_7-17" = "UACC Catchment...20",
    "age_7-17_95_ci_min" = "UACC Catchment...21",     
    "age_7-17_95_ci_max" = "UACC Catchment...22",     
    "age_18-64" = "UACC Catchment...23",     
    "age_18-64_95_ci_min" = "UACC Catchment...24",     
    "age_18-64_95_ci_max" = "UACC Catchment...25",
    "age_65" = "UACC Catchment...26",     
    "age_65_95_ci_min" = "UACC Catchment...27",     
    "age_65_95_ci_" = "UACC Catchment...28"
  ) %>%
  mutate(area = "UAZCC Catchment")

brfss_14_uazcc %>%
  pivot_longer(
    cols = c(2,5, 8, 11, 14, 17, 20, 23, 26),
    names_to = "demographic",
    names_prefix = NULL,
    names_sep = NULL,
    names_pattern = NULL,
    names_ptypes = list(),
    names_transform = list(),
    names_repair = "check_unique",
    values_to = "estimate",
    values_drop_na = FALSE,
    values_ptypes = list(),
    values_transform = list()
  ) %>%
  select(1, 20:23)
  
  pivot_longer(
    cols = c(2,3),
    names_to = c("ci_min", "ci_max"),
    names_prefix = NULL,
    names_sep = NULL,
    names_pattern = NULL,
    names_ptypes = list(),
    names_transform = list(),
    names_repair = "check_unique",
    values_to = "estimate",
    values_drop_na = FALSE,
    values_ptypes = list(),
    values_transform = list()
  )


# 2014, pima county ----
# race ----
brfss_2014_pima_race <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_pima_race)
#sample for data quality check
brfss_2014_pima_race %>%
  sample_n(1)

# 2014, pima county, 
# age ----
brfss_2014_pima_age <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect
brfss_2014_pima_age %>%
  filter(age == "7-17") %>%
  sample_n(5)

# inspect  
glimpse(brfss_2014_pima_age)
#sample for data quality check
brfss_2014_pima_age %>%
  sample_n(1)

# 2014, pima county, 
# sex ----
brfss_2014_pima_sex <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_pima_sex)
#sample for data quality check
brfss_2014_pima_sex %>%
  sample_n(1)

# 2014, pinal county ----
# race ----
# this is a combination of pinal and gila counties 
brfss_2014_pinal_race <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal`:`Pinal, Gila...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal`,
         "nhw" = `Pinal, Gila...12`,
         "hispanic" = `Pinal, Gila...13`,
         "aian" = `Pinal, Gila...14`,
         "7-17" = `Pinal, Gila...15`,
         "18-64" = `Pinal, Gila...16`,
         ">=65" = `Pinal, Gila...17`,
         "men" = `Pinal, Gila...18`,
         "women" = `Pinal, Gila...19`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_pinal_race)
#sample for data quality check
brfss_2014_pinal_race %>%
  sample_n(1)

# 2014, pinal county, 
# age----
brfss_2014_pinal_age <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal`:`Pinal, Gila...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal`,
         "nhw" = `Pinal, Gila...12`,
         "hispanic" = `Pinal, Gila...13`,
         "aian" = `Pinal, Gila...14`,
         "7-17" = `Pinal, Gila...15`,
         "18-64" = `Pinal, Gila...16`,
         ">=65" = `Pinal, Gila...17`,
         "men" = `Pinal, Gila...18`,
         "women" = `Pinal, Gila...19`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_pinal_age)
#sample for data quality check
brfss_2014_pinal_age %>%
  sample_n(1)

# 2014, pinal county, 
# sex ----
brfss_2014_pinal_sex <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal`:`Pinal, Gila...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal`,
         "nhw" = `Pinal, Gila...12`,
         "hispanic" = `Pinal, Gila...13`,
         "aian" = `Pinal, Gila...14`,
         "7-17" = `Pinal, Gila...15`,
         "18-64" = `Pinal, Gila...16`,
         ">=65" = `Pinal, Gila...17`,
         "men" = `Pinal, Gila...18`,
         "women" = `Pinal, Gila...19`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_pinal_sex)
#sample for data quality check
brfss_2014_pinal_sex %>%
  sample_n(1)

# 2014, santa cruz & Cochise county ----
# race ----
# this is a combination of santa cruz, cochise, graham, and greenlee
brfss_2014_sc_race <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_sc_race)
#sample for data quality check
brfss_2014_sc_race %>%
  sample_n(1)

# 2014, santa cruz & Cochise county
# age----
brfss_2014_sc_age <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_sc_age)
#sample for data quality check
brfss_2014_sc_age %>%
  sample_n(1)

# 2014, santa cruz & Cochise county 
# sex ----
brfss_2014_sc_sex <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_sc_sex)
#sample for data quality check
brfss_2014_sc_sex %>%
  sample_n(1)

# 2014, Yuma ----
# race ----
# this is a combination of yuma, la paz, and mohave counties 
brfss_2014_yuma_race <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_yuma_race)
#sample for data quality check
brfss_2014_yuma_race %>%
  sample_n(1)

# 2014, Yuma 
# age----
brfss_2014_yuma_age <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_yuma_age)
#sample for data quality check
brfss_2014_yuma_age %>%
  sample_n(1)

# 2014, Yuma 
# sex ----
brfss_2014_yuma_sex <- brfss_2014 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2014_yuma_sex)
#sample for data quality check
brfss_2014_yuma_sex %>%
  sample_n(1)

# 2014 join ----
# age ----
brfss_2014_age <- brfss_2014_pima_age %>%
  full_join(brfss_2014_pinal_age) %>%
  full_join(brfss_2014_sc_age) %>%
  full_join(brfss_2014_yuma_age) 

# race ----
brfss_2014_race <- brfss_2014_pima_race %>%
  full_join(brfss_2014_pinal_race) %>%
  full_join(brfss_2014_sc_race) %>%
  full_join(brfss_2014_yuma_race) 

# sex ----
brfss_2014_sex <- brfss_2014_pima_sex %>%
  full_join(brfss_2014_pinal_sex) %>%
  full_join(brfss_2014_sc_sex) %>%
  full_join(brfss_2014_yuma_sex) 

# tidy 2016 data ----
brfss_2016

# 2016, pima county ----
# race ----
brfss_2016_pima_race <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_pima_race)
#sample for data quality check
brfss_2016_pima_race %>%
  sample_n(1)

# 2016, pima county, 
# age ----
brfss_2016_pima_age <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_pima_age)
#sample for data quality check
brfss_2016_pima_age %>%
  sample_n(1)

# 2016, pima county, 
# sex ----
brfss_2016_pima_sex <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_pima_sex)
#sample for data quality check
brfss_2016_pima_sex %>%
  sample_n(1)

# 2016, pinal county ----
# race ----
# this is a combination of pinal 
brfss_2016_pinal_race <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal...11`:`Pinal...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal...11`,
         "nhw" = `Pinal...12`,
         "hispanic" = `Pinal...13`,
         "aian" = `Pinal...14`,
         "7-17" = `Pinal...15`,
         "18-64" = `Pinal...16`,
         ">=65" = `Pinal...17`,
         "men" = `Pinal...18`,
         "women" = `Pinal...19`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_pinal_race)
#sample for data quality check
brfss_2016_pinal_race %>%
  sample_n(1)

# 2016, pinal county, 
# age----
brfss_2016_pinal_age <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal...11`:`Pinal...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal...11`,
         "nhw" = `Pinal...12`,
         "hispanic" = `Pinal...13`,
         "aian" = `Pinal...14`,
         "7-17" = `Pinal...15`,
         "18-64" = `Pinal...16`,
         ">=65" = `Pinal...17`,
         "men" = `Pinal...18`,
         "women" = `Pinal...19`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_pinal_age)
#sample for data quality check
brfss_2016_pinal_age %>%
  sample_n(1)

# 2016, pinal county, 
# sex ----
brfss_2016_pinal_sex <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal...11`:`Pinal...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal...11`,
         "nhw" = `Pinal...12`,
         "hispanic" = `Pinal...13`,
         "aian" = `Pinal...14`,
         "7-17" = `Pinal...15`,
         "18-64" = `Pinal...16`,
         ">=65" = `Pinal...17`,
         "men" = `Pinal...18`,
         "women" = `Pinal...19`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_pinal_sex)
#sample for data quality check
brfss_2016_pinal_sex %>%
  sample_n(1)

# 2016, santa cruz & Cochise county ----
# race ----
# this is a combination of santa cruz, cochise, graham, and greenlee
brfss_2016_sc_race <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_sc_race)
#sample for data quality check
brfss_2016_sc_race %>%
  sample_n(1)

# 2016, santa cruz & Cochise county
# age----
brfss_2016_sc_age <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_sc_age)
#sample for data quality check
brfss_2016_sc_age %>%
  sample_n(1)

# 2016, santa cruz & Cochise county 
# sex ----
brfss_2016_sc_sex <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_sc_sex)
#sample for data quality check
brfss_2016_sc_sex %>%
  sample_n(1)

# 2016, Yuma ----
# race ----
# this is a combination of yuma, la paz, and mohave counties 
brfss_2016_yuma_race <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_yuma_race)
#sample for data quality check
brfss_2016_yuma_race %>%
  sample_n(1)

# 2016, Yuma 
# age----
brfss_2016_yuma_age <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_yuma_age)
#sample for data quality check
brfss_2016_yuma_age %>%
  sample_n(1)

# 2016, Yuma 
# sex ----
brfss_2016_yuma_sex <- brfss_2016 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2016_yuma_sex)
#sample for data quality check
brfss_2016_yuma_sex %>%
  sample_n(1)

# 2016 join ----
# age ----
brfss_2016_age <- brfss_2016_pima_age %>%
  full_join(brfss_2016_pinal_age) %>%
  full_join(brfss_2016_sc_age) %>%
  full_join(brfss_2016_yuma_age) 

# race ----
brfss_2016_race <- brfss_2016_pima_race %>%
  full_join(brfss_2016_pinal_race) %>%
  full_join(brfss_2016_sc_race) %>%
  full_join(brfss_2016_yuma_race) 

# sex ----
brfss_2016_sex <- brfss_2016_pima_sex %>%
  full_join(brfss_2016_pinal_sex) %>%
  full_join(brfss_2016_sc_sex) %>%
  full_join(brfss_2016_yuma_sex) 

# tidy 2018 data ----
brfss_2018

# 2018, pima county ----
# race ----
brfss_2018_pima_race <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_pima_race)
#sample for data quality check
brfss_2018_pima_race %>%
  sample_n(1)

# 2018, pima county, 
# age ----
brfss_2018_pima_age <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_pima_age)
#sample for data quality check
brfss_2018_pima_age %>%
  sample_n(1)

# 2018, pima county, 
# sex ----
brfss_2018_pima_sex <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_pima_sex)
#sample for data quality check
brfss_2018_pima_sex %>%
  sample_n(1)

# 2018, pinal county ----
# race ----
# this is a combination of pinal and gila counties 
brfss_2018_pinal_race <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal...11`:`Pinal...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal...11`,
         "nhw" = `Pinal...12`,
         "hispanic" = `Pinal...13`,
         "aian" = `Pinal...14`,
         "7-17" = `Pinal...15`,
         "18-64" = `Pinal...16`,
         ">=65" = `Pinal...17`,
         "men" = `Pinal...18`,
         "women" = `Pinal...19`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_pinal_race)
#sample for data quality check
brfss_2018_pinal_race %>%
  sample_n(1)

# 2018, pinal county, 
# age----
brfss_2018_pinal_age <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal...11`:`Pinal...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal...11`,
         "nhw" = `Pinal...12`,
         "hispanic" = `Pinal...13`,
         "aian" = `Pinal...14`,
         "7-17" = `Pinal...15`,
         "18-64" = `Pinal...16`,
         ">=65" = `Pinal...17`,
         "men" = `Pinal...18`,
         "women" = `Pinal...19`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_pinal_age)
#sample for data quality check
brfss_2018_pinal_age %>%
  sample_n(1)

# 2018, pinal county, 
# sex ----
brfss_2018_pinal_sex <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Pinal...11`:`Pinal...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal...11`,
         "nhw" = `Pinal...12`,
         "hispanic" = `Pinal...13`,
         "aian" = `Pinal...14`,
         "7-17" = `Pinal...15`,
         "18-64" = `Pinal...16`,
         ">=65" = `Pinal...17`,
         "men" = `Pinal...18`,
         "women" = `Pinal...19`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_pinal_sex)
#sample for data quality check
brfss_2018_pinal_sex %>%
  sample_n(1)

# 2018, santa cruz & Cochise county ----
# race ----
# this is a combination of santa cruz, cochise, graham, and greenlee
brfss_2018_sc_race <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_sc_race)
#sample for data quality check
brfss_2018_sc_race %>%
  sample_n(1)

# 2018, santa cruz & Cochise county
# age----
brfss_2018_sc_age <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_sc_age)
#sample for data quality check
brfss_2018_sc_age %>%
  sample_n(1)

# 2018, santa cruz & Cochise county 
# sex ----
brfss_2018_sc_sex <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_sc_sex)
#sample for data quality check
brfss_2018_sc_sex %>%
  sample_n(1)

# 2018, Yuma ----
# race ----
# this is a combination of yuma, la paz, and mohave counties 
brfss_2018_yuma_race <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1:5,11,12) %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_yuma_race)
#sample for data quality check
brfss_2018_yuma_race %>%
  sample_n(1)

# 2018, Yuma 
# age----
brfss_2018_yuma_age <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1,6:8,11,12) %>%
  pivot_longer(cols = 2:4,
               names_to = "age",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_yuma_age)
#sample for data quality check
brfss_2018_yuma_age %>%
  sample_n(1)

# 2018, Yuma 
# sex ----
brfss_2018_yuma_sex <- brfss_2018 %>%
  drop_na(`...1`) %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  ) %>%
  select(1,9:12) %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# inspect  
glimpse(brfss_2018_yuma_sex)
#sample for data quality check
brfss_2018_yuma_sex %>%
  sample_n(1)

# 2018 join ----
# age ----
brfss_2018_age <- brfss_2018_pima_age %>%
  full_join(brfss_2018_pinal_age) %>%
  full_join(brfss_2018_sc_age) %>%
  full_join(brfss_2018_yuma_age) 

# race ----
brfss_2018_race <- brfss_2018_pima_race %>%
  full_join(brfss_2018_pinal_race) %>%
  full_join(brfss_2018_sc_race) %>%
  full_join(brfss_2018_yuma_race) 

# sex ----
brfss_2018_sex <- brfss_2018_pima_sex %>%
  full_join(brfss_2018_pinal_sex) %>%
  full_join(brfss_2018_sc_sex) %>%
  full_join(brfss_2018_yuma_sex) 

# combine years ----
# age ----
brfss_age <- brfss_2014_age %>%
  full_join(brfss_2016_age) %>%
  full_join(brfss_2018_age)

# sample for quality check
sample_n(brfss_age, 1)

# save to rds
write_rds(brfss_age, "data/tidy/brfss_az_catchment_age.rds")

# race ----
brfss_race <- brfss_2014_race %>%
  full_join(brfss_2016_race) %>%
  full_join(brfss_2018_race)

# sample for quality check
sample_n(brfss_race, 1)

# save to rds
write_rds(brfss_race, "data/tidy/brfss_az_catchment_race.rds")

# sex ----
brfss_sex <- brfss_2014_sex %>%
  full_join(brfss_2016_sex) %>%
  full_join(brfss_2018_sex)

# sample for quality check
sample_n(brfss_sex, 1)

# save to rds
write_rds(brfss_sex, "data/tidy/brfss_az_catchment_sex.rds")


# stop here
# join all datasets 
brfss <- brfss_2014 %>%
  full_join(brfss_2016) %>%
  full_join(brfss_2018) %>%
  # drop the header rows from each set 
  drop_na(`...1`)

# inspect
glimpse(brfss)

# separate by county
# pima 
brfss_pima <- brfss %>%
  select(`...1`, `Pima...2`:`Pima...10`, year) %>%
  mutate(county = "Pima") %>%
  rename("label" = `...1`,
         "all" = `Pima...2`,
         "nhw" = `Pima...3`,
         "hispanic" = `Pima...4`,
         "aian" = `Pima...5`,
         "7-17" = `Pima...6`,
         "18-64" = `Pima...7`,
         ">=65" = `Pima...8`,
         "men" = `Pima...9`,
         "women" = `Pima...10`
         )
  
# pinal
# for year 2014 also includes Gila county
brfss_pinal <- brfss %>%
  select(`...1`, `Pinal`:`Pinal, Gila...19`, year) %>%
  mutate(county = "Pinal") %>%
  rename("label" = `...1`,
         "all" = `Pinal`,
         "nhw" = `Pinal, Gila...12`,
         "hispanic" = `Pinal, Gila...13`,
         "aian" = `Pinal, Gila...14`,
         "7-17" = `Pinal, Gila...15`,
         "18-64" = `Pinal, Gila...16`,
         ">=65" = `Pinal, Gila...17`,
         "men" = `Pinal, Gila...18`,
         "women" = `Pinal, Gila...19`
  )

# Santa Cruz, Cochise, Graham, Greenlee
brfss_santacruz <- brfss %>%
  select(`...1`, `Santa Cruz, Cochise, Graham, Greenlee...20`:`Santa Cruz, Cochise, Graham, Greenlee...28`, year) %>%
  mutate(county = "Santa Cruz & Cochise") %>%
  rename("label" = `...1`,
         "all" = `Santa Cruz, Cochise, Graham, Greenlee...20`,
         "nhw" = `Santa Cruz, Cochise, Graham, Greenlee...21`,
         "hispanic" = `Santa Cruz, Cochise, Graham, Greenlee...22`,
         "aian" = `Santa Cruz, Cochise, Graham, Greenlee...23`,
         "7-17" = `Santa Cruz, Cochise, Graham, Greenlee...24`,
         "18-64" = `Santa Cruz, Cochise, Graham, Greenlee...25`,
         ">=65" = `Santa Cruz, Cochise, Graham, Greenlee...26`,
         "men" = `Santa Cruz, Cochise, Graham, Greenlee...27`,
         "women" = `Santa Cruz, Cochise, Graham, Greenlee...28`
  )

# Yuma, La Paz, Mohave 
brfss_yuma <- brfss %>%
  select(`...1`, `Yuma, La Paz, Mohave...29`:`Yuma, La Paz, Mohave...37`, year) %>%
  mutate(county = "Yuma") %>%
  rename("label" = `...1`,
         "all" = `Yuma, La Paz, Mohave...29`,
         "nhw" = `Yuma, La Paz, Mohave...30`,
         "hispanic" = `Yuma, La Paz, Mohave...31`,
         "aian" = `Yuma, La Paz, Mohave...32`,
         "7-17" = `Yuma, La Paz, Mohave...33`,
         "18-64" = `Yuma, La Paz, Mohave...34`,
         ">=65" = `Yuma, La Paz, Mohave...35`,
         "men" = `Yuma, La Paz, Mohave...36`,
         "women" = `Yuma, La Paz, Mohave...37`
  )

# join all together
brfss <- brfss_pima %>%
  full_join(brfss_pinal) %>%
  full_join(brfss_santacruz) %>%
  full_join(brfss_yuma)

glimpse(brfss)

# gather by race 
brfss_race <- brfss %>%
  pivot_longer(cols = 2:5,
               names_to = "race",
               values_to = "value"
               ) %>%
  select(year, county, race, label, value) %>%
  mutate(value = as.numeric(value))

# gather by age
brfss_age <- brfss %>%
  pivot_longer(cols = 6:8,
               names_to = "age",
               values_to = "value"
  ) %>%
  select(year, county, age, label, value) %>%
  mutate(value = as.numeric(value))

# gather by sex
brfss_sex <- brfss %>%
  pivot_longer(cols = 9:10,
               names_to = "sex",
               values_to = "value"
  ) %>%
  select(year, county, sex, label, value) %>%
  mutate(value = as.numeric(value))

# isolate the variables 
variables <- distinct(brfss_race, label) 

write_csv(variables, "data/tidy/az_brfss_variables_of_interest.csv")

# bar chart of breast cancer screening by county, year, and age
brfss_age %>%
  filter(label == "Had a mammogram") %>%
  ggplot(mapping = aes(x = county, y = value, fill = age)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(y = "",
       x = "County",
       fill = "Age Group",
       title = "Breast Cancer Screening",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS") +
  facet_wrap(~year)

# colorectal cancer screening line chart 
brfss_age %>%
  filter(label == "Ever had sigmoidoscopy/colonoscopy") %>%
  ggplot(mapping = aes(x = year, y = value, color = age)) +
  geom_line() +
  geom_point() +
  facet_wrap(~county) +
  xlim(2010, 2020) +
  labs(y = "",
       x = "Year",
       color = "Age Group",
       title = "Colorectal Cancer Screening",
       subtitle = "Ever had sigmoidoscopy/colonoscopy?",
       caption = "Source: AZ BRFSS")

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
  
