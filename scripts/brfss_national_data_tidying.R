# tidy data from brfss export
# set up
# load packages ----
library(here)
library(tidyverse)
library(foreign)

# read data ----
# national
# 2014 dataset ----
# unzip 
unzip("data/raw/LLCP2014XPT.ZIP", exdir = "data/raw/brfss_2014")

# read to data
brfss_2014 <- read.xport("data/raw/brfss_2014/LLCP2014.XPT") 

# inspect
str(brfss_2014)
glimpse(brfss_2014)

# select variables of interest
brfss_2014 <- brfss_2014 %>%
  select("X_STATE", #4 = AZ
          "X_BMI5CAT", #1 underweight, 2 normal, 3 overweight, 4 obese
         "DIABETE3", #1 yes, 2 yes pg, 3 no, 4 no prediabetes, 7 unknown, 9 refused
         "X_SMOKER3", #1 current everyday, 2 current somedays, 3 former, 4 never, 9 refused
         "X_TOTINDA", #1 HAD PHYS ACT, 2 no phys act in last 30 days, 9 unknown refused
         "X_RFBING5", #1 no, 2 yes, 9 unknown refused
         "HADMAM", #1 yes, 2 no, 7 unknown, 9 refused
         "HOWLONG", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADPAP2", #1 YES, 2 NO, 7 UNKNOWN, 9 REFUSED
         "LASTPAP2", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HPVTEST", #1 1 YES, 2 NO, 7 UNKNOWN, 9 REFUSED
         "HPLSTTST", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADHYST2", #1 yes, 2 no, 7 unknown, 9 refused
         "BLDSTOOL", #1 yes, 2 no, 7 unknown, 9 refused
         "LSTBLDS3", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADSIGM3", #1 yes, 2 no, 7 unknown, 9 refused
         "HADSGCO1", #1 SIGMOIDOSCOPY, 2 COLONOSCOPY, 7 UNKNOWN, 9 REFUSED
         "LASTSIG3", #1 <12 months, 2 <24months, 3 <3years, 4 3-5years, 5 5-10 years, 6 >10years, 7 unknown, 9 refused
         "X_AGE65YR", #1 age 18-64, 2 age >= 65, 3 unknown refused missing
         "X_RACE", #1 NHW, 2 NHB, 3 NHAIAN, 4 NHA, 5 HAWAIIAN, 6 NH OTHER, 7 NH MULTI, 8 HISP, 9 UNKNOWN REFUSED
         "SEX" #1 MALE 2 FEMALE
         ) %>%
  as_tibble() %>%
  mutate(X_STATE = as_factor(X_STATE),
         HADMAM = as_factor(HADMAM),
         HOWLONG = as_factor(HOWLONG),
         HADPAP2 = as_factor(HADPAP2),
         LASTPAP2 = as_factor(LASTPAP2),
         HPVTEST = as_factor(HPVTEST),
         HPLSTTST = as_factor(HPLSTTST),
         HADHYST2 = as_factor(HADHYST2),
         BLDSTOOL = as_factor(BLDSTOOL),
         LSTBLDS3 = as_factor(LSTBLDS3),
         HADSIGM3 = as_factor(HADSIGM3),
         HADSGCO1 = as_factor(HADSGCO1),
         LASTSIG3 = as_factor(LASTSIG3),
         X_AGE65YR = as_factor(X_AGE65YR),
         X_RACE = as_factor(X_RACE),
         SEX = as_factor(SEX),
         YEAR = 2014)

# recode factors 
brfss_2014 <- brfss_2014 %>%
  mutate(HADMAM = fct_recode(HADMAM,
                             "Yes" = "1",
                             "No" = "2",
                             "Unknown" = "7",
                             "Refused" = "9"),
    HADPAP2 = fct_recode(HADPAP2,
                              "Yes" = "1",
                              "No" = "2",
                              "Unknown" = "7",
                              "Refused" = "9"),
    HPVTEST = fct_recode(HPVTEST,
                         "Yes" = "1",
                         "No" = "2",
                         "Unknown" = "7",
                         "Refused" = "9"),
    HADHYST2 = fct_recode(HADHYST2,
                         "Yes" = "1",
                         "No" = "2",
                         "Unknown" = "7",
                         "Refused" = "9"),
    BLDSTOOL = fct_recode(BLDSTOOL,
                         "Yes" = "1",
                         "No" = "2",
                         "Unknown" = "7",
                         "Refused" = "9"),
    HADSIGM3 = fct_recode(HADSIGM3,
                          "Yes" = "1",
                          "No" = "2",
                          "Unknown" = "7",
                          "Refused" = "9"),
    X_AGE65YR = fct_recode(X_AGE65YR,
                           "18-64" = "1",
                           ">=65" = "2",
                           "Unknown/Refused" ="3"),
    X_RACE = fct_recode(X_RACE,
                        "NHW" = "1",
                        "NHB" = "2",
                        "AIAN" = "3",
                        "Asian" = "4",
                        "Hawaiian" = "5",
                        "Other" = "6",
                        "Multi" = "7",
                        "Hispanic" = "8",
                        "Unknown/Refused" = "9"),
    SEX = fct_recode(SEX,
                          "Male" = "1",
                          "Female" = "2",
                     "Unknown" = "7",
                     "Refused" = "9"))

# save to disk
write_rds(brfss_2014, "data/tidy/brfss_usa_2014.RDS")

# cervical cancer brfss + demographics 
brfss_2014_cervical <- brfss_2014 %>% 
  select(X_STATE,
         9:13,
         19:22)

# save to RDS 
write_rds(brfss_2014_cervical, "data/tidy/brfss_usa_2014_cervical.RDS")

# breast cancer brfss + demographics 
brfss_2014_breast <- brfss_2014 %>% 
  select(X_STATE,
         7:8,
         19:22)

# save to RDS 
write_rds(brfss_2014_breast, "data/tidy/brfss_usa_2014_breast.RDS")

# colorectal cancer brfss + demographics 
brfss_2014_colorectal <- brfss_2014 %>% 
  select(X_STATE,
         14:18,
         19:22)

# save to RDS 
write_rds(brfss_2014_colorectal, "data/tidy/brfss_usa_2014_colorectal.RDS")

# filter to arizona only
brfss_2014_az <- brfss_2014 %>%
  filter(X_STATE == "4")

# 2016 dataset ----
# unzip 
unzip("data/raw/LLCP2016XPT.ZIP", exdir = "data/raw/brfss_2016")

# read to data
brfss_2016 <- read.xport("data/raw/brfss_2016/LLCP2016.XPT")

# inspect
str(brfss_2016)
glimpse(brfss_2016)

# select variables of interest
brfss_2016 <- brfss_2016 %>%
  select("X_STATE", #4 = AZ
         "X_BMI5CAT", #1 underweight, 2 normal, 3 overweight, 4 obese
         "DIABETE3", #1 yes, 2 yes pg, 3 no, 4 no prediabetes, 7 unknown, 9 refused
         "X_SMOKER3", #1 current everyday, 2 current somedays, 3 former, 4 never, 9 refused
         "X_TOTINDA", #1 HAD PHYS ACT, 2 no phys act in last 30 days, 9 unknown refused
         "X_RFBING5", #1 no, 2 yes, 9 unknown refused
         "HADMAM", #1 yes, 2 no, 7 unknown, 9 refused
         "HOWLONG", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADPAP2", #1 YES, 2 NO, 7 UNKNOWN, 9 REFUSED
         "LASTPAP2", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HPVTEST", #1 1 YES, 2 NO, 7 UNKNOWN, 9 REFUSED
         "HPLSTTST", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADHYST2", #1 yes, 2 no, 7 unknown, 9 refused
         "BLDSTOOL", #1 yes, 2 no, 7 unknown, 9 refused
         "LSTBLDS3", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADSIGM3", #1 yes, 2 no, 7 unknown, 9 refused
         "HADSGCO1", #1 SIGMOIDOSCOPY, 2 COLONOSCOPY, 7 UNKNOWN, 9 REFUSED
         "LASTSIG3", #1 <12 months, 2 <24months, 3 <3years, 4 3-5years, 5 5-10 years, 6 >10years, 7 unknown, 9 refused
         "X_AGE65YR", #1 age 18-64, 2 age >= 65, 3 unknown refused missing
         "X_RACE", #1 NHW, 2 NHB, 3 NHAIAN, 4 NHA, 5 HAWAIIAN, 6 NH OTHER, 7 NH MULTI, 8 HISP, 9 UNKNOWN REFUSED
         "SEX" #1 MALE 2 FEMALE
  ) %>%
  as_tibble() %>%
  mutate(X_STATE = as_factor(X_STATE),
         HADMAM = as_factor(HADMAM),
         HOWLONG = as_factor(HOWLONG),
         HADPAP2 = as_factor(HADPAP2),
         LASTPAP2 = as_factor(LASTPAP2),
         HPVTEST = as_factor(HPVTEST),
         HPLSTTST = as_factor(HPLSTTST),
         HADHYST2 = as_factor(HADHYST2),
         BLDSTOOL = as_factor(BLDSTOOL),
         LSTBLDS3 = as_factor(LSTBLDS3),
         HADSIGM3 = as_factor(HADSIGM3),
         HADSGCO1 = as_factor(HADSGCO1),
         LASTSIG3 = as_factor(LASTSIG3),
         X_AGE65YR = as_factor(X_AGE65YR),
         X_RACE = as_factor(X_RACE),
         SEX = as_factor(SEX),
         YEAR = 2016)

# recode factors
brfss_2016 <- brfss_2016 %>%
  mutate(HADMAM = fct_recode(HADMAM,
                             "Yes" = "1",
                             "No" = "2",
                             "Unknown" = "7",
                             "Refused" = "9"),
         HADPAP2 = fct_recode(HADPAP2,
                              "Yes" = "1",
                              "No" = "2",
                              "Unknown" = "7",
                              "Refused" = "9"),
         HPVTEST = fct_recode(HPVTEST,
                              "Yes" = "1",
                              "No" = "2",
                              "Unknown" = "7",
                              "Refused" = "9"),
         HADHYST2 = fct_recode(HADHYST2,
                               "Yes" = "1",
                               "No" = "2",
                               "Unknown" = "7",
                               "Refused" = "9"),
         BLDSTOOL = fct_recode(BLDSTOOL,
                               "Yes" = "1",
                               "No" = "2",
                               "Unknown" = "7",
                               "Refused" = "9"),
         HADSIGM3 = fct_recode(HADSIGM3,
                               "Yes" = "1",
                               "No" = "2",
                               "Unknown" = "7",
                               "Refused" = "9"),
         X_AGE65YR = fct_recode(X_AGE65YR,
                                "18-64" = "1",
                                ">=65" = "2",
                                "Unknown/Refused" ="3"),
         X_RACE = fct_recode(X_RACE,
                             "NHW" = "1",
                             "NHB" = "2",
                             "AIAN" = "3",
                             "Asian" = "4",
                             "Hawaiian" = "5",
                             "Other" = "6",
                             "Multi" = "7",
                             "Hispanic" = "8",
                             "Unknown/Refused" = "9"),
         SEX = fct_recode(SEX,
                          "Male" = "1",
                          "Female" = "2",
                          "Unknown" = "7",
                          "Refused" = "9"))

# save to disk
write_rds(brfss_2016, "data/tidy/brfss_usa_2016.RDS")

# cervical cancer brfss + demographics 
brfss_2016_cervical <- brfss_2016 %>% 
  select(X_STATE,
         9:13,
         19:22)

# save to RDS 
write_rds(brfss_2016_cervical, "data/tidy/brfss_usa_2016_cervical.RDS")

# breast cancer brfss + demographics 
brfss_2016_breast <- brfss_2016 %>% 
  select(X_STATE,
         7:8,
         19:22)

# save to RDS 
write_rds(brfss_2016_breast, "data/tidy/brfss_usa_2016_breast.RDS")

# colorectal cancer brfss + demographics 
brfss_2016_colorectal <- brfss_2016 %>% 
  select(X_STATE,
         14:18,
         19:22)

# save to RDS 
write_rds(brfss_2016_colorectal, "data/tidy/brfss_usa_2016_colorectal.RDS")

# filter to arizona only
brfss_2016_az <- brfss_2016 %>%
  filter(X_STATE == "4")

# 2018 dataset ----
# unzip 
unzip("data/raw/LLCP2018XPT.ZIP", exdir = "data/raw/brfss_2018")

# read to data
brfss_2018 <- read.xport("data/raw/brfss_2018/LLCP2018.XPT")

# inspect
str(brfss_2018)
glimpse(brfss_2018)

# select variables of interest
brfss_2018 <- brfss_2018 %>%
  select("X_STATE", #4 = AZ
         "X_BMI5CAT", #1 underweight, 2 normal, 3 overweight, 4 obese
         "DIABETE3", #1 yes, 2 yes pg, 3 no, 4 no prediabetes, 7 unknown, 9 refused
         "X_SMOKER3", #1 current everyday, 2 current somedays, 3 former, 4 never, 9 refused
         "X_TOTINDA", #1 HAD PHYS ACT, 2 no phys act in last 30 days, 9 unknown refused
         "X_RFBING5", #1 no, 2 yes, 9 unknown refused
         "HADMAM", #1 yes, 2 no, 7 unknown, 9 refused
         "HOWLONG", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADPAP2", #1 YES, 2 NO, 7 UNKNOWN, 9 REFUSED
         "LASTPAP2", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HPVTEST", #1 1 YES, 2 NO, 7 UNKNOWN, 9 REFUSED
         "HPLSTTST", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADHYST2", #1 yes, 2 no, 7 unknown, 9 refused
         "BLDSTOOL", #1 yes, 2 no, 7 unknown, 9 refused
         "LSTBLDS3", #1 <12 month, 2 <24 month, 3 <3 year, 4 3-5year, 5 >=5 year, 7 unknown, 9 refused
         "HADSIGM3", #1 yes, 2 no, 7 unknown, 9 refused
         "HADSGCO1", #1 SIGMOIDOSCOPY, 2 COLONOSCOPY, 7 UNKNOWN, 9 REFUSED
         "LASTSIG3", #1 <12 months, 2 <24months, 3 <3years, 4 3-5years, 5 5-10 years, 6 >10years, 7 unknown, 9 refused
         "X_AGE65YR", #1 age 18-64, 2 age >= 65, 3 unknown refused missing
         "X_RACE", #1 NHW, 2 NHB, 3 NHAIAN, 4 NHA, 5 HAWAIIAN, 6 NH OTHER, 7 NH MULTI, 8 HISP, 9 UNKNOWN REFUSED
         "SEX" = "SEX1" #1 MALE, 2 FEMALE, 7 UNKNOWN, 9 REFUSED
  ) %>%
  as_tibble() %>%
  mutate(X_STATE = as_factor(X_STATE),
         HADMAM = as_factor(HADMAM),
         HOWLONG = as_factor(HOWLONG),
         HADPAP2 = as_factor(HADPAP2),
         LASTPAP2 = as_factor(LASTPAP2),
         HPVTEST = as_factor(HPVTEST),
         HPLSTTST = as_factor(HPLSTTST),
         HADHYST2 = as_factor(HADHYST2),
         BLDSTOOL = as_factor(BLDSTOOL),
         LSTBLDS3 = as_factor(LSTBLDS3),
         HADSIGM3 = as_factor(HADSIGM3),
         HADSGCO1 = as_factor(HADSGCO1),
         LASTSIG3 = as_factor(LASTSIG3),
         X_AGE65YR = as_factor(X_AGE65YR),
         X_RACE = as_factor(X_RACE),
         SEX = as_factor(SEX),
         YEAR = 2018)

# recode factors
brfss_2018 <- brfss_2018 %>%
  mutate(HADMAM = fct_recode(HADMAM,
                             "Yes" = "1",
                             "No" = "2",
                             "Unknown" = "7",
                             "Refused" = "9"),
         HADPAP2 = fct_recode(HADPAP2,
                              "Yes" = "1",
                              "No" = "2",
                              "Unknown" = "7",
                              "Refused" = "9"),
         HPVTEST = fct_recode(HPVTEST,
                              "Yes" = "1",
                              "No" = "2",
                              "Unknown" = "7",
                              "Refused" = "9"),
         HADHYST2 = fct_recode(HADHYST2,
                               "Yes" = "1",
                               "No" = "2",
                               "Unknown" = "7",
                               "Refused" = "9"),
         BLDSTOOL = fct_recode(BLDSTOOL,
                               "Yes" = "1",
                               "No" = "2",
                               "Unknown" = "7",
                               "Refused" = "9"),
         HADSIGM3 = fct_recode(HADSIGM3,
                               "Yes" = "1",
                               "No" = "2",
                               "Unknown" = "7",
                               "Refused" = "9"),
         X_AGE65YR = fct_recode(X_AGE65YR,
                                "18-64" = "1",
                                ">=65" = "2",
                                "Unknown/Refused" ="3"),
         X_RACE = fct_recode(X_RACE,
                             "NHW" = "1",
                             "NHB" = "2",
                             "AIAN" = "3",
                             "Asian" = "4",
                             "Hawaiian" = "5",
                             "Other" = "6",
                             "Multi" = "7",
                             "Hispanic" = "8",
                             "Unknown/Refused" = "9"),
         SEX = fct_recode(SEX,
                          "Male" = "1",
                          "Female" = "2",
                          "Unknown" = "7",
                          "Refused" = "9"))

# save to disk
write_rds(brfss_2018, "data/tidy/brfss_usa_2018.RDS")

# national brfss data ----
# join all national brfss data to one set 
brfss <- brfss_2014 %>%
  full_join(brfss_2016) %>%
  full_join(brfss_2018)

# save to disk 
write_rds(brfss, "data/tidy/brfss_usa_complete.rds")

# arizona brfss data ---- 
# filter complete set to az only 
brfss_az <- brfss %>%
  filter(X_STATE == "4")

# save to disk 
write_rds(brfss_az, "data/tidy/brfss_az_complete.rds")

# cervical cancer brfss + demographics 
brfss_2018_cervical <- brfss_2018 %>% 
  select(X_STATE,
         9:13,
         19:22)

# save to RDS 
write_rds(brfss_2018_cervical, "data/tidy/brfss_usa_2018_cervical.RDS")

# breast cancer brfss + demographics 
brfss_2018_breast <- brfss_2018 %>% 
  select(X_STATE,
         7:8,
         19:22)

# save to RDS 
write_rds(brfss_2018_breast, "data/tidy/brfss_usa_2018_breast.RDS")

# colorectal cancer brfss + demographics 
brfss_2018_colorectal <- brfss_2018 %>% 
  select(X_STATE,
         14:18,
         19:22)

# save to RDS 
write_rds(brfss_2018_colorectal, "data/tidy/brfss_usa_2018_colorectal.RDS")

# filter to arizona only
brfss_2018_az <- brfss_2018 %>%
  filter(X_STATE == "4")

# combine all brfss az 2014-18
brfss_az <- brfss_2014_az %>%
  full_join(brfss_2016_az) %>%
  full_join(brfss_2018_az)

# save to disk
write_rds(brfss_az, "data/tidy/brfss_az.RDS")

# produce breast cancer screening dataset ---- 
brfss_breast <- brfss_2014_breast %>%
  full_join(brfss_2016_breast) %>%
  full_join(brfss_2018_breast)

# save to disk
write_rds(brfss_breast, "data/tidy/brfss_usa_breast.RDS")

# produce cervical cancer screening dataset ---- 
brfss_cervical <- brfss_2014_cervical %>%
  full_join(brfss_2016_cervical) %>%
  full_join(brfss_2018_cervical)

# save to disk
write_rds(brfss_cervical, "data/tidy/brfss_usa_cervical.RDS")

# produce colorectal cancer screening dataset ---- 
brfss_colorectal <- brfss_2014_colorectal %>%
  full_join(brfss_2016_colorectal) %>%
  full_join(brfss_2018_colorectal)

# save to disk
write_rds(brfss_colorectal, "data/tidy/brfss_usa_colorectal.RDS")

