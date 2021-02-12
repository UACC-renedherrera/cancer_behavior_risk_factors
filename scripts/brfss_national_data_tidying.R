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
  as_tibble()

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
  as_tibble()

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
         "SEX" = "SEX1" #1 MALE, 2 FEMALE, 3 
  ) %>%
  as_tibble()

# combine all datasets together
brfss <- brfss_2014 %>%
  full_join(brfss_2016) %>%
  full_join(brfss_2018) 

brfss <- as_tibble(brfss)