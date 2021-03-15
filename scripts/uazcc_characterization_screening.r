# set up ----
# packages
library(here)
library(tidyverse)
library(ggthemes)

# read data----
brfss <- read_rds("data/tidy/brfss_usa_complete.rds") # USA
brfss_az <- read_rds("data/tidy/brfss_az_complete.rds") # AZ
uazcc <- read_rds("data/tidy/brfss_az_catchment.rds") # uazcc

# set vectors for outputs
pretty_screening <- c(
  "HADMAM" = "Had a mammogram",
  "HADSIGM3" = "Ever had sigmoidoscopy/colonoscopy",
  "HADPAP2" = "Had a Pap test"
)

pretty_areas <- c(
  "usa" = "US",
  "az" = "AZ",
  "uazcc" = "Catchment",
  "nhw" = "Non-Hispanic White",
  "hisp" = "Hispanic",
  "aian" = "American Indian"
)

# inspect
glimpse(brfss)

# select variables for only
# Mammography Screening
# Colon Cancer Screening
# Cervical Cancer Screening
brfss

# 2018 ----
# USA national data ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(brfss$HADMAM)

usa_hadmam <- brfss %>%
  filter(YEAR == "2018") %>%
  select(HADMAM) %>%
  filter(!is.na(HADMAM)) %>%
  count(HADMAM, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADMAM == "Yes") %>%
  mutate(screening = "HADMAM")

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
str(brfss$HADSIGM3)

usa_hadsig <- brfss %>%
  filter(YEAR == "2018") %>%
  select(HADSIGM3) %>%
  filter(!is.na(HADSIGM3)) %>%
  count(HADSIGM3, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADSIGM3 == "Yes") %>%
  mutate(screening = "HADSIGM3")

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
str(brfss$HADPAP2)

usa_hadpap <- brfss %>%
  filter(YEAR == "2018") %>%
  select(HADPAP2) %>%
  filter(!is.na(HADPAP2)) %>%
  count(HADPAP2, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADPAP2 == "Yes") %>%
  mutate(screening = "HADPAP2")

# combine tables to show all at once
usa <- usa_hadmam %>%
  bind_rows(usa_hadsig) %>%
  bind_rows(usa_hadpap) %>%
  select(screening, "percentage" = perc) %>%
  mutate(area = "usa") %>%
  mutate(screening = pretty_screening[screening])


# Arizona data ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(brfss_az$HADMAM)

az_hadmam <- brfss_az %>%
  filter(YEAR == "2018") %>%
  select(HADMAM) %>%
  filter(!is.na(HADMAM)) %>%
  count(HADMAM, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADMAM == "Yes") %>%
  mutate(screening = "HADMAM")

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
str(brfss_az$HADSIGM3)

az_hadsig <- brfss_az %>%
  filter(YEAR == "2018") %>%
  select(HADSIGM3) %>%
  filter(!is.na(HADSIGM3)) %>%
  count(HADSIGM3, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADSIGM3 == "Yes") %>%
  mutate(screening = "HADSIGM3")

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
str(brfss_az$HADPAP2)

az_hadpap <- brfss_az %>%
  filter(YEAR == "2018") %>%
  select(HADPAP2) %>%
  filter(!is.na(HADPAP2)) %>%
  count(HADPAP2, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADPAP2 == "Yes") %>%
  mutate(screening = "HADPAP2")

# combine tables to show all at once
az <- az_hadmam %>%
  bind_rows(az_hadsig) %>%
  bind_rows(az_hadpap) %>%
  select(screening, "percentage" = perc) %>%
  mutate(area = "az") %>%
  mutate(screening = pretty_screening[screening])

# UAZCC data ----
# all respondents ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(uazcc)

uazcc_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2018
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
uazcc_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2018
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
uazcc_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2018
  )

# combine tables to show all at once
uazcc_all <- uazcc_hadmam %>%
  bind_rows(uazcc_hadsig) %>%
  bind_rows(uazcc_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "uazcc")

# non hispanic white ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
nhw_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2018
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
nhw_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2018
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
nhw_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2018
  )

# combine tables to show all at once
nhw <- nhw_hadmam %>%
  bind_rows(nhw_hadsig) %>%
  bind_rows(nhw_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "nhw")

# Hispanic  ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
hisp_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2018
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
hisp_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2018
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
hisp_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2018
  )

# combine tables to show all at once
hisp <- hisp_hadmam %>%
  bind_rows(hisp_hadsig) %>%
  bind_rows(hisp_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "hisp")

# American Indian ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
aian_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2018
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
aian_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2018
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
aian_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2018
  )

# combine tables to show all at once
aian <- aian_hadmam %>%
  bind_rows(aian_hadsig) %>%
  bind_rows(aian_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "aian")

# combine all tables for output
uazcc_screening_2018 <- usa %>%
  full_join(az) %>%
  full_join(uazcc_all) %>%
  full_join(nhw) %>%
  full_join(hisp) %>%
  full_join(aian) %>%
  mutate(year = 2018)

# spread for a wide table
screening_2018 <- uazcc_screening_2018 %>%
  mutate(area = pretty_areas[area]) %>%
  pivot_wider(
    names_from = "area",
    values_from = "percentage"
  )

screening_2018 %>%
  knitr::kable(
    col.names = c("Screening", "Year", "US", "AZ", "Catchment", "NHW n=1794", "Hispanic n=555", "AI n=68")
  )

# 2016 ----
# USA national data ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(brfss$HADMAM)

usa_hadmam <- brfss %>%
  filter(YEAR == "2016") %>%
  select(HADMAM) %>%
  filter(!is.na(HADMAM)) %>%
  count(HADMAM, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADMAM == "Yes") %>%
  mutate(screening = "HADMAM")

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
str(brfss$HADSIGM3)

usa_hadsig <- brfss %>%
  filter(YEAR == "2016") %>%
  select(HADSIGM3) %>%
  filter(!is.na(HADSIGM3)) %>%
  count(HADSIGM3, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADSIGM3 == "Yes") %>%
  mutate(screening = "HADSIGM3")

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
str(brfss$HADPAP2)

usa_hadpap <- brfss %>%
  filter(YEAR == "2016") %>%
  select(HADPAP2) %>%
  filter(!is.na(HADPAP2)) %>%
  count(HADPAP2, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADPAP2 == "Yes") %>%
  mutate(screening = "HADPAP2")

# combine tables to show all at once
usa <- usa_hadmam %>%
  bind_rows(usa_hadsig) %>%
  bind_rows(usa_hadpap) %>%
  select(screening, "percentage" = perc) %>%
  mutate(area = "usa") %>%
  mutate(screening = pretty_screening[screening])


# Arizona data ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(brfss_az$HADMAM)

az_hadmam <- brfss_az %>%
  filter(YEAR == "2016") %>%
  select(HADMAM) %>%
  filter(!is.na(HADMAM)) %>%
  count(HADMAM, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADMAM == "Yes") %>%
  mutate(screening = "HADMAM")

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
str(brfss_az$HADSIGM3)

az_hadsig <- brfss_az %>%
  filter(YEAR == "2016") %>%
  select(HADSIGM3) %>%
  filter(!is.na(HADSIGM3)) %>%
  count(HADSIGM3, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADSIGM3 == "Yes") %>%
  mutate(screening = "HADSIGM3")

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
str(brfss_az$HADPAP2)

az_hadpap <- brfss_az %>%
  filter(YEAR == "2016") %>%
  select(HADPAP2) %>%
  filter(!is.na(HADPAP2)) %>%
  count(HADPAP2, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADPAP2 == "Yes") %>%
  mutate(screening = "HADPAP2")

# combine tables to show all at once
az <- az_hadmam %>%
  bind_rows(az_hadsig) %>%
  bind_rows(az_hadpap) %>%
  select(screening, "percentage" = perc) %>%
  mutate(area = "az") %>%
  mutate(screening = pretty_screening[screening])

# UAZCC data ----
# all respondents ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(uazcc)

uazcc_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2016
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
uazcc_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2016
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
uazcc_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2016
  )

# combine tables to show all at once
uazcc_all <- uazcc_hadmam %>%
  bind_rows(uazcc_hadsig) %>%
  bind_rows(uazcc_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "uazcc")

# non hispanic white ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
nhw_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2016
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
nhw_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2016
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
nhw_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2016
  )

# combine tables to show all at once
nhw <- nhw_hadmam %>%
  bind_rows(nhw_hadsig) %>%
  bind_rows(nhw_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "nhw")

# Hispanic  ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
hisp_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2016
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
hisp_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2016
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
hisp_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2016
  )

# combine tables to show all at once
hisp <- hisp_hadmam %>%
  bind_rows(hisp_hadsig) %>%
  bind_rows(hisp_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "hisp")

# American Indian ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
aian_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2016
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
aian_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2016
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
aian_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2016
  )

# combine tables to show all at once
aian <- aian_hadmam %>%
  bind_rows(aian_hadsig) %>%
  bind_rows(aian_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "aian")

# combine all tables for output
uazcc_screening_2016 <- usa %>%
  full_join(az) %>%
  full_join(uazcc_all) %>%
  full_join(nhw) %>%
  full_join(hisp) %>%
  full_join(aian) %>%
  mutate(year = 2016)

# spread for a wide table
screening_2016 <- uazcc_screening_2016 %>%
  mutate(area = pretty_areas[area]) %>%
  pivot_wider(
    names_from = "area",
    values_from = "percentage"
  )

screening_2016 %>%
  knitr::kable(
    col.names = c("Screening", "Year", "US", "AZ", "Catchment", "NHW n=2785", "Hispanic n=625", "AI n=60")
  )

# 2014 ----
# USA national data ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(brfss$HADMAM)

usa_hadmam <- brfss %>%
  filter(YEAR == "2014") %>%
  select(HADMAM) %>%
  filter(!is.na(HADMAM)) %>%
  count(HADMAM, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADMAM == "Yes") %>%
  mutate(screening = "HADMAM")

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
str(brfss$HADSIGM3)

usa_hadsig <- brfss %>%
  filter(YEAR == "2014") %>%
  select(HADSIGM3) %>%
  filter(!is.na(HADSIGM3)) %>%
  count(HADSIGM3, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADSIGM3 == "Yes") %>%
  mutate(screening = "HADSIGM3")

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
str(brfss$HADPAP2)

usa_hadpap <- brfss %>%
  filter(YEAR == "2014") %>%
  select(HADPAP2) %>%
  filter(!is.na(HADPAP2)) %>%
  count(HADPAP2, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADPAP2 == "Yes") %>%
  mutate(screening = "HADPAP2")

# combine tables to show all at once
usa <- usa_hadmam %>%
  bind_rows(usa_hadsig) %>%
  bind_rows(usa_hadpap) %>%
  select(screening, "percentage" = perc) %>%
  mutate(area = "usa") %>%
  mutate(screening = pretty_screening[screening])


# Arizona data ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(brfss_az$HADMAM)

az_hadmam <- brfss_az %>%
  filter(YEAR == "2014") %>%
  select(HADMAM) %>%
  filter(!is.na(HADMAM)) %>%
  count(HADMAM, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADMAM == "Yes") %>%
  mutate(screening = "HADMAM")

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
str(brfss_az$HADSIGM3)

az_hadsig <- brfss_az %>%
  filter(YEAR == "2014") %>%
  select(HADSIGM3) %>%
  filter(!is.na(HADSIGM3)) %>%
  count(HADSIGM3, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADSIGM3 == "Yes") %>%
  mutate(screening = "HADSIGM3")

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
str(brfss_az$HADPAP2)

az_hadpap <- brfss_az %>%
  filter(YEAR == "2014") %>%
  select(HADPAP2) %>%
  filter(!is.na(HADPAP2)) %>%
  count(HADPAP2, sort = TRUE) %>%
  mutate(perc = 100 * (n / sum(n))) %>%
  filter(HADPAP2 == "Yes") %>%
  mutate(screening = "HADPAP2")

# combine tables to show all at once
az <- az_hadmam %>%
  bind_rows(az_hadsig) %>%
  bind_rows(az_hadpap) %>%
  select(screening, "percentage" = perc) %>%
  mutate(area = "az") %>%
  mutate(screening = pretty_screening[screening])

# UAZCC data ----
# all respondents ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
str(uazcc)

uazcc_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2014
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
uazcc_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2014
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
uazcc_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "All Respondents",
    statistic == "Mean",
    year == 2014
  )

# combine tables to show all at once
uazcc_all <- uazcc_hadmam %>%
  bind_rows(uazcc_hadsig) %>%
  bind_rows(uazcc_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "uazcc")

# non hispanic white ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
nhw_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2014
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
nhw_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2014
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
nhw_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "NHW",
    statistic == "Mean",
    year == 2014
  )

# combine tables to show all at once
nhw <- nhw_hadmam %>%
  bind_rows(nhw_hadsig) %>%
  bind_rows(nhw_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "nhw")

# Hispanic  ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
hisp_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2014
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
hisp_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2014
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
hisp_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "Hispanic",
    statistic == "Mean",
    year == 2014
  )

# combine tables to show all at once
hisp <- hisp_hadmam %>%
  bind_rows(hisp_hadsig) %>%
  bind_rows(hisp_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "hisp")

# American Indian ----
# mammography screening
# Label: Have You Ever Had a Mammogram
# Question: Have you ever had a mammogram?
aian_hadmam <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2014
  )

# Colon Cancer Screening
# Label: Ever Had Sigmoidoscopy/Colonoscopy
# Question: Sigmoidoscopy and colonoscopy are exams in which a tube is inserted in the rectum to view the colon for signs of cancer or other health problems. Have you ever had either of these exams?
aian_hadsig <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2014
  )

# Cervical Cancer Screening
# Label: Ever Had a Pap Test
# Question: Have you ever had a Pap test?
aian_hadpap <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(
    area == "uacc",
    demographic == "American Indian",
    statistic == "Mean",
    year == 2014
  )

# combine tables to show all at once
aian <- aian_hadmam %>%
  bind_rows(aian_hadsig) %>%
  bind_rows(aian_hadpap) %>%
  select(
    "screening" = variable,
    "percentage" = value
  ) %>%
  mutate(area = "aian")

# combine all tables for output
uazcc_screening_2014 <- usa %>%
  full_join(az) %>%
  full_join(uazcc_all) %>%
  full_join(nhw) %>%
  full_join(hisp) %>%
  full_join(aian) %>%
  mutate(year = 2014)

# spread for a wide table
screening_2014 <- uazcc_screening_2014 %>%
  mutate(area = pretty_areas[area]) %>%
  pivot_wider(
    names_from = "area",
    values_from = "percentage"
  )

screening_2014 %>%
  knitr::kable(
    col.names = c("Screening", "Year", "US", "AZ", "Catchment", "NHW n=3476", "Hispanic n=934", "AI n=113")
      )

# data visualizations ----
# join the long screening tables 
g <- uazcc_screening_2014 %>%
  full_join(uazcc_screening_2016) %>%
  full_join(uazcc_screening_2018)

# prepare palettes
palette_4_cat <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")
palette_3_cat <- c("#a6cee3", "#1f78b4", "#b2df8a")

# breast
g %>%
  filter(screening == "Had a mammogram") %>%
  ggplot(mapping = aes(x = year, y = percentage, color = area)) +
  geom_line(size = 1.25) +
  geom_label(aes(label = round(percentage, digits = 2))) +
  scale_color_discrete(breaks = c("usa", "az", "uazcc", "nhw", "hisp", "aian")) +
  labs(title = "Breast Cancer Screening",
       subtitle = "Have You Ever Had a Mammogram?",
       x = "Year",
       y = "Percentage % of Responses",
       caption = "Source: 2014-2018 CDC BRFSS") +
  theme_bw()

ggsave("figures/charts/brfss_catchment_mammogram.png",
       dpi = 300,
       width = 16,
       height = 10,
       scale = .666)

# cervical
g %>%
  filter(screening == "Had a Pap test") %>%
  ggplot(mapping = aes(x = year, y = percentage, color = area)) +
  geom_line(size = 1.25) +
  geom_label(aes(label = round(percentage, digits = 2))) +
  scale_color_discrete(breaks = c("usa", "az", "uazcc", "nhw", "hisp", "aian")) +
  labs(title = "Cervical Cancer Screening",
       subtitle = "Have You Ever Had a Pap Test?",
       x = "Year",
       y = "Percentage % of Responses",
       caption = "Source: 2014-2018 CDC BRFSS") +
  theme_bw()

ggsave("figures/charts/brfss_catchment_pap.png",
       dpi = 300,
       width = 16,
       height = 10,
       scale = .666)

# colorectal
g %>%
  filter(screening == "Ever had sigmoidoscopy/colonoscopy") %>%
  ggplot(mapping = aes(x = year, y = percentage, color = area)) +
  geom_line(size = 1.25) +
  geom_label(aes(label = round(percentage, digits = 2))) +
  scale_color_discrete(breaks = c("usa", "az", "uazcc", "nhw", "hisp", "aian")) +
  labs(title = "Colorectal Cancer Screening",
       subtitle = "Have You Ever Had a Sigmoidoscopy / Colonoscopy?",
       x = "Year",
       y = "Percentage % of Responses",
       caption = "Source: 2014-2018 CDC BRFSS") +
  theme_bw()

ggsave("figures/charts/brfss_catchment_crc.png",
       dpi = 300,
       width = 16,
       height = 10,
       scale = .666)
