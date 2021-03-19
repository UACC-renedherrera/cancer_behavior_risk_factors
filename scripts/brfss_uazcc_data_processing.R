# set up 
# load packages ----
library(here)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(leaflet)
library(tigris)
library(sf)
library(sp)

options(tigris_use_cache = TRUE)

# read brfss data ---- 
uazcc <- read_rds("data/tidy/brfss_az_catchment.rds")

# add new areas where Santa Cruz == c("Santa Cruz", "Cochise", "Graham", "Greenlee")
cochise <- uazcc %>%
  filter(str_detect(uazcc$area, "santa")) %>%
  mutate(area = "cochise",
         area_name = "Cochise County")

uazcc_mapping <- uazcc %>%
  full_join(cochise)

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

# inspect 
glimpse(uazcc)
str(uazcc$variable)
#dput(uazcc$variable)

# breast ----
# plot
# breast x area x all 
breast_plot <- uazcc %>%
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
       title = "Breast Cancer Screening",
       subtitle = "Had a mammogram?",
       caption = "Source: AZ BRFSS") +
  scale_color_discrete(breaks = c("UAZCC Catchment", "Pima County", "Pinal County", "Santa Cruz County", "Yuma County")) +
  theme(legend.position = "bottom", legend.title = element_blank()) 

ggplotly(breast_plot) %>%
  layout(legend = list(orientation = 'h'))

write_rds(breast_plot, "data/tidy/uazcc_brfss_mammogram_plot.rds")

# table
# breast x area x all 
breast_table <- uazcc %>%
  filter(str_detect(uazcc$variable, ".*mammogram.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean") %>%
  select(year, area_name, value) %>%
  pivot_wider(names_from = "area_name", values_from = "value")

write_rds(breast_table, "data/tidy/uazcc_brfss_mammogram_table.rds")

# map 
# mammogram
az_counties <- counties(state = "az")

uazcc_sf <- geo_join(az_counties, uazcc_mapping, by_df = "area_name", by_sp = "NAMELSAD", how = "inner")

uazcc_sf_had_mam <- st_as_sf(
  uazcc_sf,
  crs = "epsg4326") %>%
  filter(str_detect(uazcc_sf$variable, ".*mammogram.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean",
         year == "2018")

uazcc_sf_had_mam_map <- ggplot(uazcc_sf_had_mam) +
  geom_sf(mapping = aes(fill = value)) +
  geom_sf_label(aes(label = NAME)) +
  scale_fill_continuous(name = "", type = "viridis") +
  theme_map() +
  labs(title = "Breast Cancer Screening",
       subtitle = "Had a mammogram? Percentage of responses",
       caption = "Source: 2018 AZ BRFSS") 

write_rds(uazcc_sf_had_mam_map, "data/tidy/uazcc_brfss_mammogram_map.rds")

# cervical ----
# plot 
# cervical x area x all 
cervical_plot <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean") %>%
  ggplot(mapping = aes(x = year, y = value, color = area_name)) +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  #geom_label(aes(label = round(value, digits = 1))) +
  # ylim(40,90) +
  theme_bw() +
  labs(y = "Percentage of Responses",
       x = "Year",
       color = "Area",
       title = "Cervical Cancer Screening",
       subtitle = "Had a pap test?",
       caption = "Source: AZ BRFSS") +
  scale_color_discrete(breaks = c("UAZCC Catchment", "Pima County", "Pinal County", "Santa Cruz County", "Yuma County")) +
  theme(legend.position = "bottom", legend.title = element_blank()) 

ggplotly(cervical_plot) %>%
  layout(legend = list(orientation = 'h'))

write_rds(cervical_plot, "data/tidy/uazcc_brfss_pap_plot.rds")

# table
# cervical x area x all 
cervical_table <- uazcc %>%
  filter(str_detect(uazcc$variable, "Had a Pap.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean") %>%
  select(year, area_name, value) %>%
  pivot_wider(names_from = "area_name", values_from = "value")

write_rds(cervical_table, "data/tidy/uazcc_brfss_pap_table.rds")

# map 
uazcc_sf_had_pap <- st_as_sf(
  uazcc_sf,
  crs = "epsg4326") %>%
  filter(str_detect(uazcc_sf$variable, "Had a Pap.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean",
         year == "2018")

uazcc_sf_had_pap_map <- ggplot(uazcc_sf_had_pap) +
  geom_sf(mapping = aes(fill = value)) +
  geom_sf_label(aes(label = NAME)) +
  scale_fill_continuous(name = "", type = "viridis") +
  theme_map() +
  labs(title = "Cervical Cancer Screening",
       subtitle = "Had a pap test? Percentage of responses",
       caption = "Source: 2018 AZ BRFSS") 

write_rds(uazcc_sf_had_pap_map, "data/tidy/uazcc_brfss_hadpap_map.rds")

# colorectal ----
# plot
crc_plot <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean") %>%
  ggplot(mapping = aes(x = year, y = value, color = area_name)) +
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  #geom_label(aes(label = round(value, digits = 1))) +
  # ylim(40,90) +
  theme_bw() +
  labs(y = "Percentage of Responses",
       x = "Year",
       color = "Area",
       title = "Colorectal Cancer Screening",
       subtitle = "Ever had a sigmoidoscopy or colonoscopy?",
       caption = "Source: AZ BRFSS") +
  scale_color_discrete(breaks = c("UAZCC Catchment", "Pima County", "Pinal County", "Santa Cruz County", "Yuma County")) +
  theme(legend.position = "bottom", legend.title = element_blank()) 

ggplotly(crc_plot) %>%
  layout(legend = list(orientation = 'h'))

write_rds(crc_plot, "data/tidy/uazcc_brfss_crc_plot.rds")

# table
# cervical x area x all 
crc_table <- uazcc %>%
  filter(str_detect(uazcc$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean") %>%
  select(year, area_name, value) %>%
  pivot_wider(names_from = "area_name", values_from = "value")

write_rds(crc_table, "data/tidy/uazcc_brfss_crc_table.rds")

# map 
uazcc_sf_crc <- st_as_sf(
  uazcc_sf,
  crs = "epsg4326") %>%
  filter(str_detect(uazcc_sf$variable, "Ever had sigmoidoscopy.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean",
         year == "2018")

uazcc_sf_crc_map <- ggplot(uazcc_sf_crc) +
  geom_sf(mapping = aes(fill = value)) +
  geom_sf_label(aes(label = NAME)) +
  scale_fill_continuous(name = "", type = "viridis") +
  theme_map() +
  labs(title = "Colorectal Cancer Screening",
       subtitle = "Had a colonoscopy? Percentage of responses",
       caption = "Source: 2018 AZ BRFSS") 

write_rds(uazcc_sf_crc_map, "data/tidy/uazcc_brfss_crc_map.rds")
