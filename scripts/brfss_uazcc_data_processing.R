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

# GIS
az_counties <- counties(state = "az")

uazcc_sf <- geo_join(az_counties, uazcc, by_df = "area_name", by_sp = "NAMELSAD", how = "inner")

uazcc_sf <- inner_join(uazcc, az_counties, by = c("area_name" = "NAMELSAD"))
  
uazcc_sf <- st_as_sf(
  uazcc_sf,
  crs = "epsg4326") %>%
  filter(str_detect(uazcc_sf$variable, "Had a Pap.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean")

uazcc_sf <- as(uazcc_sf, "Spatial")

uazcc_sf %>%
  filter(str_detect(uazcc_sf$variable, "Had a Pap.*")) %>%
  filter(demographic == "All Respondents",
         statistic == "Mean") %>%
  leaflet() %>%
  addProviderTiles("Stamen.Tonerlite") %>%
  addPolygons()

prj <- "+proj=longlat +datum=WGS84"

mapview(uazcc_sf)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = uazcc)