library(tidyverse)
library(sf)
library(string2path)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 7, height = 10, units = "in", dpi = 320)

eu <- read_sf(here::here("2023/2023-week_10/data/europe.geo.json"))
coast <- read_sf("/Users/georgios/SynologyDrive/R/30daymapchallenge/2022/data/ne_10m_land")

nuts_area <- eu %>% 
  rmapshaper::ms_simplify(keep = 0.1)
  
nuts_smooth <- nuts_area %>% 
  sfdct::ct_triangulate()

eu_text <- string2path("EUROPE", font = "DIN Condensed") %>% 
  mutate(
    x = x * 15 + 3,
    y = y * 15 + 18
    ) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  group_by(glyph_id) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_cast("POLYGON") %>% 
  ungroup() %>% 
  rmapshaper::ms_simplify(0.25) %>%
  sfdct::ct_triangulate() 

ggplot(nuts_smooth) +
  geom_sf(data = coast, fill = NA) +
  geom_sf(fill = NA, linewidth = 0.1) +
  geom_sf(data = eu_text, fill = "grey99") +
  scale_color_viridis_d(option = "turbo") +
  coord_sf(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), xlim = c(-22, 52.5), ylim = c(16, 85)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99")
  )

