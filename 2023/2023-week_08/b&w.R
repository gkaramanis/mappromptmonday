library(tidyverse)
library(sf)
library(eurostat)
library(googlesheets4)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# Get spatial data for map
nuts3 <- eurostat_geodata_60_2016 %>% 
  janitor::clean_names() %>% 
  filter(levl_code == 3)  

# Get world coastline, to be used as "background"
world_coast <- read_sf("https://gisco-services.ec.europa.eu/distribution/v2/coas/geojson/COAS_RG_10M_2016_3035.geojson") %>% 
  st_transform(crs = 4326)

dist <- read_sheet("https://docs.google.com/spreadsheets/d/1zp0SrMUdzT-6EvI3S73LUctsLN_jKQIQHRLEb4zlStU/edit#gid=1607027764", sheet = "LONG") %>% 
  janitor::clean_names()

nuts3_dist <- nuts3 %>% 
  left_join(dist)

f1 <- "Futura"
f2 <- "Source Serif Pro"

ggplot(nuts3_dist) +
  annotate("rect", xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf, fill = "grey99", color = "black", linewidth = 0.5) +
  geom_sf(aes(fill = pct_no_station_within_5k), linewidth = 0.1) +
  coord_sf(xlim = c(-27.5, 47.5), ylim = c(30, 71)) +
  scale_fill_steps(low = "grey95", high = "black", na.value = NA, breaks = seq(0, 100, 10), labels = c("0%", "", "20%", "", "40%", "", "60%", "", "80%", "", "100%")) +
  labs(
    title = "Percentage of population without a train station within 5 km",
    subtitle = "By NUTS 3 region, 2019",
    caption = "Source: The European Data Journalism Network · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.8, "lines"),
    plot.background = element_rect(fill = "grey95", color = "black", size = 1),
    plot.title = element_text(family = f2, face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(5, 0, 10, 0), hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )
  
