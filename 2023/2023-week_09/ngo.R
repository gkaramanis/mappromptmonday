library(tidyverse)
library(sf)
library(ggtext)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 9.2, height = 7, units = "in", dpi = 320)

# Source: Food and Agriculture Organization of the United Nations
# https://www.fao.org/3/i6338e/i6338e.pdf
tree_area <- read_sf(here::here("2023/2023-week_09/data/Taxus_baccata/Taxus_baccata_area.shp"))
tree_countries <- read_sf(here::here("2023/2023-week_09/data/Taxus_baccata/Taxus_baccata_countries.shp"))
tree_locations <- read_sf(here::here("2023/2023-week_09/data/Taxus_baccata/Taxus_baccata_locations.shp"))

# Get world coastline, to be used as "background"
world_coast <- sf::read_sf("https://gisco-services.ec.europa.eu/distribution/v2/coas/geojson/COAS_RG_10M_2016_3035.geojson")

# Fonts
f1 <- "Canela Text"

ggplot() +
  geom_sf(data = world_coast, fill = "grey15", color = NA) +
  geom_sf(data = tree_countries, fill = "#060D06", color = "grey30", linewidth = 0.1) + 
  geom_sf(data = tree_area, fill = "#505720") + 
  geom_sf(data = tree_locations, color = "#BDC76D", size = 0.2) +
  annotate("richtext", label = "<span style='color:grey97;font-size:18px'>**Geographic distribution<br>of the English yew<br>(Taxus baccata)**</span><br><br><span style='color:#C3CD74'>··  Known distribution</span><br><span style='color:#292E00'>·· Presumed native distribution</span><br><span style='color:#060D06'>·· Countries of presumed native distribution</span><br><br><span style='font-size:10px;color:black'>Source: Food and Agriculture Organization of the<br>United Nations · Graphic: Georgios Karamanis</span>", x = 3.3e6, y = 4.8e6, family = f1, fill = alpha("grey92", 0.15), hjust = 0, label.padding = unit(c(0.5, 0.75, 0.5, 0.75), "lines"), label.color = NA, size = 3, label.r = unit(0.5, "lines")) +
  annotate("point", x = 3.49e6, y = c(4.815e6, 4.66e6, 4.5e6), size = c(1.6, 2, 2), shape = c(19, 15, 15), color = c("#BDC76D", "#505720", "#060D06")) +
  coord_sf(xlim = c(-3000000, 6100000), ylim = c(-1000000, 5900000), crs = "+proj=aea +lat_0=30 +lon_0=10 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs +type=crs") +
  labs(
    # title = "Geographic distribution of the English yew (Taxus baccata)",
    # subtitle = "<span style='color:#BDC76D'>● Known distribution</span> <span style='color:#505720'>◼ Presumed native distribution</span><span style='color:#060D06'>◼ Countries of presumed native distribution</span>",
    # caption = "Source: Food and Agriculture Organization of the United Nations · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#4E5451", color = NA),
    # plot.title = element_text(color = "grey99", face = "bold"),
    # plot.subtitle = element_markdown(),
    plot.caption = element_text(color = "grey90"),
    plot.margin = margin(5, 5, 5, 5)
  )
  
