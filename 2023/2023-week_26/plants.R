library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 9.7, height = 8, units = "in", dpi = 320)

world <- rnaturalearthdata::countries50 %>%
sf::st_as_sf()

# https://figshare.com/collections/Chorological_maps_and_data_for_the_main_European_woody_species/2918528/6
pru_mah_range <- read_sf(here::here("2023/2023-week_26/data/Prunus_mahaleb_shp/Prunus_mahaleb_plg_clip.shp"))

pru_mah_iso <- read_sf(here::here("2023/2023-week_26/data/Prunus_mahaleb_shp/Prunus_mahaleb_pnt.shp"))

f1 <- "Ruda"

ggplot() +
  geom_sf(data = world, fill = "#FFFFE8", color = "black", linewidth = 0.15) +
  geom_sf(data = pru_mah_range, fill = "red3", color = "brown", alpha = 0.85) +
  geom_sf(data = pru_mah_iso, color = "red3", alpha = 0.85, shape = 4) +
  coord_sf(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), xlim = c(-32, 60), ylim = c(29, 72)) +
  labs(
    title = "Prunus mahaleb (mahaleb cherry)",
    subtitle = "<span style='color:red3;font-size:16px'>⯀</span>Native range and <span style='color:red3;font-size:16px'>✕ </span>Isolated populations",
    caption = "**Source:** Caudullo, Giovanni; Welk, Erik; San-Miguel-Ayanz, Jesús (2017). Chorological maps and data for the main European<br>woody species figshare. Collection. https:\\/\\/doi.org/10.6084/m9.figshare.c.2918528 · **Graphic:** Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "#D2EDFD", color = NA),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = ggtext::element_markdown(margin = margin(6, 0, 10, 0)),
    plot.caption = ggtext::element_markdown(lineheight = 1.1),
    plot.margin = margin(10, 10, 10, 10)
  )

