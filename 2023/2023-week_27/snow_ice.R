library(tidyverse)
library(sf)
library(marmap)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# https://booksite.elsevier.com/9780444534477/digital_maps.php
lgm <- read_sf(here::here("2023/2023-week_27/data/lgm/lgm.shp"))
alpen <- read_sf(here::here("2023/2023-week_27/data/lgm/lgm_alpen.shp"))
world <- read_sf(here::here("2023/2023-week_27/data/lgm/cntry_98.shp"))


eu_bathy <- getNOAA.bathy(lon1 = -50, lon2 = 90, lat1 = 20, lat2 = 90, resolution = 4)

eu_bathy_proj <- eu_bathy %>%
  marmap::as.raster() %>%
  raster::projectRaster(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") %>%
  terra::rast()

f1 <- "Outfit"

ggplot() +
  tidyterra::geom_spatraster(data = eu_bathy_proj) +
  geom_sf(data = world, fill = NA, linewidth = 0.15) +
  geom_sf(data = lgm, alpha = 0.7, fill = "cyan", color = "cyan", linewidth = 0.5) +
  geom_sf(data = alpen, alpha = 0.7, fill = "cyan", color = "cyan", linewidth = 0.5) +
  # geom_sf(data = mount) +
  scale_fill_etopo() +
  coord_sf(xlim = c(-35, 30) * 1e5, ylim = c(30, 80) * 1e5, expand = FALSE) +
  labs(
    title = "Areas in today's Europe covered by ice sheets during the Last Glacial Maximum",
    subtitle = "",
    caption = "Source: Ehlers, J., Gibbard, P. L., & Hughes, P. D. (Eds.). (2011). Quaternary Glaciations - Extent and Chronology: A Closer Look. Elsevier ·  Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 17),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

