library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 8.5, units = "in", dpi = 320)

elev <- terra::rast(here::here("2023/2023-week_15/data/elevation1x1_new.tif"))

bbox <- st_bbox(elev)

world <- rgeoboundaries::gb_adm0() %>% 
  st_transform(crs = st_crs(elev)) %>% 
  st_crop(bbox)
  
f1 <- "Saira"

ggplot() +
  geom_sf(data = world, fill = "white", color = NA) +
  tidyterra::geom_spatraster_contour(data = elev, aes(color = after_stat(level)), linewidth = 0.15, binwidth = 20) +
  scico::scale_color_scico(palette = "batlowK", direction = 1, end = 0.8, begin = 0.2, labels = c("", "🡑 Height", "", "", "")) +
  coord_sf(expand = FALSE) +
  labs(
    caption = "Source: European Environment Agency · Graphic: Georgios Karamanis",
    color = "Height"
    ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(0.08, 0.75),
    legend.key.width = unit(0.7, "lines"),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "#B4C2D9", color = NA),
    axis.title = element_blank(),
    panel.grid = element_line(color = "grey95", linewidth = 0.3),
    plot.caption = element_text(color = "grey30")
  )
