library(tidyverse)
library(data.table)
library(ggfx)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 8.2, units = "in", dpi = 320)

# library(marmap)
# bathy <- getNOAA.bathy(-12, 40, 30, 72)
# bathy_df <- fortify.bathy(bathy)
# write.csv(bathy_df, here::here("2023/2023-week_11/data/eu_bathy.csv"))

eu_bathy <- read_csv(here::here("2023/2023-week_11/data/eu_bathy.csv")) %>% 
  select(-1)

# https://eliocamp.github.io/codigo-r/2018/02/how-to-make-shaded-relief-in-r/
eu_bathy_dt <- as.data.table(eu_bathy)
eu_bathy_dt[, c("dx", "dy") := metR::Derivate(z ~ x + y)]
eu_bathy_dt[, angle := atan2(-dy, -dx)]
sun.angle <- pi/4

f1 <- "Outfit"

ggplot() +
  as_reference(
    geom_raster(data = eu_bathy_dt, aes(x, y, fill = if_else(z >= 0, cos(angle + sun.angle), 0))),
    id = "sun_angle"
  ) +
    scale_fill_gradient2(low = "white", high = "white", mid = "gray60", midpoint = sun.angle) +
  ggnewscale::new_scale_fill() +
  with_blend(
    geom_raster(data = eu_bathy, aes(x, y, fill = z)),
    bg_layer = "sun_angle",
    blend_type = "multiply"
  ) +
  marmap::scale_fill_etopo() +
  coord_fixed(expand = FALSE) +
  labs(
    caption = "Source: NOAA · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = NA),
    plot.caption = element_text(color = "grey97"),
    plot.margin = margin(20, 0, 10, 0)
  )
