library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# https://edo.jrc.ec.europa.eu/edov2/php/index.php?id=1000#
drought <- terra::rast(here::here("2023/2023-week_16/data/cdinx_m_euu_20230411_t.tif"))

bbox <- st_bbox(drought)

eu <- rgeoboundaries::gb_adm0() %>% 
  st_transform(crs = st_crs(drought)) %>% 
  st_crop(bbox)

# https://edo.jrc.ec.europa.eu/documents/factsheets/factsheet_combinedDroughtIndicator_v3.pdf
cdi_df <- tribble(
  ~cdi, ~color, ~level, ~interpretation, 
  0, "white", "No drought", "Normal conditions",
  1, "gold", "Watch", "Precipitation deficit",
  2, "orange2", "Warning", "Negative soil moisture anomaly, usually linked with precipitation deficit",
  3, "firebrick2", "Alert", "Negative anomaly of vegetation growth, usually linked with precipitation deficit and negative soil moisture anomaly",
  4, "dodgerblue1", "Full recovery", "After a drought episode, both meteorological conditions and vegetation growth return to normal",
  5, "darkviolet", "Temporary Soil Moisture recovery", "After a drought episode, soil moisture conditions are above the drought threshold but not enough to consider the episode closed",
  6, "darkgreen", "Temporary vegetation recovery", "After a drought episode, vegetation conditions are above the drought threshold but not enough to consider the episode closed",
  7, "grey80", "No data", "No drought data"
  ) %>% 
  mutate(color = fct_inorder(color))

drought_df <- terra::as.data.frame(drought, xy = TRUE) %>% 
  rename(cdi = 3) %>% 
  mutate(cdi = as.integer(cdi)) %>% 
  add_row(x = NA, y = NA, cdi = 1) %>%
  left_join(cdi_df)

f1 <- "Porpora"

ggplot() +
  geom_sf(data = eu, fill = "#E6E4DF") +
  geom_raster(data = drought_df, aes(x, y, fill = color)) +
  geom_sf(data = eu, fill = NA) +
  scale_fill_identity(guide = guide_legend(), labels = str_wrap(paste0(cdi_df$level, ": ",  cdi_df$interpretation), 40)) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Situation of Combined Drought Indicator in Europe - 2nd ten-day period of April 2023",
    caption = "Source: European Drought Observatory · Graphic: Georgios Karamanis",
    fill = ""
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.84, 0.77),
    legend.text = element_text(margin = margin(5, 0, 5, 0)),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "grey80"),
    legend.margin = margin(0, 10, 5, 10),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "#CDE1F2"),
    plot.title = element_text(margin = margin(0, 0, 10, 0), size = 16),
    plot.margin = margin(0, 20, 0, 20)
  )

