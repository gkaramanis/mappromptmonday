library(tidyverse)
library(ggpattern)
library(patchwork)
library(camcorder)

gg_record(dir = "mappromptmonday-temp/", device = "png", width = 20, height = 10, units = "in", dpi = 320)

# https://www.worldclim.org/data/worldclim21.html
# Lowest temp of coldest month, 1970-2000, bio6 5m
wc_high <- terra::rast(here::here("2023/2023-week_06/data/wc2.1_5m_bio_5.tif"))
# Highest temp of warmest month, 1970-2000, bio5 5m
wc_low <- terra::rast(here::here("2023/2023-week_06/data/wc2.1_5m_bio_6.tif"))

proj_string <- "+proj=ortho +lat_0=50 +lon_0=30"

wc_high_df <- wc_high %>% 
  terra::project(proj_string) %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  rename(value = 3)

wc_low_df <- wc_low %>% 
  terra::project(proj_string) %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  rename(value = 3)

globe <- sf::st_point(x = c(0,0)) %>%
  sf::st_buffer(dist = 6391000) %>%
  sf::st_sfc(crs = proj_string)

pal <- MetBrewer::met.brewer("OKeeffe1")

f1 <- "Outfit"

plot_globe <- function(df, legend) {
  ggplot() +
  geom_sf_pattern(data = globe, pattern_fill = "#424144", pattern_fill2 = "black", color = NA, pattern = "gradient") +
  geom_raster(data = df, aes(x, y, fill = value)) +
  scale_fill_gradientn(colors = rev(pal), rescaler = ~ scales::rescale_mid(.x, mid = 0), guide = guide_colorbar(direction = "horizontal", title.position = "top"), breaks = c(-40, -20, 0, 20), labels = c("-40", "-20", "0°C", "20")) +
  labs(
    fill = legend
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.86, 0.93),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white", hjust = 0.5),
    plot.background = element_rect(fill = "grey15", color = NA)
  )
}

w <- plot_globe(wc_high_df, "Highest temperature of\nwarmest month, 1970-2000")
c <- plot_globe(wc_low_df, "Lowest temperature of\ncoldest month, 1970-2000")


c + w &
  theme(plot.background = element_rect(fill = "grey15", color = NA))
