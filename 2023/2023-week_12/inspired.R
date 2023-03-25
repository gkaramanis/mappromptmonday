library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 13, height = 8, units = "in", dpi = 320)

# Inspiration
# https://twitter.com/MapsbyW/status/1623348572929536000/
# Tutorial
# https://twitter.com/TmarcoH/status/1611156932856893441

tempr <- terra::rast(here::here("2023/2023-week_12/data/20230323_0800.nc4"))
terra::crs(tempr) <- "EPSG:4326"

proj_string <- "+proj=ortho +lat_0=45 +lon_0=10"

tempr_df <- tempr[[5]] %>% # surface air temperature in K
  terra::project(proj_string) %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  rename(t = 3) %>% 
  # K to C
  mutate(tc = t - 273.15)

world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  st_cast('MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split = TRUE) %>%
  st_cast('POLYGON') %>% 
  st_transform(proj_string) 

pal <- c(scico::scico(palette = "oslo", n = 6)[1:5], "#EBEED9", scico::scico(palette = "lajolla", n = 6)[2:6])

f1 <- "Outfit"

m <- ggplot() +
  geom_raster(data = tempr_df, aes(x, y, fill = tc)) +
  geom_sf(data = world, fill = NA, color = "grey10", linewidth = 0.15) +
  scale_fill_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  coord_sf(xlim = c(-75, 75) * 1e5, ylim = c(-20, 65) * 1e5) +
  labs(
    title = "Global surface temperature average, March 23, 2023, 08:00",
    caption = "Source: GMAO/NASA · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(color = "grey95", size = 16, hjust = 0.05),
    plot.caption = element_text(color = "grey95", size = 12)
  )

# North Pole
np_proj_string <- "+proj=ortho +lat_0=90 +lon_0=30"

np_tempr_df <- tempr[[5]] %>% # surface air temperature in K
  terra::project(np_proj_string) %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  rename(t = 3) %>% 
  # K to C
  mutate(tc = t - 273.15)

np_world <- world %>%  
  st_make_valid() %>% 
  st_transform(np_proj_string) 

np <- ggplot() +
  geom_raster(data = np_tempr_df, aes(x, y, fill = tc)) +
  geom_sf(data = np_world, fill = NA, color = "grey10", linewidth = 0.15) +
  scale_fill_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  labs(
    caption = "North Pole"
  ) +
  coord_sf() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.caption = element_text(family = f1, color = "grey90", hjust = 0.5, size = 12)
  )

# South Pole
sp_proj_string <- "+proj=ortho +lat_0=-90 +lon_0=30"

sp_tempr_df <- tempr[[5]] %>% # surface air temperature in K
  terra::project(sp_proj_string) %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  rename(t = 3) %>% 
  # K to C
  mutate(tc = t - 273.15)

sp_world <- world %>%  
  st_make_valid() %>% 
  st_transform(sp_proj_string) 

sp <- ggplot() +
  geom_raster(data = sp_tempr_df, aes(x, y, fill = tc)) +
  geom_sf(data = sp_world, fill = NA, color = "grey10", linewidth = 0.15) +
  scale_fill_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  labs(
    caption = "South Pole"
  ) +
  coord_sf() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.caption = element_text(family = f1, color = "grey90", hjust = 0.5, size = 12)
  )

# Final image
m +
  inset_element(np, 0, 0.6, 0.2, 1) +
  inset_element(sp, 0.8, 0.6, 1, 1) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "grey10", color = NA),
      plot.margin = margin(5, 1, 5, 1)
    )
  )
