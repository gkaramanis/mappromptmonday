library(tidyverse)
library(suncalc)
library(lutz)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

tz_sf <- read_sf("/Users/georgios/SynologyDrive/R/tidytuesday/2023/2023-week_13/data/tz.geojson") %>% 
  rmapshaper::ms_simplify()

tz_sf_eu <- tz_sf %>% 
  st_crop(xmin = -30, xmax = 62, ymin = 15, ymax = 80)

tz_v <- terra::vect(tz_sf_eu)
tz_r <- terra::rast(tz_v, 1000, 1000)
tz_rr <- terra::rasterize(tz_v, tz_r, field = "TZID")

tz_df <- tz_rr %>% 
  terra::as.data.frame(xy = TRUE) 

tz_sun <- tz_df %>% 
  mutate(date = as.Date("2023-06-21")) %>% 
  rename(lon = x, lat = y) %>% 
  mutate(sunset = getSunlightTimes(data = ., keep = "sunsetStart")$sunsetStart) %>% 
  mutate(tz = tz_lookup_coords(lat = lat, lon = lon, method = "accurate")) %>% 
  rowwise() %>% 
  mutate(
    local_time = format(as.POSIXct(sunset, tz = "UTC", usetz = TRUE), tz = tz, usetz = TRUE, format = "%H:%M:%S"),
    local_hour = str_sub(local_time, 1, 2),
    local_minute = str_sub(local_time, 4, 5),
    hour_label = paste0(local_hour, ":00-", local_hour, ":59")
    )

tz_sun$hour_label <- factor(tz_sun$hour_label, c("16:00-16:59", "18:00-18:59", "19:00-19:59", "20:00-20:59", "21:00-21:59", "22:00-22:59", "23:00-23:59", "00:00-00:59"))

tz_sun_sf <- tz_sun %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

f1 <- "Outfit"
f2 <- "Fantasque Sans Mono"

ggplot() +
  geom_sf(data = tz_sun_sf, aes(color = hour_label), size = 0.2, shape = 15) +
  geom_sf(data = tz_sf, fill = NA, color = "black", linewidth = 0.1) +
  scale_color_brewer(palette = "RdPu", na.value = "grey80") +
  coord_sf(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), xlim = c(-22, 45), ylim = c(28, 71)) +
  guides(color = guide_legend(override.aes = list(size = 6), title = "Sunset time on\nJune 21, 2023")) +
  labs(
    caption = "Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.07, 0.75),
    legend.text = element_text(family = f2, size = 11),
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "grey99", color = NA)
  )
  
