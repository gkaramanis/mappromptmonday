library(tidyverse)
library(tidyterra)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

# https://www.swpc.noaa.gov/products/aurora-30-minute-forecast
aurora <- jsonlite::read_json("https://services.swpc.noaa.gov/json/ovation_aurora_latest.json")

aurora_df <- aurora %>% 
  as_tibble() %>% 
  mutate(id = row_number()) %>% 
  unnest_wider(coordinates, names_sep = "_") %>% 
  rename("x" = 4, "y" = 5, "aurora" = 6)

for_date <- as_datetime(unique(aurora_df$`Forecast Time`))

ortho <- "+proj=ortho +lat_0=45 +lon_0=10"

aurora_r <- aurora_df %>% 
  select(x, y, aurora) %>%
  filter(aurora >= 5) %>% 
  terra::rast(crs = "EPSG:4326") %>% 
  terra::project(ortho) 

world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  st_cast('MULTILINESTRING') %>%
  st_cast('LINESTRING', do_split = TRUE) %>%
  st_cast('POLYGON') %>% 
  st_transform(ortho)

ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6391000) %>%
  st_sfc(crs = ortho)

f1 <- "Outfit"

ggplot() +
  geom_sf(data = ocean, fill = "#041446", color = "grey10") +
  geom_sf(data = world, linewidth = 0.15, fill = "#46342B", color = "grey5") +
  geom_spatraster(data = aurora_r) +
  scale_fill_distiller(palette = "YlGn", direction = -1, na.value = NA, breaks = seq(5, 15, 5)) +
  coord_sf(expand = TRUE) +
  labs(
    title = "Aurora Forecast",
    subtitle = paste("For", for_date, "(UTC)"),
    caption = "Source: Space Weather Prediction Center (NOAA) · Graphic: Georgios Karamanis",
    fill = "Probability\nof Aurora (%)"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.06, 0.14),
    legend.key.width = unit(0.7, "lines"),
    legend.key.height = unit(1, "lines"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    plot.background = element_rect(fill = "grey10", color = NA),
    panel.grid = element_line(),
    panel.background = element_rect(fill = "grey10", color = NA),
    plot.title = element_text(face = "bold", size = 15, color = "grey99"),
    plot.subtitle = element_text(color = "grey99"),
    plot.caption = element_text(color = "grey99"),
    plot.margin = margin(10, 0, 10, 0)
  )
  
