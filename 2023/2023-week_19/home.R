library(tidyverse)
library(sf)
library(geosphere)
library(ggtext)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 8, height = 9, units = "in", dpi = 320)

cities <- tribble(
  ~city,         ~sister_city,     ~country,   ~lat, ~lon,
  "Thessaloniki", "Alexandria",     "Egypt",    31.20009,  29.91874,
  "Thessaloniki", "Blagoevgrad",    "Bulgaria", 42.01156,  23.11496,
  "Thessaloniki", "Bologna",        "Italy",    44.49489,  11.34262,
  "Thessaloniki", "Bratislava",     "Slovakia", 48.14816,  17.10674,
  "Thessaloniki", "Busan",          "South Korea", 35.17955, 129.07564,
  "Thessaloniki", "Cologne",        "Germany",  50.93836,   6.95997,
  "Thessaloniki", "Constana",       "Romania",  44.18063,  28.63429,
  "Thessaloniki", "Dnipropetrovsk", "Ukraine",  48.46472,  35.04618,
  "Thessaloniki", "Dongguan",       "China",    23.02315, 113.75861,
  "Thessaloniki", "Durrs",          "Albania",  41.32379,  19.45448,
  "Thessaloniki", "Guimares",       "Portugal", 41.44177,  -8.29594,
  "Thessaloniki", "Hartford, Connecticut", "USA",  41.76580, -72.67337,
  "Thessaloniki", "Kolkata",        "India",    22.57265,  88.36389,
  "Thessaloniki", "Kor",            "South Korea", 35.84272, 128.56320,
  "Thessaloniki", "Leipzig",        "Germany",  51.33970,  12.37307,
  "Thessaloniki", "Limassol",       "Cyprus",   34.68501,  33.05554,
  "Thessaloniki", "Melbourne",      "Australia", -37.81422, 144.96316,
  "Thessaloniki", "Nice",           "France",   43.71017,   7.26195,
  "Thessaloniki", "Plovdiv",        "Bulgaria", 42.14384,  24.74956,
  "Thessaloniki", "Saint Petersburg", "Russia", 59.93863,  30.31413,
  "Thessaloniki", "San Francisco",  "USA",      37.77493, -122.41942,
  "Thessaloniki", "Tel Aviv",       "Israel",   32.08530,  34.78177,
  "Thessaloniki", "Tianjin",        "China",    39.08415, 117.20078,
  "Uppsala", "Brum", "UK", 52.48624, -1.89040,
  "Uppsala", "Craiova", "Romania", 44.3, 23.8, 
  "Uppsala", "Daejeon", "South Korea", 36.4, 127.4, 
  "Uppsala", "Frederiksberg", "Denmark", 55.7, 12.5, 
  "Uppsala", "Hafnarfjrur", "Iceland", 64.1, -21.9, 
  "Uppsala", "Hmeenlinna", "Finland", 60.99, 24.47, 
  "Uppsala", "Minneapolis", "USA", 44.98, -93.27,  
  "Uppsala", "Tartu", "Estonia", 58.38, 26.71
) %>% 
  mutate(
    from_lon = case_when(
      city == "Thessaloniki" ~ 22.94,
      city == "Uppsala" ~ 17.64
    ),
    from_lat = case_when(
      city == "Thessaloniki" ~ 40.64,
      city == "Uppsala" ~ 59.86
    )
  )

world <- rnaturalearth::ne_countries(scale = 50) %>% 
  st_as_sf() 

gc <- gcIntermediate(
  cities %>% select(from_lon, from_lat),
  cities %>% select(lon, lat),
  n = 10, addStartEnd = TRUE, breakAtDateLine = TRUE, sp = TRUE) %>% 
  st_as_sf() %>% 
  mutate(city = cities$city)

f1 <- "Poppins"

ggplot(cities) +
  geom_point(aes(31, 41), size = 250, color = "grey10", fill = "#F6FBFE", shape = 21) +
  geom_sf(data = world, linewidth = 0.2, fill = "#FBFEF6") +
  geom_point(aes(lon, lat, color = city), size = 1) +
  geom_sf(data = gc, aes(color = city), alpha = 0.7) +
  scale_color_manual(values = c("purple3", "coral3")) +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326)) +
  labs(
    title = "Sister and twin cities of <span style='color:coral3'>**Uppsala**</span> and <span style='color:purple3'>**Thessaloniki**</span>",
    caption = "Source: Sister Cities of the World · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_markdown(hjust = 0.5, size = 18),
    plot.caption = element_text(hjust = 0.5)
  )

