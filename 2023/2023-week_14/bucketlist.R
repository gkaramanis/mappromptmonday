library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 12, height = 9, units = "in", dpi = 320)

# Data not shared on GitHub!

# Photos.app photos
# sqlite3 -header -csv Photos.sqlite "select ZDATECREATED,ZLONGITUDE,ZLATITUDE from ZASSET;" > photos.csv
photos_raw <- read_csv(here::here("2023/2023-week_14/data/photos.csv")) %>% 
  janitor::clean_names()

photos <- photos_raw %>% 
  mutate(
    date = as_date(as_datetime(zdatecreated, origin = "2001-01-01"))
    ) %>% 
  filter(zlongitude != -180 & zlatitude != -180) %>% 
  select(date, long = zlongitude, lat = zlatitude)

# Lightroom photos
# sqlite3 -header -csv lrcat.db "SELECT id_local, gpsLongitude, gpsLatitude FROM AgHarvestedExifMetadata WHERE AgHarvestedExifMetadata.gpsLongitude IS NOT NULL;" > lr1-photos.csv
# sqlite3 -header -csv lrcat.db "SELECT id_local, captureTime FROM Adobe_images;" > lr2-photos.csv
lr_raw <- read_csv(here::here("2023/2023-week_14/data/lr1-photos.csv")) %>% 
  janitor::clean_names()

lr <- lr_raw %>% 
  filter(!is.na(gps_longitude)) %>% 
  rowwise() %>% 
  mutate(date = as_date(paste(c(date_year, str_pad(date_month, 2, pad = 0), str_pad(date_day, 2, pad = 0)), collapse = "-"))) %>% 
  select(date, long = gps_longitude, lat = gps_latitude)

world <- rgeoboundaries::gb_adm0() %>% 
  st_transform(crs = "+proj=moll") %>% 
  rmapshaper::ms_simplify()

all_photos <- bind_rows(photos, lr) %>% 
  mutate(olivia = date >= as_date("2017-11-30")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = "+proj=moll") %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])


# Binning
world_grid <- st_make_grid(world,
                           n = c(300, 300),
                           what = 'polygons',
                           square = FALSE,
                           flat_topped = TRUE) %>%
  st_as_sf() %>%
  mutate(id = row_number())

joined_sf <- st_join(all_photos, world_grid, join = st_within) %>% 
  group_by(olivia) %>% 
  mutate(total = n()) %>% 
  count(olivia, id, total) %>% 
  st_drop_geometry() %>% 
  mutate(p = n / total)

photos_grid <- world_grid %>% 
  left_join(joined_sf) %>% 
  filter(!is.na(olivia))

f1 <- "Saira"
pal <- rev(MetBrewer::met.brewer("Tam")[2:6])

ggplot(photos_grid) +
  geom_sf(data = world, fill = "#464C56", color = "grey15") +
  geom_sf(aes(fill = p), color = "grey10") +
  scale_fill_stepsn(colors = pal, guide = guide_colorsteps(direction = "horizontal", title.position = "top"), name = "Percentage of geotagged photos") +
  coord_sf(xlim = c(-105, 40)*1e5, ylim = c(35, 80)*1e5) +
  facet_wrap(vars(olivia), ncol = 1, labeller = as_labeller(c(`FALSE` = "Before Olivia", `TRUE` = "Since Olivia"))) +
  labs(
    caption = "Source: Personal data · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.text = element_text(color = "grey99"),
    legend.position = c(0.5, 0.5),
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(2.5, "lines"),
    legend.title = element_text(color = "grey99", hjust = 0.5),
    plot.background = element_rect(fill = "grey5", color = NA),
    strip.text = element_text(color = "grey99", hjust = 0, size = 12),
    panel.spacing.y = unit(4, "lines"),
    plot.caption = element_text(color = "grey99"),
    plot.margin = margin(0, 10, 0, 10)
  )
