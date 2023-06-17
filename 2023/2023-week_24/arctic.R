library(tidyverse)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 11, height = 6.5, units = "in", dpi = 320)

# Source:
# https://nsidc.org/arcticseaicenews/sea-ice-tools/
# https://masie_web.apps.nsidc.org/pub/DATASETS/NOAA/G02135/north/monthly/shapefiles/

# Get list of shapefiles
file_list <- list.files(here::here("2023/2023-week_24/data/"), pattern = "*shp", full.names = TRUE, recursive = TRUE)

# Get CRS of the shapefiles
crs_list <- map(file_list, sf::read_sf)
crs_input <- crs_list %>% 
  map(., ~ sf::st_crs(.x)$input)

# Create dataframe of shapefiles and their CRS, remove those with WGS 84, because they can't be joined with the others
files_crs <- tibble(
  file = file_list,
  crs = crs_input
  ) %>% 
  filter(str_detect(crs, "WGS 84", negate = TRUE))

# Join all shapefiles and extract year and month from their names
shapefile_list <- files_crs$file %>% 
  set_names(basename(.)) %>% 
  map_dfr(., sf::read_sf, .id = "id")
  
arctic <- shapefile_list %>% 
  mutate(
    date = str_extract(id, "\\d+"),
    year = as.integer(str_sub(date, 1, 4)),
    month = as.integer(str_sub(date, 5, 6)),
    month_lab = month.abb[month] %>% fct_inorder()
  )

# Land labels
land <- tribble(
  ~"region", ~"month_lab",	~"longitude",	~"latitude",
  "Alaska", "Mar",	-152,	64,
  "Canada",	"Mar", -110,	60,
  "Greenland", "Mar", -43,	71,
  "Russia",	"Mar", 109,	63
  ) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mutate(month_lab = fct_inorder(month_lab))

# World countries
world <- rnaturalearthdata::countries50 %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs = "+proj=ortho +lat_0=90 +lon_0=0 +x_0=0 +y_0=0")

f1 <- "Outfit"
f2 <- "Charter"

ggplot(arctic) +
  geom_sf(data = world, color = NA, fill = "#F7F3F2") +
  geom_sf(aes(color = year), linewidth = 0.1, fill = "white", alpha = 0.1) +
  shadowtext::geom_shadowtext(data = land, aes(label = region, geometry = geometry), size = 2.5, family = f1, color = "#836C65", stat = "sf_coordinates", bg.color = "#F4F0EF") +
  geom_rect(data = NULL, aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = NA, color = "black", linewidth = 0.2) +
  scale_color_viridis_c(direction = -1, option = "mako", begin = 0.15, name = NULL) +
  coord_sf(xlim = c(-3825000, 3600000), ylim = c(-5025000, 5850000)) +
  facet_wrap(vars(month_lab), nrow = 2) +
  labs(
    title = "Arctic sea ice extent, monthly 1978-2023",
    caption = "Source: National Snow and Ice Data Center · Graphic: Georgios Karamanis",
    fill = "Year"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.key.height = unit(0.25, "lines"),
    legend.key.width = unit(3, "lines"),
    legend.position = "top",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text = element_blank(),
    panel.grid = element_line(linewidth = 0.1, color = "grey70"),
    strip.text = element_text(margin = margin(5, 0, 5, 0), color = "grey20", family = f2, face = "bold"),
    panel.background = element_rect(fill = "#F8FFFF", color = "black", linewidth = 0.2),
    panel.spacing.x = unit(1, "line"),
    panel.spacing.y = unit(0.6, "lines"),
    plot.title = element_text(family = f2, face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0), size = 15),
    plot.caption = element_text(hjust = 0.5, margin = margin(10, 0, 0, 0), color = "grey40", size = 8),
    plot.margin = margin(0, 20, 0, 20)
  )

