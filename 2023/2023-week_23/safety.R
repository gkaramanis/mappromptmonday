library(tidyverse)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_emergency_telephone_numbers"

eu_numbers <- read_html(url) %>% 
  html_table() %>% 
  .[[4]] %>% 
  janitor::clean_names() %>% 
  mutate(across(1:4, function(x) str_remove_all(x, "\\[.+\\]"))) %>%
  mutate(across(1:4, function(x) str_replace_all(x, " or ", "/"))) %>%
  mutate(country = as.character(country))

world <- rgeoboundaries::gb_adm0() %>% 
  janitor::clean_names() %>% 
  mutate(shape_name = case_when(
    shape_name == "Republic of Macedonia" ~ "North Macedonia",
    shape_name == "None" ~ "Switzerland",
    shape_name == "Russian Federation" ~ "Russia",
    TRUE ~ shape_name
  )) %>% 
  filter(shape_name %in% eu_numbers$country) %>% 
  sf::st_make_valid() %>% 
  sf::st_crop(xmin = -50, xmax = 90, ymin = 10, ymax = 85) %>% 
  rmapshaper::ms_simplify(keep = 0.1) 
  
pal <- MetBrewer::met.brewer("Johnson", 5)

eu_map <- world %>% 
  rename(country = shape_name) %>% 
  left_join(eu_numbers) %>% 
  mutate(
    color = case_when(
      str_detect(police, "112") & police != "112" ~ pal[4],
      police == "112" ~ pal[5],
      TRUE ~ pal[3]
    )
  )

f1 <- "DIN Condensed"
f2 <- "American Typewriter"

ggplot(eu_map) +
  geom_sf(aes(fill = I(color)), color = "white") +
  ggrepel::geom_text_repel(aes(label = police, geometry = geometry), family = f1, lineheight = 0.9, stat = "sf_coordinates", min.segment.length = 2, color = "white", bg.color = "black", bg.r = 0.07, fontface = "bold", seed = 112, size = 5) +
  coord_sf(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), xlim = c(-28, 61), ylim = c(35, 79)) +
  labs(
    title = glue::glue("Police emergency number<br><span style='color:{pal[5]}'>**112 only**</span>, <span style='color:{pal[4]}'>**112 and other**</span>, <span style='color:{pal[3]}'>**Other**</span>"),
    caption = "Source: Wikipedia - List of emergency telephone numbers · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = ggtext::element_markdown(margin = margin(0, 0, 10, 0), hjust = 0.5, size = 18, color = "grey40"),
    plot.caption = element_text(hjust = 0.5, color = "grey40", size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )
  
