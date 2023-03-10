library(tidyverse)
library(sf)
library(rvest)
library(ggtext)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# https://cooperpetcare.com/pet-statistics/
dogcats_raw <- read_html("https://cooperpetcare.com/pet-statistics/") %>% 
  html_table() %>% 
  .[[1]]
  
dogcats <- dogcats_raw %>% 
  janitor::clean_names() %>% 
  mutate(
    across(2:4, function(x) as.numeric(str_remove_all(x, "\\."))),
    cat_ratio = round(cats / dogs, 2),
    admin = case_when(
      country == "Czech Republic" ~ "Czechia",
      TRUE ~ country
    )
    ) 

eu <- read_sf(here::here("2023/2023-week_10/data/europe.geo.json")) 

world_coast <- sf::read_sf("https://gisco-services.ec.europa.eu/distribution/v2/coas/geojson/COAS_RG_10M_2016_3035.geojson")

eu_dogcats <- eu %>% 
  select(admin, pop_est) %>% 
  left_join(dogcats) %>% 
  mutate(
    pets_pc = pets / pop_est,
    centroid = st_centroid(geometry, of_largest_polygon = TRUE),
    label = case_when(
      country == "Luxembourg" |  country == "Croatia" ~ paste0("\n\n", admin, "\n", cat_ratio),
      TRUE ~ paste0(admin, "\n", cat_ratio)
    ) 
    ) 

f1 <- "DIN Condensed"
f2 <- "Outfit"

ggplot(eu_dogcats) +
  geom_sf(data = world_coast, fill = "grey90", color = NA) +
  geom_sf(aes(fill = cat_ratio), color = "white") +
  geom_sf_text(data = . %>% filter(!is.na(cat_ratio)), 
               aes(geometry = centroid,
                   label = label,
                   size = pets_pc), 
               lineheight = 0.85, family = f1, color = "grey10") +
  scale_fill_stepsn(colors = MetBrewer::met.brewer("Morgenstern", direction = -1), na.value = "grey90", limits = c(0.5, 3.5), rescaler = ~ scales::rescale_mid(.x, mid = 1), labels = c("0.5\n(2 times\nmore dogs)", 1, "", 2, "", "3 times\nmore cats", "")) +
  scale_size_continuous(range = c(3, 5), guide = "none") +
  coord_sf(xlim = c(-24, 51), ylim = c(28, 74)) +
  labs(
    fill = "**Cats to dogs ratio**<br><span style='font-size:12px'>Size of country label corresponds to pets per capita</span>",
    caption = "Source: Cooper Pet Care · Graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_colorsteps(title.position = "top")) +
  theme_void(base_family = f2) +
  theme(
    legend.position = c(0.27, 0.91),
    legend.title.align = 0.5,
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title = element_markdown(size = 16, margin = margin(20, 0, 0, 0)),
    legend.text = element_text(size = 12),
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    plot.margin = margin(5, 5, 5, 5)
  )
  