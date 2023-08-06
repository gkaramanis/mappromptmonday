library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

# In thousand euros 
# https://www.trademap.org/Country_SelProduct_TS.aspx?nvpm=1%7c%7c25%7c%7c%7c1806%7c%7c%7c4%7c1%7c1%7c3%7c2%7c1%7c3%7c1%7c1%7c1
chocolate <- read_tsv(here::here("2023/2023-week_29/data/Trade_Map_-_List_of_markets_for_the_selected_product_(Chocolate_and_other_food_preparations_containing_cocoa).txt")) %>% 
  janitor::clean_names()
  
choc_eu <- chocolate %>% 
  select(country = partners, balance_in_value_in_2022) %>% 
  filter(!country %in% c("World", "Europe Aggregation")) %>% 
  mutate(
    country = case_when(
      # country == "Czech Republic" ~ "Czechia",
      country == "Macedonia, North" ~ "Macedonia",
      country == "Russian Federation" ~ "Russia",
      country == "Serbia" ~ "Republic of Serbia",
      country == "Moldova, Republic of" ~ "Moldova",
      country == "Libya, State of" ~ "Libya",
      country == "Türkiye" ~ "Turkey",
      country == "Syrian Arab Republic" ~ "Syria",
      country == "Iran, Islamic Republic of" ~ "Iran",
      TRUE ~ country
    ),
    # Million euros
    balance_m = round(balance_in_value_in_2022/1000) 
  )

world_sf <- rnaturalearthdata::countries50 %>% 
  st_as_sf()

choc_eu_sf <- world_sf %>% 
  left_join(choc_eu, by = c("sovereignt" = "country")) %>% 
  filter(!is.na(balance_in_value_in_2022))

f1 <- "Outfit"
f2 <- "Publico Headline"

ggplot(choc_eu_sf) +
  geom_sf(data = world_sf, color = "grey99", fill = "#D4D0CE") +
  geom_sf(data = . %>% filter(continent == "Europe"), aes(fill = balance_m)) +
  # Top 5 import - export (Right map) 
  shadowtext::geom_shadowtext(data = . %>% filter(admin %in% (choc_eu %>% slice_max(order_by = balance_m, n = 5) %>% pull(country))), aes(geometry = geometry, label = scales::number(balance_m), size = abs(balance_m)), color = "black", family = f1, stat = "sf_coordinates", bg.color = "grey99") +
  # Bottom 5 import - export (Left map)
  shadowtext::geom_shadowtext(data = . %>% filter(admin %in% (choc_eu %>% slice_min(order_by = balance_m, n = 5) %>% pull(country))), aes(geometry = geometry, label = scales::number(balance_m), size = abs(balance_m)), color = "black", family = f1, stat = "sf_coordinates", bg.color = "grey99") +
    # Nudge label for Norway (bottom 5)
  shadowtext::geom_shadowtext(data = . %>% filter(admin == "Norway"), aes(geometry = geometry, label = scales::number(balance_m), size = abs(balance_m)), color = "black", family = f1, stat = "sf_coordinates", bg.color = "grey99", nudge_y = -19, nudge_x = -13) +
  coord_sf(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=15 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), xlim = c(-22, 52.5), ylim = c(30, 75)) +
  scale_fill_distiller(palette = "BrBG", rescaler = ~ scales::rescale_mid(.x, mid = 0), guide = guide_colorbar(title.position = "top", title.hjust = 0), labels = scales::number_format()) +
  scale_size_continuous(range = c(3, 4), guide = FALSE) +
  facet_wrap(vars(balance_in_value_in_2022 > 0)) +
  labs(
    title = "Chocolate imports and exports",
    subtitle = "Balance in million euros between imported and exported value for chocolate and other food preparations containing cocoa (2022).",
    caption = "Source: ITC Trade Map · Graphic: Georgios Karamanis",
    fill = paste0("More imports", strrep(" ", 29), "More exports")
  ) +
  theme_void(base_family = f1, base_size = 12) +
  theme(
    legend.position = "top",
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.margin = margin(20, 0, 10, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "#F1F4F8", color = NA),
    strip.text = element_blank(),
    plot.margin = margin(0, 20, 0, 20),
    plot.title = element_text(face = "bold", family = f2, size = 18),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )

