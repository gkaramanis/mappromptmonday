library(tidyverse)
library(ggforce)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

ch_raw <- eurostat::get_eurostat("nrg_chddr2_a")

ch <- ch_raw %>% 
  filter(str_length(geo) == 4) %>% 
  filter(time == "2022-01-01")

eu <- eurostat::get_eurostat_geospatial(nuts_level = 2, year = 2021, resolution = 03) %>% 
  select(geo)

world <- rnaturalearthdata::countries50 %>% 
  st_as_sf() %>% 
  janitor::clean_names() %>% 
  filter(continent %in% c("Europe", "Asia", "Africa"))

ch_w <- eu %>% 
  left_join(ch) %>% 
  pivot_wider(names_from = "indic_nrg", values_from = "values") %>%
  mutate(cntr = st_centroid(geometry)) %>% 
  rowwise() %>% 
  mutate(
    x = st_coordinates(cntr)[1],
    y = st_coordinates(cntr)[2]
    ) %>% 
  filter(!is.na(HDD) & !is.na(CDD))

f1 <- "Outfit"

ggplot(ch_w) +
  geom_sf(data = world, linewidth = 0.1, color = NA, fill = "grey94") +
  geom_sf(aes(geometry = geometry), fill = "grey90", linewidth = 0.2, color = "white") +
  geom_point(aes(x - 0.25, y, size = HDD), color = "#FFA630", shape = "⬆") +
  geom_point(aes(x + 0.25, y, size = CDD), color = "#0474BA", shape = "⬇") +
  scale_size_continuous(name = "<span style='color:#FFA630'>Heating</span> and <span style='color:#0474BA'>cooling</span> degree days<br>by NUTS 3 region, 2022", labels = scales::number, breaks = seq(0, 6, 2) * 1e3) +
  coord_sf(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), xlim = c(-22.5, 45), ylim = c(31, 71)) +
  guides(size = guide_legend(override.aes = list(shape = "⬆", color = "grey30"))) +
  labs(
    caption = "Data: Eurostat · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.text = element_text(margin = margin(0, 0, 0, 20), size = 12),
    legend.position = c(0.22, 0.85),
    legend.title = ggtext::element_markdown(lineheight = 1.3, face = "bold", margin = margin(0, 0, 5, 0), size = 13),
    plot.background = element_rect(fill = "grey99", color = NA)
  )


