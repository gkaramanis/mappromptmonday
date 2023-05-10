library(tidyverse)
library(eurostat)
library(sf)
library(patchwork)
library(camcorder)

gg_record(here::here("mappromptmonday-temp"), width = 12, height = 8, dpi = 320)

surg_raw <- get_eurostat("hlth_co_proc2")

countries <- bind_rows(eu_countries, ea_countries, efta_countries, eu_candidate_countries) %>% 
  distinct() %>% 
  add_row(code = "UK", name = "United Kingdom", label = "United Kingdom")

surg <- surg_raw %>% 
  filter(icha_hc == "TOT_PAT" & unit == "P_HTHAB") %>% 
  group_by(geo) %>% 
  filter(time == max(time)) %>% 
  filter(values == max(values)) 

surg_labels <- label_eurostat(surg) 

surg_map <- surg %>% 
  left_join(countries, by = c("geo" = "code")) %>% 
  left_join(surg_labels, by = c("label" = "geo")) %>% 
  right_join(get_eurostat_geospatial(nuts_level = 0)) %>% 
  filter(!is.na(icd9cm.y)) %>% 
  mutate(icd9cm.y = str_wrap(str_remove(icd9cm.y, "\\s*\\(.*?\\)"), 20))

st_geometry(surg_map) <- surg_map$geometry

world_coast <- sf::read_sf("https://gisco-services.ec.europa.eu/distribution/v2/coas/geojson/COAS_RG_10M_2016_3035.geojson")

liec <- surg_map %>% 
  filter(geo == "LI") %>% 
  st_centroid() %>% 
  st_buffer(rect_sf, dist = 100e3)


f1 <- "Outfit"

m <- ggplot(surg_map) +
  geom_sf(data = world_coast) +
  geom_sf(aes(fill = icd9cm.y), linewidth = 0.1, color = "grey10") +
  geom_sf(data = liec, fill = NA, color = MetBrewer::met.brewer("Klimt")[4], linewidth = 0.6) +
  coord_sf(crs = "+proj=aea +lat_1=30 +lat_2=60 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", default_crs = sf::st_crs(4326), xlim = c(-22, 50), ylim = c(31, 71)) +
  MetBrewer::scale_fill_met_d("Klimt") +
  labs(
    title = "Most common surgery performed by country",
    fill = NULL
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    legend.position = c(0.85, 0.65),
    legend.text = element_text(lineheight = 0.8),
    legend.key.height = unit(1.5, "lines"),
    legend.key.width = unit(0.7, "lines"),
    plot.title = element_text(size = 20, face = "bold", margin = margin(0, 0, 20, 0)),
    plot.margin = margin(0, 0, 0, 10)
  )


p <- surg_map %>% 
  select(name, values.x, icd9cm.y, time.x) %>% 
  mutate(
    name = fct_reorder(name, values.x),
    year = year(time.x)
    ) %>% 
  ggplot() +
  geom_col(aes(name, values.x, fill = icd9cm.y), width = 0.8) +
  geom_label(aes(name, -20, label = paste0(name, " (", year, ")")), hjust = 1, family = f1, size = 3, label.size = 0) +
  MetBrewer::scale_fill_met_d("Klimt") +
  coord_flip(clip = "off") +
  scale_y_continuous(position = "right") +
  labs(
    title = "Surgeries per 100,000 (year of data)",
    caption = "Source: Eurostat · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 20, 0, 100),
    plot.title = element_text(hjust = 0, size = 11, margin = margin(0, 0, 0, 0)),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )
  

m + p +
  plot_layout(widths = c(3, 1)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "grey99")
    )
  )

