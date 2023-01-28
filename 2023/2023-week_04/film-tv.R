library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 11, height = 9, units = "in", dpi = 320)

internet_activities <- eurostat::get_eurostat("isoc_ci_ac_i")

world_coast <- sf::read_sf("https://gisco-services.ec.europa.eu/distribution/v2/coas/geojson/COAS_RG_10M_2016_3035.geojson")

eu <- eurostat::eurostat_geodata_60_2016 %>% 
  filter(LEVL_CODE == 0)

streamed_tv <- internet_activities %>% 
  filter(indic_is == "I_IUSTV") %>% # Streamed TV
  filter(unit == "PC_IND_IU3") %>%  # Percentage who used the internet the last 3 months
  # filter(ind_type == "IND_TOTAL") %>% # All individuals
  filter(time == "2022-01-01") %>% 
  filter(ind_type == "Y16_29" | ind_type == "Y35_44") %>%
  pivot_wider(names_from = ind_type, values_from = values) %>%
  mutate(age_diff = Y35_44 - Y16_29)

eu_stream <- eu %>% 
  left_join(streamed_tv)

f1 <- "Outfit"

ggplot(eu_stream) +
  geom_sf(data = world_coast, fill = "grey96") +
  geom_sf(aes(fill = age_diff)) +
  scale_fill_stepsn(colors = MetBrewer::met.brewer("Hiroshige", direction = -1), breaks = seq(-20, 20, 10), limits = c(-20, 20), labels = abs(seq(-20, 20, 10)), na.value = "grey96") +
  coord_sf(xlim = c(-2600000, 3100000), ylim = c(300000, 4500000), crs = "+proj=aea +lat_0=30 +lon_0=10 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs +type=crs") +
  labs(
    title = "Streaming TV viewership",
    subtitle = "Difference in percentage between age groups that watched streaming TV the last 3 months (2022)",
    caption = "Source: Eurostat (i_iustv, 2022) · Graphic: Georgios Karamanis",
    fill = "more people aged 16-29                   more aged 35-44"
  ) +
  guides(fill = guide_colorsteps(title.position = "top", direction = "horizontal")) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(0.2, 0.95),
    legend.background = element_rect(fill = alpha("grey99", 0.9), color = NA),
    legend.margin = margin(10, 40, 10, 40),
    legend.key.height = unit(0.5, "line"),
    legend.key.width = unit(3.5, "line"),
    legend.title.align = 0.5,
    legend.text = element_text(color = "grey30"),
    legend.title = element_text(color = "grey30"),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(margin = margin(0, 0, 15, 0), size = 13),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

