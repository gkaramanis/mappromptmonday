library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

# Internal Displacements because of natural disasters in Europe, 2008-2021
# https://www.internal-displacement.org/database/displacement-data
disasters_raw <- readxl::read_xlsx(here::here("2023/2023-week_13/data/IDMC_GIDD_disasters_internal_displacement_data_2021-1680021356363.xlsx")) %>% 
  janitor::clean_names()

top_disasters <- disasters_raw %>% 
  group_by(iso3) %>% 
  filter(disaster_internal_displacements == max(disaster_internal_displacements)) %>% 
  ungroup() %>% 
  distinct(country_territory, hazard_type, year)

disasters <- disasters_raw %>% 
  group_by(iso3) %>% 
  summarise(
    country_territory,
    int_disp = sum(disaster_internal_displacements)
  ) %>% 
  ungroup() %>% 
  distinct() %>% 
  left_join(top_disasters)

world <- rgeoboundaries::gb_adm0()

disasters_sf <- world %>% 
  mutate(shapeISO = case_when(
    shapeName == "Greece" ~ "GRC",
    shapeName == "Kosovo" ~ "XKX",
    TRUE ~ shapeISO
  )) %>% 
  right_join(disasters,  by = c("shapeISO" = "iso3")) %>% 
  filter(!is.na(year))

f1 <- "Outfit"
f2 <- "Rowan"
f3 <- "DIN Condensed"

pal <- rev(MetBrewer::met.brewer("Klimt"))

ggplot(disasters_sf) +
  geom_sf(data = world, color = "black", linewidth = 0.1, fill = "white") +
  geom_sf(aes(fill = hazard_type), color = "white", linewidth = 0.1) +
  geom_sf_text(aes(label = year, size = int_disp), color = "white", family = f3) +
  scale_fill_manual(values = pal) +
  scale_size_continuous(range = c(3, 6), labels = scales::number) +
  coord_sf(xlim = c(-27.5, 47.5), ylim = c(30, 71)) +
  labs(
    title = "Natural Disasters in Europe, 2008-2021",
    subtitle = "Biggest disaster in each country by type and estimated number of people displaced",
    caption = "Source: Internal Displacement Monitoring Centre (IDMC) · Graphic: Georgios Karamanis",
    size = "Number of\npeople displaced",
    fill = "Type of disaster"
  ) +
  guides(size = guide_legend(override.aes = aes(label = "YEAR"))) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.12, 0.5),
    legend.margin = margin(0, 0, 15, 0),
    legend.title = element_text(family = f2, face = "bold"),
    legend.text = element_text(margin = margin(0, 0, 0, 5)),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = "#BFC9E5", color = NA),
    plot.title = element_text(size = 18, face = "bold", family = f2),
    plot.subtitle = element_text(margin = margin(5, 0, 10, 0), size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )
