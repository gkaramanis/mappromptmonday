library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "mappromptmonday-temp", device = "png", width = 9, height = 8, units = "in", dpi = 320)

livestock_raw <- eurostat::get_eurostat("agr_r_animal")

# Thousand heads!
livestock <- livestock_raw %>% 
  filter(time == as.Date("2021-01-01")) %>% 
  # filter(str_length(geo) > 3) %>% 
  mutate(animal_group = str_sub(animals, 1, 2)) %>% 
  group_by(geo, animal_group) %>% 
  summarise(total_values = sum(values, na.rm = TRUE)) %>% 
  slice_max(order_by = total_values, n = 1) %>% 
  ungroup() %>% 
  mutate(
    animal_label = case_when(
      animal_group == "A2" ~ "Bovine",
      animal_group == "A3" ~ "Swine",
      animal_group == "A4" ~ "Sheep/Goats",
      TRUE ~ animal_group
    )
  )

eu <- eurostat::eurostat_geodata_60_2016

# NUTS 2
livestock_nuts2 <- eu %>% 
  filter(LEVL_CODE == 2) %>% 
  left_join(livestock)

# Germany NUTS 1
livestock_nuts1 <- eu %>% 
  filter(LEVL_CODE == 1 & CNTR_CODE == "DE") %>% 
  left_join(livestock)

cntr_brd <- eu %>% 
  filter(LEVL_CODE == 0)

f1 <- "Outfit"

ggplot() +
  geom_sf(data = livestock_nuts2, aes(fill = animal_label), color = "white") +
  geom_sf(data = livestock_nuts1, aes(fill = animal_label), color = "white") +
  geom_sf(data = cntr_brd, fill = NA, color = "black", linewidth = 0.1) +
  scale_fill_manual(values = MetBrewer::met.brewer("Egypt")[c(4, 2, 1)], breaks = c("Bovine", "Sheep/Goats", "Swine"), na.value = "grey90") +
  coord_sf(xlim = c(-22.5, 45), ylim = c(32, 70.5)) +
  labs(
    caption = "*NUTS 1 for Germany · Source: Eurostat (agr_r_animal, 2021) · Graphic: Georgios Karamanis",
    fill = "Most common\nlivestock by\nNUTS 2 region*"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.9, 0.52),
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 12),
    plot.background = element_rect(fill = "grey98", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )
  

