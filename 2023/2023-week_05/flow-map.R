library(tidyverse)
library(camcorder)
library(edgebundle)
library(igraph)

gg_record(dir = "mappromptmonday-temp/", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# mov <- read_csv(here::here("2023/2023-week_05/data/000005TU_20230115-164302.csv")) %>%
#   janitor::clean_names()

# mov_geo <- mov %>% 
#   # head() %>% 
#   tidygeocoder::geocode(country = country_of_emi_immigration)
# 
# write_csv(mov_geo, here::here("2023/2023-week_05/data/move_geo.csv"))

mov_geo <- read_csv(here::here("2023/2023-week_05/data/move_geo.csv")) %>% 
  filter(immigrations_2021 > 0) %>% 
  add_row(country_of_emi_immigration = "Sweden", lat = 60, long = 16) %>%
  filter(!is.na(lat)) %>% 
  filter(
    between(long, -25.5, 37.5) & 
    between(lat, 32, 72)
  )

immigr <- mov_geo %>% 
  select(x = long, y = lat, n = immigrations_2021)

relations <- mov_geo %>% 
  mutate(
    from = country_of_emi_immigration,
    to = "Sweden"
  ) %>% 
  select(from, to)

ig <- graph_from_data_frame(relations, directed = TRUE, vertices = mov_geo)

xy <- cbind(V(ig)$long, V(ig)$lat)

verts <- data.frame(x = V(ig)$long, y = V(ig)$lat) %>% 
  left_join(immigr)

fbundle <- edge_bundle_force(ig, xy, compatibility_threshold = 0.8) %>% 
  filter(group != 191) %>% 
  left_join(immigr) %>% 
  group_by(group) %>% 
  fill(n) %>% 
  ungroup()

world <- map_data("world")

f1 <- "Outfit"


ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey90") +
  annotate("text", x = 16, y = 62, label = "SWEDEN", size = 5, family = f1, fontface = "bold") +
  geom_path(data = fbundle %>% filter(n < 1000), aes(x, y, group = group, linewidth = n), alpha = 0.2) +
  geom_path(data = fbundle %>% filter(n >= 1000), aes(x, y, group = group, linewidth = n, color = n), alpha = 0.8) +
  geom_point(data = verts, aes(x, y, color = ifelse(n > 1000, n, NA), size = n)) +
  shadowtext::geom_shadowtext(data = mov_geo %>% filter(immigrations_2021 < 1000), aes(long, lat - 0.75, label = paste0(country_of_emi_immigration, "\n", immigrations_2021)), stat = "unique", check_overlap = TRUE, family = f1, vjust = 1, size = 2, color = "black", bg.color = "white", lineheight = 0.9) +
  shadowtext::geom_shadowtext(data = mov_geo %>% filter(immigrations_2021 >= 1000), aes(long, lat - 0.75, label = paste0(country_of_emi_immigration, "\n", scales::number(immigrations_2021))), stat = "unique", check_overlap = TRUE, family = f1, vjust = 1, size = 4, color = "black", bg.color = "white", lineheight = 0.9) +
  scale_color_stepsn(colors = MetBrewer::met.brewer("Tam"), na.value = "grey70") +
  scale_linewidth_continuous(range = c(0.2, 2)) +
  scale_size_continuous(range = c(0.5, 4)) +
  coord_map("azequalarea", orientation = c(lat = 60.13, long = 18.64, 0), xlim = c(-12, 40), ylim = c(30, 72)) +
  labs(
    title = "Immigration to Sweden, 2021",
    subtitle = "From other European and selected countries. Highlighted are countries with more than 1 000 people",
    caption = "Source: Statistics Sweden · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size = 18, face = "bold", margin = margin(10, 0, 7, 0)),
    plot.subtitle = element_text(size = 12, margin = margin(0, 0, 10, 0))
  )
