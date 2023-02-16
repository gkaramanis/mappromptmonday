library(tidyverse)
library(getCRUCLdata)
library(biscale)
library(patchwork)
library(camcorder)

# Idea and colors by Colin Angus @VictimOfMaths
# https://twitter.com/VictimOfMaths/status/1589552401324732416

gg_record(dir = "mappromptmonday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

sun_pre <- get_CRU_df(sunp = TRUE, pre = TRUE)

eu_sunpre <- sun_pre %>% 
  filter(lon < 45 & lon > -26) %>% 
  filter(lat < 71 & lat > 29) %>% 
  group_by(lon, lat) %>% 
  summarise(
    sun = mean(sun),
    pre = mean(pre)
  ) %>% 
  bi_class(., x = pre, y = sun, style = "quantile", dim = 3)

f1 <- "Outfit"
pal <- c("1-1" = "#f3f3f3", "2-1" = "#b4d3e1", "3-1" = "#509dc2", 
         "1-2" = "#f3e6b3", "2-2" = "#b3b3b3", "3-2" = "#376387",
         "1-3" = "#f3b300", "2-3" = "#b36600", "3-3" = "#1B1B1B")

m <- ggplot(eu_sunpre) +
  geom_tile(aes(lon, lat, fill = bi_class, color = bi_class)) +
  bi_scale_fill(pal = pal, dim = 3, na.value = "white", flip_axes = TRUE) +
  bi_scale_color(pal = pal, dim = 3, na.value = "white") +
  coord_fixed(expand = FALSE) +
  labs(
    title = "Sunshine and precipitation",
    subtitle = "Mean % of daylength and mm/month, 1961–1990",
    caption = "Source: CRU CL v.2.0 (1961-1990) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(5, 0, 10, 0), size = 12)
  )

l <- bi_legend(pal = pal, dim = 3, size = 11,
               xlab = "More rain ",
               ylab = "More sun ",
               base_family = f1
               ) +
  coord_fixed(expand = FALSE) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )  

m  + 
  inset_element(l, 0, 0.3, 0.2, 0.55)

