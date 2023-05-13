library(tidyverse)
library(terra)
library(cetcolor)
library(camcorder)

gg_record(here::here("mappromptmonday-temp"), width = 10, height = 8, dpi = 320)

f <- list.files(path = here::here("2023/2023-week_18/data/"), pattern = 'tif$', full.names = TRUE)

r <- lapply(f, rast)

m <- mosaic(sprc(r))

mdf <- terra::as.data.frame(m, xy = TRUE) %>% 
  rename(v = 3)

rgb2hex <- function(x) {
  sapply(strsplit(x, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))
}

codes <- tribble(
  ~"code", ~"rgb", ~"municipal level term", ~"technical term",
  11, "205 245 122", "MOSTLY UNINHABITED AREA", "VERY LOW DENSITY AREA",
  12, "171 205 102", "DISPERSED RURAL AREA", "LOW DENSITY AREA",
  13, "55 86 35", "VILLAGE", "SMALL SETTLEMENT",
  21, "255 255 0", "SUBURBS OR PERI-URBAN AREA", "SEMI-DENSE AREA",
  22, "168 112 0", "SEMI-DENSE TOWN", "SEMI-DENSE, MEDIUM SETTLEMENT",
  23, "115 38 0", "DENSE TOWN", "DENSE, MEDIUM SETTLEMENT",
  30, "255 0 0", "CITY", "LARGE SETTLEMENT"
) %>% 
  mutate(hex = rgb2hex(rgb)) # It isn't used

f1 <- "Outfit"

# Source: GHSL project (GHS-SMOD R2023A)
ggplot(mdf %>% filter(v > 10) %>% left_join(codes, by = c("v" = "code"))) +
  geom_raster(aes(x, y,  fill = factor(v))) +
  scale_fill_manual(values = rev(cet_pal(7, name = "cbl1")), labels = str_to_sentence(codes$`technical term`)) +
  labs(
    caption = "Source: Global Human Settlement Layer (GHS-SMOD R2023A) · Graphic: Georgios Karamanis",
    fill = "Degree of Urbanisation"
  ) +
  coord_sf() +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.12, 0.55),
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(margin = margin(0, 0, 10, 0))
  )
  
