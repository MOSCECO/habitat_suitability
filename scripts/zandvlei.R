# libraries ----
libs_to_call <- list(
  
  "basemaps",
  "ggmap",
  "ggnewscale",
  "ggplot2",
  "ggpubr", 
  "ggrepel",
  "ggspatial",
  "ggthemes",
  "here",
  "osmdata",
  "patchwork", 
  "purrr",
  "reshape2",
  "sf", 
  "stringr",
  "terra",
  "tidyverse"
  
)

# library calls ----
lapply(libs_to_call, function(i) {
  
  bool <- is.element(i, .packages(all.available = TRUE))
  
  if (!bool) {
    install.packages(i, dependencies = T)
  }
  
  library(i, character.only = TRUE)
  
}
)

# this sets your google map permanently
# register_google(
#   key = "XXX", write = TRUE
# )
has_google_key()
google_key()

# False Bay rough bounding box ----
wkt_fb <- "POLYGON((18.29049 -34.39946,18.90097 -34.39946,18.90097 -33.89137,18.29049 -33.89137,18.29049 -34.39946))"
pl_fb <- st_sf(st_as_sfc(wkt_fb, crs = 4326))
bb_fb  <- st_bbox(pl_fb)
bb_fb2 <- bb_fb
names(bb_fb2) <- c("left", "bottom", "right", "top")

# Zandlvei rough bounding box ----
wkt <- "POLYGON((18.46085 -34.10741,18.48143 -34.10741,18.48143 -34.08048,18.46085 -34.08048,18.46085 -34.10741))"
zandvlei_pl <- st_as_sf(st_as_sfc(wkt, crs = 4326))
zandvlei_pl$label <- "Zandvlei estuary"
zandvlei_bb <- st_bbox(zandvlei_pl)
zandvlei_bb2 <- zandvlei_bb
names(zandvlei_bb2) <- c("left", "bottom", "right", "top")


# False Bay satellite Image ----
map_fb <- get_map(
  location = bb_fb2,
  color = "color", 
  source = "google", 
  maptype = "satellite", 
  zoom = 10
)
false_bay_centroid <- st_point(c(18.639530809389292, -34.219757691573825)) %>% 
  st_sfc(., crs = "EPSG:4326") %>% 
  st_as_sf()
false_bay_centroid$label <- "False Bay"

# Cape Town city hall ----
ct_city_hall <- st_point(c(18.423824523978283, -33.92508191963811)) %>% 
  st_sfc(., crs = "EPSG:4326") %>% 
  st_as_sf()
ct_city_hall$label <- "Cape Town"

# Visualization ----
p_false_bay <- ggmap(map_fb) +
  geom_sf(data = ct_city_hall, col = "red", inherit.aes = FALSE) + 
  geom_sf_label(
    data = ct_city_hall, 
    aes(label = label), 
    nudge_x = 0.09,
    inherit.aes = FALSE
  ) +
  geom_sf(data = zandvlei_pl, col ="red", fill = NA, inherit.aes = F) +
  geom_sf_label(
    data = zandvlei_pl, aes(label = label), nudge_x = 0.13, inherit.aes = F
  ) +
  geom_sf_label(
    data = false_bay_centroid, aes(label = label), inherit.aes = F
  ) +
  xlab("Longitude") + 
  ylab("Latitude")

# Zandvlei satellite image ----
zandvlei_map <- get_map(
  location = zandvlei_bb2,
  color = "color", 
  source = "google", 
  maptype = "satellite", 
  zoom = 14
)

# Visualization ----
p_zandvlei <- ggmap(zandvlei_map) + 
  scale_x_continuous(limits = zandvlei_bb[c(1, 3)]) + 
  scale_y_continuous(limits = zandvlei_bb[c(2, 4)]) +
  xlab("Longitude") + 
  ylab("Latitude")

# natural areas
zandlvei_natural <- opq(zandvlei_bb) %>% 
  add_osm_feature(key = "natural") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_nat <- zandlvei_natural$osm_points %>% 
  st_crop(zandvlei_bb)
li_nat <- zandlvei_natural$osm_lines %>% 
  st_crop(zandvlei_bb)
pl_nat <- zandlvei_natural$osm_polygons %>% 
  st_crop(zandvlei_bb)
nat_bb <- zandvlei_bb
nat_bb[c(2, 3)] <- c(-34.1, 18.475)
pl_nat1 <- pl_nat %>% 
  filter(!is.na(natural)) %>% 
  st_crop(nat_bb)
ggplot() + geom_sf(data = pl_nat1)
mp_nat <- zandlvei_natural$osm_multipolygons %>% 
  st_crop(zandvlei_bb) %>% 
  filter(natural != "water") %>% 
  filter(natural != "sand")
zandvlei <- zandlvei_natural$osm_multipolygons %>% 
  filter(natural == "water")
features_natural <- pl_nat1 %>% select(natural) %>% 
  rbind(mp_nat %>% select(natural)) %>% 
  rbind(zandvlei %>% select(natural)) %>% 
  group_by(natural) %>% 
  summarise(geometry = st_union(geometry))
ggplot() + geom_sf(data = zandvlei, fill = "lightblue")
ggplot() + geom_sf(data = mp_nat %>% filter(natural == "sand")) + 
  scale_x_continuous(limits = zandvlei_bb[c(1, 3)]) + 
  scale_y_continuous(limits = zandvlei_bb[c(2, 4)]) 

# buildings
zandlvei_building <- opq(zandvlei_bb) %>% 
  add_osm_feature(key = "building") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_bui <- zandlvei_building$osm_points %>% 
  st_crop(zandvlei_bb)
li_bui <- zandlvei_building$osm_lines %>% 
  st_crop(zandvlei_bb)
pl_bui  <- zandlvei_building$osm_polygons %>% 
  st_crop(zandvlei_bb)
mp_bui  <- zandlvei_building$osm_multipolygons %>% 
  st_crop(zandvlei_bb)

ggplot() +
  geom_sf(data = zandvlei) + 
  geom_sf(
    data = mp_bui
  )

# highway
zandlvei_highway <- opq(zandvlei_bb) %>% 
  add_osm_feature(key = "highway") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_hig <- zandlvei_highway$osm_points %>% 
  st_crop(zandvlei_bb)
li_hig <- zandlvei_highway$osm_lines %>% 
  st_crop(zandvlei_bb)
pl_hig  <- zandlvei_highway$osm_polygons %>% 
  st_crop(zandvlei_bb)

# sampling site
site_1 <- st_point(c(18.465315870202115, -34.08803141865802)) %>% 
  st_sfc(., crs = "EPSG:4326") %>% 
  st_as_sf()
site_1$nb <- 1
site_2 <- st_point(c(18.473, -34.101)) %>% 
  st_sfc(., crs = "EPSG:4326") %>% 
  st_as_sf()
site_2$nb <- 2

# sampling site
marina_da_gama <- st_point(c(18.47689961031797, -34.090807699357285)) %>% 
  st_sfc(., crs = "EPSG:4326") %>% 
  st_as_sf()
marina_da_gama$label <- "Marina Da Gama"

# sandprawns distribution
sp_area <- " POLYGON((18.4749 -34.1042,18.4753 -34.10413,18.4757 -34.10356,18.47468 -34.10184,18.47329 -34.10159,18.47343 -34.10061,18.47181 -34.10017,18.47074 -34.09984,18.4706 -34.10009,18.47062 -34.10037,18.47072 -34.10056,18.4702 -34.10087,18.47044 -34.10137,18.4749 -34.1042))"
sp_area <- st_sf(sf::st_as_sfc(sp_area, crs = 4326))
sp_area <- st_intersection(zandvlei, sp_area) 
sp_area <- sp_area %>% 
  select(natural)
sp_area$natural <- "Sandprawns"

# Natural features in zandvlei
features_natural <- rbind(
  features_natural, 
  sp_area
)
features_natural$natural <- factor(features_natural$natural)
levels(features_natural$natural) <-   c(
  "False Bay",
  "Beach", 
  "Grassland", 
  "Sandprawns",
  "Scrub", 
  "Water", 
  "Wetland", 
  "Wood"
)
p_scheme <- ggplot() +
  # Natural features in zandvlei
  geom_sf(data = features_natural, aes(fill = natural), alpha = 0.7, inherit.aes = F) +
  scale_fill_manual(
    values = c(
      "cyan", "lightyellow", "green4", "lightblue", "green3", "blue", 
      "lightgreen", "brown"
    )
  ) +
  # geom_sf(data = zandvlei, fill = "blue", alpha = 0.6, inherit.aes = F) +
  # geom_sf(data = sp_area, fill = "lightblue", col = NA, inherit.aes = F) +
  # geom_sf(data = zandvlei_beach, fill = "lightyellow", col = NA, inherit.aes = F) +
  # geom_sf(data = zandvlei_bay, fill = "cyan", col = NA, inherit.aes = F) +
  # geom_sf(data = zandvlei_scrub, fill = "green3", col = NA, inherit.aes = F) +
  # geom_sf(data = st_union(zandvlei_wetland), fill = "lightgreen", col = NA, inherit.aes = F) +
  # geom_sf(data = pl_nat, aes(fill = natural), col = NA, inherit.aes = F) +
  # Anthropogenic + experiment features
  geom_sf(data = li_hig, col = "grey", alpha = 0.7, inherit.aes = F) +
  geom_sf(data = site_1, col = "red", shape = "+", size = 7, inherit.aes = F) + 
  geom_label_repel(
    data = site_1,
    aes(label = nb, geometry = x), 
    stat = "sf_coordinates", 
    size = 5, 
    direction = "both",
    nudge_x = 0.0005, 
    nudge_y = 0.0005,
    inherit.aes = F
  ) + 
  geom_sf(data = site_2, col = "red", shape = "+", size = 7, inherit.aes = F) + 
  geom_label_repel(
    data = site_2, 
    aes(label = nb, geometry = x), 
    stat = "sf_coordinates", 
    size = 5, 
    direction = "both",
    nudge_x = 0.0005, 
    nudge_y = 0.0005,
    inherit.aes = F) +
  geom_sf_label(
    data = marina_da_gama, aes(label = label), inherit.aes = F
  ) + 
  annotation_scale(location = "bl", width_hint = 0.125) +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_x = unit(0, "in"), 
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering
  ) + 
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing = unit(0, "lines"),
        plot.background = element_blank(), 
        legend.justification = c(0, 0)
  ) + 
  guides(fill = guide_legend(title = NULL))
x11(); p_scheme

# inset ZA ----
# path <- "/home/borea/Documents/mosceco/r_projects/mosceco_miscellanous/ensta_planification_spatiale/data/mappemonde.rds"
# za <- readRDS(path)
# za <- za %>% 
#   filter(region == "South Africa")
# za2 <- st_difference(za %>% filter(is.na(subregion)), 
#                      za %>% filter(subregion == "enclave"))
# ggplot() + geom_sf(data = za2)
# za_map <- st_union(za2)
# saveRDS(za_map, here("data", "za.rds"))
za_map <- readRDS(here("data", "za.rds"))

# Cape Town rough bounding box ---
ct_frame <- "POLYGON((17.56633 -34.91719,19.78378 -34.91719,19.78378 -33.18429,17.56633 -33.18429,17.56633 -34.91719))"
ct <- st_as_sf(sf::st_as_sfc(ct_frame, crs = 4326))
p_za <- ggplot() + 
  geom_sf(data = za_map) + 
  geom_sf(data = ct, fill = NA, col = "red", lwd = 0.5) + 
  theme_map()

# Final visualization ----
P <- ((p_za / p_false_bay) | p_zandvlei | p_scheme) + 
  plot_annotation(
    caption = "Projection: World Geodetic System 1984 ensemble (EPSG:4326)",
    tag_levels = "A"
  )
x11(); P

ggexport(
  P, 
  filename = here("figures", "zandvlei.png"), 
  width = 5000, 
  height = 2600, 
  res = 230
)

