# install.packages("OpenStreetMap")
# library(OpenStreetMap)
vignette(package = "osmdata")
# install.packages("osmdata")
library(osmdata)

# osmdata       1. osmdata (source, html)
# osmdata-sc    2. osmdata_sc (source,
#                              html)
# osm-sf-translation
# 3. OSM to Simple Features
# (source, html)
# query-split   4. query-split (source,
#                               html)
vignette("osmdata")
wkt <- "POLYGON((18.45263 -34.11049,18.48405 -34.11049,18.48405 -34.07782,18.45263 -34.07782,18.45263 -34.11049))"
bb <- st_bbox(sf::st_as_sfc(wkt, crs = 4326))

# raster
# install.packages("basemaps")
library(basemaps)
public_token <- "pk.eyJ1IjoiaWdyZWdtYW4iLCJhIjoiY2xqOWFsZzBuMWNscDNlcW5pNGJ1ZDQ5MSJ9.UTMNwP7SNAMQAKflMSCHvA"
zandvlei_token <- "pk.eyJ1IjoiaWdyZWdtYW4iLCJhIjoiY2xqOWRwbDZkMGVnMTNlcGd2aWdlMmhqbCJ9.mZYlBvVJypqTZ7UHtIzqMw"
r <- basemap(
  ext = bb, 
  map_service = "mapbox",
  class = "magick", 
  map_type = "satellite", 
  map_token = zandvlei_token
)
devtools::install_github("16EAGLE/basemaps")
basemap_magick(ext = bb, map_service = "mapbox", map_type = "streets", map_token = zandvlei_token)
tmp <- basemap_geotif(
  ext = bb, 
  map_service = "mapbox", 
  map_type = "satellite", 
  map_token = zandvlei_token
)
sr <- rast(tmp)

# natural areas
zandlvei_natural <- opq(bb) %>% 
  add_osm_feature(key = "natural") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_nat <- zandlvei_natural$osm_points
li_nat <- zandlvei_natural$osm_lines
pl_nat <- zandlvei_natural$osm_polygons
zandvlei <- zandlvei_natural$osm_multipolygons %>% 
  filter(natural == "water")
ggplot() + geom_sf(data = zandvlei, fill = "lightblue")

# anthropogenic areas
# route
zandlvei_route <- opq(bb) %>% 
  rou_osm_feature(key = "route") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_rou <- zandlvei_route$osm_points %>% 
  st_crop(zandvlei)
li_rou <- zandlvei_route$osm_lines %>% 
  st_crop(zandvlei)
pl_rou <- zandlvei_route$osm_polygons %>% 
  st_crop(zandvlei)
ggplot() +
  geom_sf(data = zandvlei) + 
  geom_sf(data = li_rou)

# buildings
zandlvei_building <- opq(bb) %>% 
  add_osm_feature(key = "building") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_bui <- zandlvei_building$osm_points %>% 
  st_crop(zandvlei)
li_bui <- zandlvei_building$osm_lines %>% 
  st_crop(zandvlei)
pl_bui  <- zandlvei_building$osm_polygons %>% 
  st_crop(zandvlei)

ggplot() +
  geom_sf(data = zandvlei) + 
  geom_sf(
    data = pl_bui, 
    aes(
      fill = `addr:city` == "Marina Da Gama"
    )
  )

# highway
zandlvei_highway <- opq(bb) %>% 
  add_osm_feature(key = "highway") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_hig <- zandlvei_highway$osm_points %>% 
  st_crop(zandvlei)
li_hig <- zandlvei_highway$osm_lines %>% 
  st_crop(zandvlei)
pl_hig  <- zandlvei_highway$osm_polygons %>% 
  st_crop(zandvlei)

e <- st_bbox(zandvlei)[c(1, 3, 2, 4)]
sr_4326 <- project(sr, "epsg:4326")
sr_4326 <- sr_4326 %>% crop(e)
srdf <- as.data.frame(sr_4326, xy = T)
require(ggnewscale)
require(ggrepel)
require(ggspatial)
p <- ggplot() +
  geom_sf(data = zandvlei, fill = "blue", alpha = 0.6) +
  geom_sf(data = sp_area, fill = "lightblue", col = NA) +
  # geom_tile(
  #   data = srdf,
  #   aes(x, y, fill = red)
  # ) +
  # new_scale_fill() + 
  # geom_sf(data = zandvlei, fill = "lightblue", alpha = 0.3) +
  geom_sf(data = li_hig, col = "grey", alpha = 0.7) +
  # geom_sf(data = pl_hig)
  # geom_sf(
  #   data = pl_bui, 
  #   aes(
  #     fill = `addr:city` == "Marina Da Gama"
  #   )
  # )
  geom_sf(data = site_1, col = "red", shape = "+", size = 7) + 
  geom_text_repel(data = site_1, aes(label = nb, geometry = x), 
                  stat = "sf_coordinates", size = 5) + 
  geom_sf(data = site_2, col = "red", shape = "+", size = 7) + 
  geom_text_repel(data = site_2, aes(label = nb, geometry = x), 
                  stat = "sf_coordinates", size = 5) +
  theme_map() + 
  annotation_scale(location = "bl", width_hint = 0.125, ) +
  annotation_north_arrow(
    location = "bl", 
    which_north = "true", 
    pad_x = unit(0, "in"), 
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering
  )
x11(); p

# sampling site
site_1 <- st_point(c(18.465315870202115, -34.08803141865802)) %>% 
  st_sfc(., crs = "EPSG:4326") %>% 
  st_as_sf()
site_1$nb <- 1
site_2 <- st_point(c(18.473, -34.101)) %>% 
  st_sfc(., crs = "EPSG:4326") %>% 
  st_as_sf()
site_2$nb <- 2

# sandprawns distribution
sp_area <- " POLYGON((18.4749 -34.1042,18.4753 -34.10413,18.4757 -34.10356,18.47468 -34.10184,18.47329 -34.10159,18.47343 -34.10061,18.47181 -34.10017,18.47074 -34.09984,18.4706 -34.10009,18.47062 -34.10037,18.47072 -34.10056,18.4702 -34.10087,18.47044 -34.10137,18.4749 -34.1042))"
sp_area <- st_as_sf(sf::st_as_sfc(sp_area, crs = 4326))
sp_area <- st_intersection(zandvlei, sp_area)

# inset ZA
# path <- "/home/borea/Documents/mosceco/r_projects/mosceco_miscellanous/ensta_planification_spatiale/data/mappemonde.rds"
# za <- readRDS(path)
# za <- za %>% 
#   filter(region == "South Africa")
# za2 <- st_difference(za %>% filter(is.na(subregion)), 
#                      za %>% filter(subregion == "enclave"))
# ggplot() + geom_sf(data = za2)
# za3 <- st_union(za2)
# saveRDS(za3, here("data", "za.rds"))
za3 <- readRDS(here("data", "za.rds"))
ggplot() + geom_sf(data = za3)

ct_frame <- "POLYGON((17.56633 -34.91719,19.78378 -34.91719,19.78378 -33.18429,17.56633 -33.18429,17.56633 -34.91719))"
ct <- st_as_sf(sf::st_as_sfc(ct_frame, crs = 4326))
ggplot() + geom_sf(data = za3) + geom_sf(data = ct, fill = NA, col = "red")

za_4222 <- st_transform(za3, crs = "ESRI:54009")
ct_4222 <- st_transform(ct, crs = "ESRI:54009")
ggplot() + 
  geom_sf(data = za3) + 
  geom_sf(data = ct, fill = NA, col = "red") + 
  theme_map()

# natural areas
ct_3587 <- st_as_sf(sf::st_as_sfc(ct_frame, crs = 3587)) %>% 
  st_bbox()
false_bay <- opq(ct_3587) %>% 
  add_osm_feature(key = "natural") %>% 
  opq_string() %>% 
  osmdata_sf()
pt_nat <- false_bay$osm_points
li_nat <- false_bay$osm_lines
pl_nat <- false_bay$osm_polygons
ggplot() + 
  geom_sf(data = li_nat) +
  geom_sf(data = pl_nat)
