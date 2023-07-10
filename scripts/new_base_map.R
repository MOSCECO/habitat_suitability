install.packages("basemaps")
devtools::install_github("16EAGLE/basemaps")

library(basemaps)
data(ext)
# or use draw_ext() to interactively draw an extent yourself

# view all available maps
get_maptypes()

# set defaults for the basemap
set_defaults(map_service = "osm", map_type = "topographic")

# load and return basemap map as class of choice, e.g. as image using magick:
zandvlei <- basemap_magick(
  ext = bb, 
  map_service = "mapbox", 
  map_type    = "satellite", 
  map_token   = public_token
)
false_bay <- basemap_terra(
  ext = st_bbox(ct), 
  map_service = "mapbox", 
  map_type    = "satellite", 
  map_token   = public_token
)