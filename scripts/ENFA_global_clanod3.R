# Méthode niche globale et modèles des distributions filtrés
# ENFA

# Données biologiques du Muséum ----
bn <- "Claremontiella nodulosa"
sp  <- pa[[bn]] %>% as.data.frame(xy = T)

# Présences
spp <- sp %>% 
  filter(individualCount > 0)
spp_sf <- st_as_sf(
  spp, 
  coords = c("x", "y"),
  remove = F,
  crs = "EPSG:4326"
)
# Données environnementales ----
clm <- terra::extract(climosaic, spp_sf)
clm <- clm[, grepl("sbt|so|sw1|hm0|ww", names(clm))]
names(clm) <- gsub("hm0", "vhm0", names(clm))
clm_sf <- st_as_sf(clm, geometry = st_geometry(spp_sf))

# données biologiques mondiales ----
cgc_clanod_f <- cgc_clanod %>% 
  na.omit() %>% 
  filter(!duplicated(.$geometry)) %>% 
  select(names(.)[grepl("stdv|mean", names(.))])
names(cgc_clanod_f) <- names(cgc_clanod_f) %>% 
  gsub("bottomt", "sbt", .) %>% 
  gsub("VHM0_WW", "ww", .) %>% 
  gsub("VHM0_SW1", "sw1", .) %>% 
  gsub("VHM0", "vhm0", .)
dim(cgc_clanod_f) # 231 occurrences

# regroupement données biologiques ----
occ_sf <- cgc_clanod_f[
  , c(order(names(cgc_clanod_f)[-length(cgc_clanod_f)]), length(cgc_clanod_f))
] %>% 
  rbind(clm_sf[, c(order(names(clm_sf)[-length(clm_sf)]), length(clm_sf))])

# réduction de la colinéarité
occ_sf <- occ_sf %>% select(
  -all_of(usdm::vifstep(occ_sf %>% st_drop_geometry())@excluded)
)

# conversion en raster
# mapply(\(x, y) {  writeRaster(
#   x, here("data", "analysis", paste0("CONVERSION.", y, ".tif"))
# )}, as.list(cgc), names(cgc), SIMPLIFY = F)
# env <- lapply(
#   list.files(here("data", "analysis"), pattern = "CONVERSION", full.names = T),
#   raster
# )
# env <- do.call(stack, env)

# Routine d'analyse sous le package/format sp

# Transformation des données d'occurrences au format sp
occ_sp <- SpatialPoints(occ_sf %>% st_coordinates())

# Transformation des données de carte au format sp
cgc_sub <- cgc %>% 
  terra::subset(names(.)[grepl("stdv|mean", names(.))])
var_col <- usdm::vifstep(as.data.frame(cgc_sub %>% na.omit()))@excluded
cgc_sub <- subset(cgc_sub, names(cgc_sub)[!names(cgc_sub) %in% var_col])
names(cgc_sub) <- names(cgc_sub) %>% 
  gsub("_lyr.1", "", .) %>% 
  gsub("bottomt", "sbt", .) %>% 
  gsub("VHM0_WW", "ww", .) %>% 
  gsub("VHM0_SW1", "sw1", .) %>% 
  gsub("VHM0", "vhm0", .)
global_map    <- as.data.frame(cgc_sub, xy = T, na.rm = FALSE)
global_map_sp <- SpatialPixelsDataFrame(
  global_map[, 1:2], as.data.frame(global_map[, -c(1:2)])
)

# Transformation des données de carte de projection au format sp
climosaic_enfa   <- climosaic %>% terra::subset(names(cgc_sub))
climosaic_map    <- as.data.frame(climosaic_enfa, xy = T, na.rm = FALSE)
climosaic_map_sp <- SpatialPixelsDataFrame(
  climosaic_map[, 1:2], as.data.frame(climosaic_map[, -c(1:2)])
)