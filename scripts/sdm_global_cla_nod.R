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

#Transformation of the coordinates into a spatial object
occ_sp <- SpatialPoints(occ_sf %>% st_coordinates())
#Transformation of the total map into a spatial object 
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
#slot function : transformation of spatial to regular data (for the output) - keeping only the weigth [,1]
weigth <- slot(count.points(occ_sp, global_map_sp), "data") [,1]

##The model needs centered and standardised env. var. (recomand use of dudi.pca)
env.pca <- dudi.pca(env, scannf = FALSE)

## --- Modeling
enfa <- enfa(env.pca, weigth, scannf = FALSE)
#Projections
pred <- predict(enfa, data)
#Binding with coordinates
pred.fin <- cbind(data@data[,1:2], pred@data)
#Adding the coordinates associated with NAs
#Isolation of those coordinates 
coord <- rbind(coord, data@data[,1:2])3
coord <- coord %>% count(x, y) %>% filter(n == 1) %>% dplyr::select(-n)
coord <- cbind(coord, rep(NA, length(nrow(coord))))
colnames(coord)[3] <- "Approx.MD"
pred.fin <- rbind(pred.fin, coord)

raster <- rasterFromXYZ(pred.fin)



# modèle
env_mod <- env_value %>% 
  dplyr::select(-all_of(usdm::vifstep(env_value)@excluded))
env_pca <- dudi.pca(env_mod, scannf = F)
enfa_model <- adehabitatHS::enfa(env_pca, 1, scannf = F)

# projection
enfa_predi <- adehabitatHS::predict.enfa(
  enfa_model,
  SpatialPixelsDataFrame(env_coord, subset(env_value, select = -vhm0))
)
