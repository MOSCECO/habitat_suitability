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

# Routine ENFA ----
map <- global_map_sp
locs <- occ_sp

# Vérification de la symétrie des distributions des variables
x11() ; hist(map, type = "l")
# Variables non symétriques (approximativement) : 
#   mean.sbt, mean.so, stdv.ww

# Transformation des variables
# slot(map, "data")[, "mean.sbt"] <- log2(slot(map, "data")[, "mean.sbt"])
# slot(map, "data")[, "mean.so"]  <- log2(slot(map, "data")[, "mean.so"])
# slot(map, "data")[, "stdv.ww"]  <- log2(slot(map, "data")[, "stdv.ww"])
# x11() ; hist(map, type = "l")

## We prepare the data for the ENFA
tab  <- slot(map, "data")
pr   <- slot(count.points(locs, map), "data")[,1]
nona <- which(rowSums(apply(tab, 2, is.na)) == 0)
pr   <- pr[nona]
table(pr, useNA = "always")

## We then perform the PCA before the ENFA
pc <- dudi.pca(tab %>% na.omit(), scannf = FALSE)

## The object 'pc' contains the transformed table (i.e.
## centered so that all columns have a mean of 0
## and scaled so that all columns have a variance of 1
## 'pc' also contains the weights of the habitat variables,
## and the weights of the pixels in the analysis

enfa1 <- adehabitatHS::enfa(dudi = pc, pr = pr, scannf = FALSE)
hist(enfa1)
hist(enfa1, scores = FALSE, type = "l")


## scatterplot
x11(); ade4::scatter(enfa1)


## randomization test
## Not run: 
(renfa <- randtest(enfa1))
plot(renfa)

require(biomod2)
tb <- map@coords[nona, ] %>% 
  cbind(value = enfa1$li[, 1]) %>% 
  as_tibble()
levelplot(
  x = value ~ x * y,
  data = tb, 
  aspect = "iso",
  main = "Claremontiella nodulosa ENFA global projection", 
  col.regions = colorRampPalette(c("grey90", "yellow4", "green4"))(100)
) + latticeExtra::layer(sp.points(locs, cex = 1))

# lac maracaibo rempli...

# projection sur la zone locale
global_map2    <- as.data.frame(cgc_sub, xy = T, na.rm = FALSE) %>% na.omit()
global_map_sp2 <- SpatialPixelsDataFrame(
  global_map2[, 1:2], as.data.frame(global_map2[, -c(1:2)])
)
# a <- row.names(as.data.frame(enfa1$tab %>% na.omit()))
# b <- row.names(global_map_sp2@data)
# e <- which(!(b %in% a))
# global_map3    <- as.data.frame(cgc_sub, xy = T, na.rm = FALSE) %>% 
#   na.omit()
# global_map3 <- global_map3[-e, ]
# global_map_sp3 <- SpatialPixelsDataFrame(
#   global_map3[, 1:2], as.data.frame(global_map3[, -c(1:2)])
# )
pred <- adehabitatHS::predict.enfa(
  object = enfa1,
  map    = global_map_sp2
)
x11()
image(pred)
contour(pred, col="green", add=TRUE)
points(locs, pch = 3)
test <- predict.enfa(
  object = enfa1,
  map    = climosaic_map_sp
)

# solution : 
# (1)
# obtenir les coordonnées de chaque pixel de la zone locale dans 
# l'espace factoriel de l'ACP réalisée dans l'ENFA (en les mettant en 
# individus supplémentaires)
# (2)
# définir la position de la niche comme dans l'ENFA
# (3)
# calculer, pour chaque pixel, les coordonées de ligne avec la fonction
# mahalanobis (distance de M entre un pixel et le barycentre de la niche)
