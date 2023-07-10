################################################################################
#### MODÈLE LOCAL - DONNÉES ENVIRONNEMENTALES SEXTANT                       ####
################################################################################

# PARAMÉTRAGE ####
# "GLM", "GBM", "GAM", "CTA", "ANN", "SRE",
# "FDA", "MARS", "RF", "MAXENT", "MAXNET"
alg <- "MAXENT"
# Nombre de répétitions (nombre de jeux de validation croisées)
CV_nb_rep <- 5

# nom du modèle
vec_name_model <- c(paste0(tolower(alg), CV_nb_rep), "02", "local", "sxt")
pts_name_model <- paste(vec_name_model, collapse = ".")

# Claremontiella nodulosa

# jeux de données environnementales pour calibration du SDM ----
# carte globale des variables environnementales
clim_sub      <- sxt_sub
clim_proj_sub <- clim_sub

# Données biologiques ----
bn <- "Claremontiella nodulosa"
sp  <- pa[[bn]] %>% as.data.frame(xy = T)
binnam <- str_split(bn, " ")[[1]] %>%
  lapply(substr, 1, 3) %>%
  paste0(collapse = ".")

# Données locales ----
# Présences ----
spp_local <- sp %>%
  filter(individualCount > 0) %>%
  cbind(type = "pr", id = paste0("pr", 1:nrow(.)), scale = "local") %>%
  select(type, id, scale, x, y, individualCount)
spp_local_sf <- st_as_sf(
  spp_local,
  coords = c("x", "y"),
  remove = F,
  crs = "EPSG:4326"
)
spp_local_sf <- spp_local_sf %>%
  cbind(terra::extract(clim_proj_sub, spp_local_sf, ID = F))

# Absences ----
spa_local <- sp %>%
  filter(individualCount == 0) %>%
  cbind(type = "ab", id = paste0("ab", 1:nrow(.)), scale = "local") %>%
  select(type, id, scale, x, y, individualCount)
spa_local_sf <- st_as_sf(
  spa_local,
  coords = c("x", "y"),
  remove = F,
  crs = "EPSG:4326"
)
spa_local_sf <- spa_local_sf %>%
  cbind(terra::extract(clim_proj_sub, spa_local_sf, ID = F))

# Aggrégation données biologiques ----
bio_list <- list(spp_local_sf, spa_local_sf)
bio <- do.call(rbind, bio_list)
bio <- bio %>%
  arrange(desc(individualCount), type)

# Application de la fonction de production d'un SDM pour un seul algorithme
sdmOneAlgo(
  alg            = alg,
  CV_nb_rep      = CV_nb_rep,
  binam          = binam,
  vec_name_model = vec_name_model,
  bio            = bio,
  clim_sub       = clim_proj_sub,
  clim_proj_sub  = clim_proj_sub
)
