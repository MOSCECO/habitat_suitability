################################################################################
#### MODÈLE GLOBAL - DONNÉES ENVIRONNEMENTALES COPERNICUS                   ####
################################################################################

# PARAMÉTRAGE ####
# "GLM", "GBM", "GAM", "CTA", "ANN", "SRE",
# "FDA", "MARS", "RF", "MAXENT", "MAXNET"
alg <- "MAXENT"
# Nombre de répétitions (nombre de jeux de validation croisées)
CV_nb_rep <- 5

# nom du modèle
vec_name_model <- c(paste0(tolower(alg), CV_nb_rep), "01", "global", "cpc")
pts_name_model <- paste(vec_name_model, collapse = ".")

# Claremontiella nodulosa

# jeux de données environnementales pour calibration du SDM ----
# carte globale des variables environnementales
clim_sub      <- cgc_sub
clim_proj_sub <- subset(climosaic, names(clim_sub))

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

# Données mondiales ----
# Présences ----
pas <- nrow(spp_local_sf) + 1
spp_global_sf <- cgc_clanod_f %>%
  cbind(st_coordinates(cgc_clanod_f)) %>%
  cbind(
    type = "pr",
    id = paste0("pr", pas:(pas + nrow(cgc_clanod_f) - 1)),
    scale = "global",
    individualCount = 1
  ) %>%
  select(
    type, id, scale, x = X, y = Y, individualCount, all_of(names(clim_sub))
  )

# Pseudo-absences ----
pa_set <- bm_PseudoAbsences(
  resp.var    = rep(1, nrow(spp_global_sf)),
  expl.var    = clim_sub,
  nb.absences = nrow(spp_global_sf)
)
spa_global_sf <- cbind(
  type = "pa", id = row.names(pa_set$xy), scale = "global",
  pa_set$xy, individualCount = 0, pa_set$env
) %>%
  filter(grepl("pa", .$id)) %>%
  as_tibble() %>%
  st_as_sf(
    .,
    coords = c("x", "y"),
    remove = F,
    crs = "EPSG:4326"
  )

# Aggrégation données biologiques ----
bio_list <- list(spp_local_sf, spa_local_sf, spp_global_sf, spa_global_sf)
bio <- do.call(rbind, bio_list)
bio <- bio %>%
  arrange(desc(individualCount), type)

# sauvegarde des données biologiques
# saveRDS(
#   bio,
#   here("data", "analysis", "bio_data_cpc.rds")
# )

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
