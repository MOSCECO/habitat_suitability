# Méthode niche globale et modèles des distributions filtrés
# ENFA

# import des données environnementales pour la période étudiée
env       <- envbind %>% dplyr::select(x, y, station = stn, everything())
env_dim   <- env     %>% na.omit() %>% filter(!duplicated(.$station))
dim(env_dim)
env_coord <- env_dim %>% dplyr::select(x, y)
env_value <- env_dim %>% dplyr::select(-c(x, y, station))

# import des occurrences de l'espèce
occ <- global_occf$Muricoidea$Claremontiella_nodulosa %>% 
  add_column(station = envbind$stn) %>% 
  dplyr::select(x = decimalLongitude, y  = decimalLatitude, station)  
occ_dim   <- occ %>% 
  filter(station %in% env_dim$station) %>% 
  filter(!duplicated(.$station))
dim(occ_dim)

# visualisation des occurrences
# ggplot() +
#   geom_sf(data = m %>% st_crop(st_bbox(occ_dim)), fill = NA) + 
#   geom_sf(
#     data = occ_dim %>% cbind(env_dim %>% dplyr::select(-c(x, y, station))),
#     aes(col = bottomt)
#   ) + 
#   scale_color_gradient(low = "blue", high = "red")

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
