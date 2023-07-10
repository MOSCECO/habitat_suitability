# regroupement de modèles différents selon les une hiérarchie : 
# habitat > moodèle sextant local > modèle copernicus global
# Pour cela : 
# (1) Faire plusieurs runs (5) avec un algorithme pour chaque type de modèle
#   (a) faire tous les modèles sur le même script jusqu'au wmean/ca
#   (b) concaténer les résultats
#   (c) réutiliser la même fonction pour appliquer wmean/ca à la concaténation
# (2) Les pondérer par weighted means puis committee averaging
# (3) Pondérer les raster de sorties par weighted means et committee averaging
# (4) Transformer le raster obtenu en probabilité de présence par un seuil
# (5) réaliser cela pour RF; MAXENT puis un modèle d'ensemble. 

################################################################################
#### MODÈLE GLOBAL - DONNÉES ENVIRONNEMENTALES COPERNICUS                   ####
################################################################################

# nom du modèle
vec_name_model <- c("rf", "global", "cpc")
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

# Identifiant du modèle ----
modeling_id <- gsub(
  " ", 
  ".", 
  paste(
    binnam, 
    paste(vec_name_model, collapse = " ")
  )
)

# Formatage des données pour le modèle ----
path_models <- here("data", "analysis", "models")
makeMyDir(path_models)
spec_data_cpc <- BIOMOD_FormatingData( 
  # Données initiales
  resp.var       = bio$individualCount,
  expl.var       = bio %>% st_drop_geometry() %>% 
    select(-c(type, id, scale, x, y, individualCount)), 
  resp.xy        = st_coordinates(bio) %>%
    as_tibble() %>%  select(x = X, y = Y),
  # Modalités de sauvegarde
  dir.name       = path_models, 
  resp.name      = modeling_id, 
  # Génération des pseudo-absences
  PA.strategy    = "user.defined",
  PA.user.table  = rep(TRUE, nrow(bio)) %>% as.matrix(), 
  # Gestion des occurrences multiples dans une cellule
  filter.raster  = TRUE
)

# Paramétrage du modèle ----
biom_options <- BIOMOD_ModelingOptions(
  MAXENT = list(
    path_to_maxent.jar = here("scripts", "maxent", "maxent.jar")
  )
)
# all_biomod2_algos <- c(
#   "GLM", "GBM", "GAM", "CTA", "ANN", "SRE", 
#   "FDA", "MARS", "RF", "MAXENT", "MAXNET"
# )
all_biomod2_algos <- "RF"
# Modélisation des habitats favorables selon une méthode ensembliste ----
cl <- startMPIcluster()
registerDoMPI(cl)
spec_models_cpc <- BIOMOD_Modeling(
  bm.options      = biom_options,
  bm.format       = spec_data,
  modeling.id     = modeling_id,
  models          = all_biomod2_algos,
  CV.nb.rep       = 5,
  data.split.perc = 80,
  var.import      = 3, 
  do.full.models  = F, 
  nb.cpu = 2
)

# calculate the mean of variable importance by algorithm
(spec_models_var_import <- get_variables_importance(spec_models))

var_importance <- dcast(
  spec_models_var_import, 
  expl.var ~ algo, 
  fun.aggregate = mean, 
  value.var = "var.imp"
)
p5 <- ggplot() +
  geom_col(
    data = var_importance, 
    aes(
      x = expl.var %>%
        factor(
          levels = expl.var[order(RF, decreasing = T)]
        ),
      y = RF
    )
  ) + 
  xlab("Variable environnementale") +
  ylab("Contribution (%)")

path_eval <- here(
  "data", "analysis", "models", modeling_id, "eval"
)
makeMyDir(path_eval)

ggexport(
  plot = p5,
  filename = here(path_eval, "contributions_variables.png"), 
  width = 1000, 
  height = 800, 
  res = 200,
  units = "px",
  device = "png", 
  limitsize = F
)


# Models response curves
# To do this we first have to load the produced models.
lapply(
  all_biomod2_algos, 
  \(my_algo) {
    glm_eval_strip <- biomod2::bm_PlotResponseCurves(
      bm.out           = spec_models,
      models.chosen    = BIOMOD_LoadModels(spec_models, algo = my_algo), 
      fixed.var        = "median",
      main             = my_algo, 
      do.plot          = F
    )
    pout <- glm_eval_strip$plot + 
      guides(col = "none")
    ggexport(
      plot = pout,
      filename = here(path_eval, paste0("response_curves", my_algo, ".png")), 
      width = 1000,
      height = 800, 
      res = 100,
      units = "px",
      device = "png", 
      limitsize = F
    )
  }
)

# Ensemble modelling
all_ensemble_algos <- c("EMcv", "EMca","EMwmean")
names(all_ensemble_algos) <- all_ensemble_algos
spec_ensemble_models_cpc <- BIOMOD_EnsembleModeling(
  bm.mod               = spec_models,
  em.by                = "all", 
  em.algo              = all_ensemble_algos,
  metric.select        = "TSS"
)
(spec_ensemble_models_scores <- get_evaluations(spec_ensemble_models))
ensemble_scores_names <- c(
  "metric.eval", "cutoff", "sensitivity", "specificity", "calibration"
)

# ensemble scores ----
EMscores <- all_ensemble_algos %>% lapply(
  \(a) {
    spec_ensemble_models_scores %>% 
      filter(algo == a) %>% 
      select(all_of(ensemble_scores_names))
  }
)
thlds <- lapply(EMscores, \(tb) max(tb$cutoff, na.rm = T))
thlds[which(thlds == -Inf)] <- NA

# ensemble response curve ----
lapply(
  all_ensemble_algos, 
  \(my_algo) {
    mod_eval_strip <- biomod2::bm_PlotResponseCurves(
      bm.out           = spec_ensemble_models,
      models.chosen    = BIOMOD_LoadModels(
        spec_ensemble_models, algo = my_algo
      ),
      fixed.var        = "median",
      main             = my_algo, 
      do.plot          = F
    )
    pout <- mod_eval_strip$plot + 
      guides(col = "none")
    ggexport(
      plot = pout,
      filename = here(
        path_eval, paste0("response_curves_ensemble", my_algo, ".png")), 
      width = 1000,
      height = 800, 
      res = 100,
      units = "px",
      device = "png", 
      limitsize = F
    )
  }
)

################################################################################
#### MODÈLE LOCAL - DONNÉES ENVIRONNEMENTALES SEXTANT                       ####
################################################################################

# nom du modèle
vec_name_model <- c("rf", "local", "sxt")
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

# Identifiant du modèle ----
modeling_id <- gsub(
  " ", 
  ".", 
  paste(
    binnam, 
    paste(vec_name_model, collapse = " ")
  )
)

# Formatage des données pour le modèle ----
path_models <- here("data", "analysis", "models")
makeMyDir(path_models)
spec_data_sxt <- BIOMOD_FormatingData( 
  # Données initiales
  resp.var       = bio$individualCount,
  expl.var       = clim_sub, 
  resp.xy        = st_coordinates(bio) %>%
    as_tibble() %>%  select(x = X, y = Y),
  # Modalités de sauvegarde
  dir.name       = path_models, 
  resp.name      = modeling_id, 
  # Gestion des occurrences multiples dans une cellule
  filter.raster  = TRUE
)

# Paramétrage du modèle ----
biom_options <- BIOMOD_ModelingOptions(
  MAXENT = list(
    path_to_maxent.jar = here("scripts", "maxent", "maxent.jar")
  )
)
# all_biomod2_algos <- c(
#   "GLM", "GBM", "GAM", "CTA", "ANN", "SRE", 
#   "FDA", "MARS", "RF", "MAXENT", "MAXNET"
# )
all_biomod2_algos <- "RF"
# Modélisation des habitats favorables selon une méthode ensembliste ----
cl <- startMPIcluster()
registerDoMPI(cl)
spec_models_sxt <- BIOMOD_Modeling(
  bm.options      = biom_options,
  bm.format       = spec_data,
  modeling.id     = modeling_id,
  models          = all_biomod2_algos,
  CV.nb.rep       = 5,
  data.split.perc = 80,
  var.import      = 3, 
  do.full.models  = F, 
  nb.cpu          = 2
)

path_eval <- here(
  "data", "analysis", "models", modeling_id, "eval"
)
makeMyDir(path_eval)

(spec_models_var_import <- get_variables_importance(spec_models))

# calculate the mean of variable importance by algorithm
var_importance <- dcast(
  spec_models_var_import, 
  expl.var ~ algo, 
  fun.aggregate = mean, 
  value.var = "var.imp"
)

p5 <- ggplot() +
  geom_col(
    data = var_importance, 
    aes(
      x = expl.var %>%
        factor(
          levels = expl.var[order(RF,decreasing = T)]
        ),
      y = RF
    )
  ) + 
  xlab("Variable environnementale") +
  ylab("Contribution (%)")

ggexport(
  plot = p5,
  filename = here(path_eval, "contributions_variables.png"), 
  width = 1000, 
  height = 800, 
  res = 200,
  units = "px",
  device = "png", 
  limitsize = F
)


# Models response curves
# To do this we first have to load the produced models.
lapply(
  all_biomod2_algos, 
  \(my_algo) {
    glm_eval_strip <- biomod2::bm_PlotResponseCurves(
      bm.out           = spec_models,
      models.chosen    = BIOMOD_LoadModels(spec_models, algo = my_algo), 
      fixed.var        = "median",
      main             = my_algo, 
      do.plot          = F
    )
    pout <- glm_eval_strip$plot + 
      guides(col = "none")
    ggexport(
      plot = pout,
      filename = here(path_eval, paste0("response_curves", my_algo, ".png")), 
      width = 1000,
      height = 800, 
      res = 100,
      units = "px",
      device = "png", 
      limitsize = F
    )
  }
)

# Ensemble modelling
all_ensemble_algos <- c("EMcv", "EMca","EMwmean")
names(all_ensemble_algos) <- all_ensemble_algos
spec_ensemble_models_sxt <- BIOMOD_EnsembleModeling(
  bm.mod               = spec_models,
  em.by                = "all", 
  em.algo              = all_ensemble_algos,
  metric.select        = "TSS"
)
(spec_ensemble_models_scores <- get_evaluations(spec_ensemble_models))
ensemble_scores_names <- c(
  "metric.eval", "cutoff", "sensitivity", "specificity", "calibration"
)

# ensemble scores ----
EMscores <- all_ensemble_algos %>% lapply(
  \(a) {
    spec_ensemble_models_scores %>% 
      filter(algo == a) %>% 
      select(all_of(ensemble_scores_names))
  }
)
thlds <- lapply(EMscores, \(tb) max(tb$cutoff, na.rm = T))
thlds[which(thlds == -Inf)] <- NA

# ensemble response curve ----
lapply(
  all_ensemble_algos, 
  \(my_algo) {
    mod_eval_strip <- biomod2::bm_PlotResponseCurves(
      bm.out           = spec_ensemble_models,
      models.chosen    = BIOMOD_LoadModels(
        spec_ensemble_models, algo = my_algo
      ),
      fixed.var        = "median",
      main             = my_algo, 
      do.plot          = F
    )
    pout <- mod_eval_strip$plot + 
      guides(col = "none")
    ggexport(
      plot = pout,
      filename = here(
        path_eval, paste0("response_curves_ensemble", my_algo, ".png")), 
      width = 1000,
      height = 800, 
      res = 100,
      units = "px",
      device = "png", 
      limitsize = F
    )
  }
)
