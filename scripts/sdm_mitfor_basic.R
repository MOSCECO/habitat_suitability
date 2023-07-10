# biomod routine sur l'exemple de Protea Laurifolia en Afrique du Sud
# Guisan, A., Thuiller, W., Zimmermann, N.E., 2017. Habitat Suitability and 
# Distribution Models: With Applications in R, 1st ed. Cambridge University 
# Press. https://doi.org/10.1017/9781139028271

# Sélection de l'espèce la plus abondante pour les deux îles !
# Conversion en spatial raster
env_vars_spatRaster <- sapply(
  names(env_vars_raster), 
  function(isl) {
    spatrasts <- sapply(
      names(env_vars_raster[[isl]]),
      function(nr) {
        r <- env_vars_raster[[isl]][[nr]]
        return(as(r, "SpatRaster"))
      },
      USE.NAMES = T,
      simplify = F
    )
    return(spatrasts)
  },
  USE.NAMES = T,
  simplify = F
)

# mise à la même résolution
clim  <- lapply(env_vars_spatRaster, rast)

climx <- rast(
  x          = ext(clim$MTQ), 
  resolution = res(clim$GLP),
  crs        = terra::crs(clim$MTQ)
)
# Ajustement de la résolution de la Martinique à celle de la Guadeloupe
clim$MTQ <- resample(clim$MTQ, climx, method = "near")
climosaic_rast <- mosaic(x = clim$GLP, y = clim$MTQ)
climosaic <- climosaic_rast %>% as.data.frame() %>% na.omit()
climosaic <- climosaic %>%
  mutate(dpth = log((depth - min(depth)) + 1), .keep = "unused")
climosaic_hell <- decostand(climosaic, "hellinger")
# sélection de variables qui ne covarient pas
require(ade4)
pca_mq <- dudi.pca(climosaic_hell, scannf = F, nf = 2)
x11()
plot(pca_mq$li[, 1:2])
# plot(pca_mq$li[, c(1,3)])

# occurrences d'espèces
sp <- species$majo
aphia <- names(sort(table(sp$aphiaID), decreasing = T)[1])
sp %>% filter(aphiaID == aphia) %>% select(scientificName) %>% unique()
# Mithraculus forceps la plus abondante en Guadeloupe et Martinique
# Mais ! Déséquilibre : 61 occurrences en Martinique et 27 en Guadeloupe
# Stenorhynchus seticornis (421957) plus équililbré, respectivement 26 et 22 
# occurrences
spe <- sp %>% filter(aphiaID == aphia)

spe_pts <- spe %>% select(decimalLongitude, decimalLatitude)
spe_cell <- cellFromXY(
  subset(climosaic_rast, 1),
  spe_pts
)

# Discriminate Mithraculus forceps presences from the entire 
# Martinique environmental space. 
par(mfrow = c(1:2))
s.class(
  pca_mq$li[, 1:2], 
  fac = factor(
    rownames(climosaic) %in% spe_cell, levels = c("FALSE", "TRUE"), 
    labels = c("background", "MitFor")
  ), 
  col = c("red", "blue"), 
  csta = 0, 
  cellipse = 2, 
  cpoint = .3, 
  pch = 16
)
mtext("(a)", side = 3, line = 3, adj = 0)
s.corcircle(pca_mq$co, clabel = .5 )
mtext("(b)", side = 3, line = 3, adj = 0)

# On retient les moyennes des variables : SST, CHLA, HM0 & DPTH
varenv_subset <- c("chla", "depth", "hm0", "sst")
clim_mq_sub_spatrast <- subset(climosaic_rast, varenv_subset)
require(biomod2)

path_models <- here("data", "analysis", "models")
makeMyDir(path_models)
MitFor_data <- BIOMOD_FormatingData( 
  resp.var       = rep(1, nrow(spe)), 
  expl.var       = clim_mq_sub_spatrast, 
  resp.xy        = spe[, c("decimalLongitude", "decimalLatitude")], 
  dir.name       = path_models,  
  resp.name      = "Mithraculus.forceps.basic", 
  PA.nb.rep      = 3, 
  PA.nb.absences = 500, 
  PA.strategy    = "random",
  filter.raster  = TRUE
)
# x11()
# plot(MitFor_data)

MitFor_opt <- BIOMOD_ModelingOptions(
  GLM = list(type    = "quadratic", interaction.level = 1), 
  GBM = list(n.trees = 1000), 
  GAM = list(algo    = "GAM_mgcv")
)

# MitFor_models <- BIOMOD_Modeling(
#   bm.format       = MitFor_data,
#   modeling.id     = "AllModels",
#   models          = c("GLM", "GBM", "RF", "GAM"),
#   bm.options      = MitFor_opt,
#   nb.rep          = 4,
#   data.split.perc = 80,
#   var.import      = 3,
#   do.full.models  = F
# )
# 
# saveRDS(
#   MitFor_models,
#   here(
#     "data", "analysis", "mithraculus_forceps_glm_gbm_rf_gam_basic.rds"
#   )
# )

MitFor_models <- readRDS(
  here(
    "data", "analysis", "mithraculus_forceps_glm_gbm_rf_gam_basic.rds"
  )
)

# Error message 2023-02-16
# Warning messages:
#   1: executing %dopar% sequentially: no parallel backend registered 
# 2: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                Iteration limit reached without full convergence - check carefully

# get model evaluation scores
MitFor_models_scores <- get_evaluations(MitFor_models)

p1 <- bm_PlotEvalMean(
  bm.out      = MitFor_models, 
  metric.eval = c("ROC","TSS"),
  group.by    = "algo", 
  ylim = c( 0, 1.1)
)
p2 <- bm_PlotEvalMean(
  bm.out      = MitFor_models, 
  metric.eval = c("ROC","TSS"),
  group.by    = "run", 
  ylim = c( 0, 1.1)
)
p3 <- bm_PlotEvalMean(
  bm.out      = MitFor_models, 
  metric.eval = c("ROC","TSS"),
  group.by    = "PA", 
  ylim = c( 0, 1.1)
)
require(patchwork)
(p1$plot / p2$plot) | p3$plot

(MitFor_models_var_import <- get_variables_importance(MitFor_models))

# calculate the mean of variable importance by algorithm
dcast(
  MitFor_models_var_import, 
  expl.var ~ algo, 
  fun.aggregate = mean, 
  value.var = "var.imp"
)


# Models response curves
# To do this we first have to load the produced models.
MitFor_glm <- BIOMOD_LoadModels(MitFor_models, algo = "GLM")
MitFor_gbm <- BIOMOD_LoadModels(MitFor_models, algo = "GBM")
MitFor_rf  <- BIOMOD_LoadModels(MitFor_models, algo = "RF")
MitFor_gam <- BIOMOD_LoadModels(MitFor_models, algo = "GAM")

glm_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = MitFor_models,
  models.chosen    = MitFor_glm, 
  fixed.var        = "median",
  main             = "GLM", 
  do.plot          = F
)
glm_eval_strip$plot + # mettre les couleurs par jeu de pseudo-absences ?
  guides(col = "none")

gam_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = MitFor_models,
  models.chosen    = MitFor_gam, 
  fixed.var        = "median",
  main             = "GAM", 
  do.plot          = F
)
gam_eval_strip$plot + 
  guides(col = "none")

rf_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = MitFor_models,
  models.chosen    = MitFor_rf, 
  fixed.var        = "median",
  main             = "RF", 
  do.plot          = F
)
rf_eval_strip$plot + 
  guides(col = "none")

gbm_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = MitFor_models,
  models.chosen    = MitFor_gbm, 
  fixed.var        = "median",
  main             = "GBM", 
  do.plot          = F
)
gbm_eval_strip$plot + 
  guides(col = "none")

# Ensemble modelling
MitFor_ensemble_models <- BIOMOD_EnsembleModeling(
  bm.mod               = MitFor_models,
  em.by                = "all", 
  em.algo              = c("EMcv", "EMca", "EMwmean"),
  metric.select        = "TSS",
  metric.select.thresh = 0.8
)
(MitFor_ensemble_models_scores <- get_evaluations(MitFor_ensemble_models))
ensemble_scores_names <- c(
  "metric.eval", "cutoff", "sensitivity", "specificity", "calibration"
)
MitFor_ensemble_models_scores %>% 
  filter(algo == "EMcv") %>% select(ensemble_scores_names)
MitFor_ensemble_models_scores %>% 
  filter(algo == "EMca") %>% select(ensemble_scores_names)
MitFor_ensemble_models_scores %>% 
  filter(algo == "EMwmean") %>% select(ensemble_scores_names)

### Current projections ###
MitFor_models_proj_current <- BIOMOD_Projection( 
  bm.mod          = MitFor_models,
  new.env         = clim_mq_sub_spatrast,
  proj.name       = "current",
  metric.binary   = "TSS",
  output.format   = ".img",
  do.stack        = FALSE 
)

MitFor_ensemble_models_proj_current <- BIOMOD_EnsembleForecasting(
  bm.em         = MitFor_ensemble_models, 
  # bm.proj       = MitFor_models_proj_current,
  new.env       = clim_mq_sub_spatrast,
  proj.name     = "current", 
  models.chosen = "all"
)

# Visualisation
plot(MitFor_ensemble_models_proj_current)
MitFor_proj_current_spatRast <- terra::unwrap(
  MitFor_ensemble_models_proj_current@proj.out@val
)

MitFor_pjs <- lapply(
  clim, 
  \(rs) return(terra::crop(MitFor_proj_current_spatRast, rs[["chla"]]))
)

MitFor_pjs_tb <- lapply(
  MitFor_pjs, \(sr) {
    tb <- as_tibble(
      crds(sr[[2]]) %>% 
        cbind(terra::values(sr[[2]]) %>% na.omit())
    )
    names(tb)[3] <- "value"
    return(tb)
  }
)

MitFor_pjs_plots <- mapply(
  \(tb, isl) {
    p <- ggplot() + 
      geom_tile(data = tb, aes(x = x, y = y, fill = value)) + 
      geom_sf(data = isl) + 
      # scale_fill_gradient(low = "#3e36c2", high = "#7abe76")
      scale_fill_gradient2(
        low = "grey90", mid = "yellow4", high =  "green4", midpoint = 500
      )
    x11()
    print(p)
    return(p)
  },
  MitFor_pjs_tb, 
  maps, 
  USE.NAMES = T, 
  SIMPLIFY = F
)

# Méthode dans Guisan et al. 2017
MitFor_pjs <- lapply(
  clim, 
  \(rs) {
    stk_MitFor_ef <- get_predictions(MitFor_ensemble_models_proj_current)
    stk_MitFor_ef <- subset(
      stk_MitFor_ef, 
      grep("EMca", names(stk_MitFor_ef))
    )
    names(stk_MitFor_ef) <- sapply(
      strsplit(names(stk_MitFor_ef), "_"), getElement, 2
    )
    return(terra::crop(stk_MitFor_ef, rs[["chla"]]))
  }
)

MitFor_pjs_tb <- lapply(
  MitFor_pjs, \(sr) {
    tb <- as_tibble(
      crds(sr[[1]]) %>% 
        cbind(terra::values(sr[[1]]) %>% na.omit())
    )
    names(tb)[3] <- "value"
    return(tb)
  }
)

MitFor_pjs_plots <- lapply(
  MitFor_pjs_tb,
  \(tb) {
    p <- levelplot(
      x = value ~ x * y,
      data = tb, 
      aspect = "iso",
      main = "Mithraculus forceps ensemble projections", 
      col.regions = colorRampPalette(c("grey90", "yellow4", "green4"))(100)
    )
    x11()
    print(p)
    return(p)
  }
)

x11()
plot(
  climosaic_rast %>% 
    subset(varenv_subset)
)

