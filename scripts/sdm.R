# biomod routine sur l'exemple de Protea Laurifolia en Afrique du Sud
# Guisan, A., Thuiller, W., Zimmermann, N.E., 2017. Habitat Suitability and 
# Distribution Models: With Applications in R, 1st ed. Cambridge University 
# Press. https://doi.org/10.1017/9781139028271

# regroupement des données environnementales
extents <- list(
  GLP = list(
    x = c(-62.00949, -60.647),
    y = c(15.65, 16.802)
  ),
  MTQ = list(
    x = c(-61.33667, -60.64367 ),
    y = c(14.27667, 15.02067)
  )
)

env_vars_raster <- lapply(
  islands, 
  function(isl) {
    env_vars <- lapply(
      list.files(here("data", "tidy", "climatology", isl)),
      function(vrv) {
        r <- list.files(
          here("data", "tidy", "climatology", isl, vrv),
          pattern = "*mean*",
          full.names = T
        ) %>% 
          raster()
        extent(r) <- extents[[isl]]
        return(r)
      }
    )
    names(env_vars) <- list.files(here("data", "tidy", "climatology", isl))
    
    a <- lapply(env_vars, res)
    b <- paste0(a$chla, collapse = ";")
    p <- lapply(a, function(x) paste0(x, collapse = ";") != b) %>% unlist()
    env_resample <- lapply(
      env_vars[p], 
      resample, 
      y = env_vars[!p][[1]],
      method = "bilinear"
    )
    env_vars[p] <- env_resample
    return(stack(env_vars))
  }
)

# lapply(
#   env_vars,
#   function(v) {
# 
#     # x11()
#     # plot(v)
# 
#     extent(v)
# 
#     # raster::res(v)
# 
#   }
# )

lapply(
  env_vars_raster,
  function(x) {
    x11()
    plot(x)
  }
)

# On commence par la Martinique avec une espèce abondante
clim <- env_vars_raster$MTQ %>% as.data.frame() %>% na.omit()
# clim <- clim %>%
#   mutate(depth_exp = exp(depth), .keep = "unused")
clim <- clim %>%
  mutate(dpth = log((depth - min(depth)) + 1), .keep = "unused")
clim_hell <- decostand(clim, "hellinger")
# sélection de variables qui ne covarient pas
require(ade4)
pca_mq <- dudi.pca(clim_hell, scannf = F, nf = 2)
x11()
plot(pca_mq$li[, 1:2])

# occurrences d'espèces
sp <- species$majo %>% 
  filter(country == "MQ")
aphia <- names(sort(table(sp$aphiaI), decreasing = T)[1])
sp %>% filter(aphiaID == aphia) %>% select(scientificName) %>% unique()
spe <- sp %>% 
  filter(aphiaID == aphia)

spe_pts <- spe %>% select(decimalLongitude, decimalLatitude)
spe_cell <- cellFromXY(
  subset(env_vars_raster$MTQ, 1),
  spe_pts
)

# Discriminate Mithraculus forceps presences from the entire 
# Martinique environmental space. 
par(mfrow = c(1:2))
s.class(
  pca_mq$li[, 1:2], 
  fac = factor(
    rownames(clim) %in% spe_cell, levels = c("FALSE", "TRUE"), 
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

# On retient les moyennes des variables : SST, SBT, CHLA, HM0 & DPTH
require(terra)
clim_mq_sub <- subset(env_vars_raster$MTQ, c("chla", "hm0", "sst", "sbt", "depth"))
clim_mq_sub_spatrast <- rast(
  lapply(
    names(clim_mq_sub), 
    function(nr) {
      r <- clim_mq_sub[[nr]]
      return(as(r, "SpatRaster"))
    }
  )
)
require(biomod2)

MitFor_data <- BIOMOD_FormatingData( 
  resp.var       = rep(1, nrow(spe)), 
  expl.var       = clim_mq_sub_spatrast, 
  resp.xy        = spe[, c("decimalLongitude", "decimalLatitude")], 
  resp.name      = "Mithraculus.forceps", 
  PA.nb.rep      = 3, 
  PA.nb.absences = 500, 
  PA.strategy    = "random"
)
# x11()
# plot(MitFor_data)

MitFor_opt <- BIOMOD_ModelingOptions(
  GLM = list(type    = "quadratic", interaction.level = 1), 
  GBM = list(n.trees = 1000), 
  GAM = list(algo    = "GAM_mgcv")
)

MitFor_models <- BIOMOD_Modeling(
  bm.format       = MitFor_data, 
  modeling.id     = "AllModels", 
  models          = c("GLM", "GBM", "RF", "GAM"), 
  bm.options      = MitFor_opt, 
  nb.rep          = 4, 
  data.split.perc = 80, 
  var.import      = 3, 
  do.full.models  = F
)

saveRDS(
  MitFor_models,
  here(
    "data", "analysis", "tests_mithraculus_forceps_models_biomod2_with_depth.rds"
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
  proj.name       = "current_depth",
  metric.binary   = "TSS",
  output.format   = ".img",
  do.stack        = FALSE 
)

MitFor_ensemble_models_proj_current <- BIOMOD_EnsembleForecasting(
  bm.em         = MitFor_ensemble_models, 
  # bm.proj       = MitFor_models_proj_current,
  new.env       = clim_mq_sub_spatrast,
  proj.name     = "current_depth", 
  models.chosen = "all"
)

r <- raster(here("Mithraculus.forceps/proj_current_depth/proj_current_depth_Mithraculus.forceps_ensemble.tif"))
tb <- as_tibble(coordinates(r) %>% cbind(value = values(r)))
ggplot() + 
  geom_tile(data = tb, aes(x = x, y = y, fill = value)) + 
  geom_sf(data = maps$MTQ)

plot(MitFor_ensemble_models_proj_current)
stk_MitFor_ef <- get_predictions(MitFor_ensemble_models_proj_current)
stk_MitFor_ef <- subset(
  stk_MitFor_ef, 
  grep("EMca", names(stk_MitFor_ef))
)
names(stk_MitFor_ef) <- sapply(
  strsplit(names(stk_MitFor_ef), "_"), getElement, 2
)

tb_rast_emca <- as_tibble(
  crds(stk_MitFor_ef) %>% 
    cbind(values(stk_MitFor_ef) %>% na.omit())
)
names(tb_rast_emca)[3] <- "value"

levelplot(
  x = value ~ x * y,
  data = tb_rast_emca, 
  main = "Mithraculus forceps ensemble projections", 
  col.regions = colorRampPalette(c("grey90", "yellow4", "green4"))(100)
)

ggplot() + 
  geom_tile(data = tb_rast_emca, aes(x = x, y = y, fill = value)) + 
  geom_sf(data = maps$MTQ) + 
  # scale_fill_gradient(low = "#3e36c2", high = "#7abe76")
  scale_fill_gradient2(
    low = "grey90", mid = "yellow4", high =  "green4", midpoint = 500
  )