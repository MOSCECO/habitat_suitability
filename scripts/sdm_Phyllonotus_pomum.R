# HABITAT SUITABILITY MODEL
# One Species
# MOSCECO - Biodiversité benthique de Martinique et de Guadeloupe

# Phyllonotus pomum

# Données biologiques ----

sp  <- pa$`Phyllonotus pomum`
bn <- pa$`Phyllonotus pomum`$scientificName %>% unique()
binnam <- str_split(unique(sp$scientificName), " ")[[1]] %>% 
  lapply(substr, 1, 3) %>% 
  paste0(collapse = ".")
# sp <- species$muri[species$muri$scientificName == bn]
# ajout des missions scientifiques
sp$expedition[is.na(sp$expedition)] <- ifelse(
  grepl("CP|DW", sp$method[is.na(sp$expedition)]),
  "KARUBENTHOS 2",
  NA
)
sp$expedition[is.na(sp$expedition)] <- ifelse(
  grepl("G", sp$method[is.na(sp$expedition)]),
  "KARUBENTHOS 2012",
  "MADIBENTHOS"
)

isl <- unique(sp$ISL)
tax <- unique(sp$TAX)

# Présences
spp <- sp %>% 
  filter(individualCount > 0)
spp_sf <- st_as_sf(
  spp, 
  coords = c("decimalLongitude", "decimalLatitude"),
  remove = F
)
# Absences
spa <- sp %>% 
  filter(individualCount == 0) %>% 
  filter(expedition != "KARUBENTHOS 2")
# On ignore la mission Karubenthos 2 qui présente un biote très différent 
# de celui de la côte... mais que pour les absences ? Sûrement un biais là.

# Données environnementales ----
mtd <- "mean"
clm <- terra::extract(climosaic_rast, spp_sf)
M <- spp %>% cbind(clm)

# données manquantes dans la matrice occurrence/environnement 
# profondeurs : problème avec le gebco
vec_dpt <- which(do.call(rbind, stations)$collectEvent %in% M$collectStation)
M$depth <- do.call(rbind, stations)$depth[vec_dpt]
# salinité : associer la salinité à 9m pour les stations manquantes
M$so[is.na(M$so)] <- M$so9[is.na(M$so)]
M <- M %>% select(-so9)
m <- M[26:ncol(M)]

# Colinéarité ----
# variance inflation factor
varenv_subset <- names(m)[-which(names(m) %in% usdm::vifstep(m)@excluded)]
m <- m %>% select(all_of(varenv_subset))
clim_sub <- terra::subset(climosaic_rast, varenv_subset)

# Création des jeux de données de différentes prévalences ---
# prevalences <- seq(0.5, 1, 0.1)
prevalences <- seq(0.9, 1, 0.1)
nabs <- ceiling(nrow(spp)*prevalences)
# absence dataset variability
# adv <- 10
adv <- 2
ABS <- lapply(
  nabs, 
  \(n) {
    out <- lapply(
      1:adv, 
      FUN = sample, 
      x = 1:nrow(spa), 
      size = n
    )
    names(out) <- paste0("d", 1:adv) # d: dataset
    return(out)
  }
)
names(ABS) <- paste0("p", prevalences) # p: prevalence

# Jeu de données biologique présence/absences ----
prvl <- names(ABS)[[2]]
dtst <- names(ABS$p1)[[1]]
abs  <- ABS[[prvl]][[dtst]]
spec_sub <- spp %>% 
  mutate(individualCount = 1) %>% 
  rbind(spa[abs, ])

# Discriminate species presences from the entire 
# Martinique environmental space.
allEnv <- terra::extract(
  climosaic_rast, 
  do.call(rbind, stations) %>% 
    filter(survey != "KARUBENTHOS 2") %>% 
    filter(!duplicated(paste(.$decimalLongitude, .$decimalLatitude)))
)
allEnv$so[is.na(allEnv$so)] <- allEnv$so9[is.na(allEnv$so)]
allEnv$depth <- do.call(rbind, stations) %>% 
  filter(survey != "KARUBENTHOS 2") %>% 
  filter(!duplicated(paste(.$decimalLongitude, .$decimalLatitude)))%>% 
  st_drop_geometry() %>% 
  select(depth) %>% 
  unlist(use.names = F)
allEnv <- allEnv %>% select(-c(ID, so9))

res <- dudi.pca(allEnv, scannf = F)

climosaic <- subset(climosaic_rast, 1) %>% 
  as.data.frame() %>% 
  na.omit()
spe_env <- cellFromXY(
  subset(climosaic_rast, 1),
  do.call(rbind, stations) %>% 
    filter(survey != "KARUBENTHOS 2") %>% 
    select(decimalLongitude, decimalLatitude) %>% 
    filter(!duplicated(.)) %>% 
    st_drop_geometry()
)
spe_cell <- cellFromXY(
  subset(climosaic_rast, 1),
  spp[, c("decimalLongitude", "decimalLatitude")] %>% 
    filter(!duplicated(.))
)

par(mfrow = c(1:2))
s.class(
  res$li[, 1:2],
  fac = factor(
    spe_env %in% spe_cell, 
    levels = c("FALSE", "TRUE"), 
    labels = c("background", binnam)
  ), 
  col = c("red", "blue"), 
  csta = 0, 
  cellipse = 2, 
  cpoint = .3, 
  pch = 16
)
mtext("(a)", side = 3, line = 3, adj = 0)
s.corcircle(res$co, clabel = .5 )
mtext("(b)", side = 3, line = 3, adj = 0)

# Formatage des paramètres du modèle ----
path_models <- here("data", "analysis", "models")
makeMyDir(path_models)
spec_data <- BIOMOD_FormatingData( 
  resp.var       = spec_sub$individualCount, 
  expl.var       = clim_sub, 
  resp.xy        = spec_sub[, c("decimalLongitude", "decimalLatitude")], 
  dir.name       = path_models, 
  resp.name      = gsub(" ", ".", paste(bn, "basic", "ensemble")), 
  filter.raster  = TRUE
)

# Modélisation des habitats favorables selon une méthode ensembliste ----
spec_models <- BIOMOD_Modeling(
  bm.format       = spec_data,
  modeling.id     = "spec.basic.ensemble",
  nb.rep          = 10,
  data.split.perc = 80,
  var.import      = 3,
  do.full.models  = F
)

# Warning messages:
#   1: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                  Iteration limit reached without full convergence - check carefully
#                2: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                               Fitting terminated with step failure - check results carefully
#                             3: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                                            Fitting terminated with step failure - check results carefully
#                                          4: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                                                         Fitting terminated with step failure - check results carefully

# Évaluation ----
# get model evaluation scores
modScores <- get_evaluations(spec_models)

p1 <- bm_PlotEvalMean(
  bm.out      = spec_models, 
  metric.eval = c("KAPPA","TSS"),
  group.by    = "algo"#, 
  # ylim = c( 0, 1.1)
)
p2 <- bm_PlotEvalMean(
  bm.out      = spec_models, 
  metric.eval = c("ROC","TSS"),
  group.by    = "run", 
  ylim = c(0, 1.5)
)
(spec_models_var_import <- get_variables_importance(spec_models))

# calculate the mean of variable importance by algorithm
dcast(
  spec_models_var_import, 
  expl.var ~ algo, 
  fun.aggregate = mean, 
  value.var = "var.imp"
)


# Models response curves
# To do this we first have to load the produced models.
spec_glm <- BIOMOD_LoadModels(spec_models, algo = "GLM")
spec_gbm <- BIOMOD_LoadModels(spec_models, algo = "GBM")
spec_rf  <- BIOMOD_LoadModels(spec_models, algo = "RF")
spec_gam <- BIOMOD_LoadModels(spec_models, algo = "GAM")

glm_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = spec_models,
  models.chosen    = spec_glm, 
  fixed.var        = "median",
  main             = "GLM", 
  do.plot          = F
)
glm_eval_strip$plot + 
  guides(col = "none")

gam_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = spec_models,
  models.chosen    = spec_gam, 
  fixed.var        = "median",
  main             = "GAM", 
  do.plot          = F
)
gam_eval_strip$plot + 
  guides(col = "none")

rf_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = spec_models,
  models.chosen    = spec_rf, 
  fixed.var        = "median",
  main             = "RF", 
  do.plot          = F
)
rf_eval_strip$plot + 
  guides(col = "none")

gbm_eval_strip <- biomod2::bm_PlotResponseCurves(
  bm.out           = spec_models,
  models.chosen    = spec_gbm, 
  fixed.var        = "median",
  main             = "GBM", 
  do.plot          = F
)
gbm_eval_strip$plot + 
  guides(col = "none")

# Ensemble modelling
spec_ensemble_models <- BIOMOD_EnsembleModeling(
  bm.mod               = spec_models,
  em.by                = "all", 
  em.algo              = c("EMcv", "EMca", "EMwmean"),
  metric.select        = "TSS"
)
(spec_ensemble_models_scores <- get_evaluations(spec_ensemble_models))
ensemble_scores_names <- c(
  "metric.eval", "cutoff", "sensitivity", "specificity", "calibration"
)
spec_ensemble_models_scores %>% 
  filter(algo == "EMcv") %>% select(ensemble_scores_names)
spec_ensemble_models_scores %>% 
  filter(algo == "EMca") %>% select(ensemble_scores_names)
spec_ensemble_models_scores %>% 
  filter(algo == "EMwmean") %>% select(ensemble_scores_names)

### Current projections ###
spec_models_proj_current <- BIOMOD_Projection( 
  bm.mod          = spec_models,
  new.env         = clim_sub,
  proj.name       = "current",
  metric.binary   = "TSS",
  output.format   = ".img",
  do.stack        = FALSE 
)

spec_ensemble_models_proj_current <- BIOMOD_EnsembleForecasting(
  bm.em         = spec_ensemble_models, 
  # bm.proj       = spec_models_proj_current,
  new.env       = clim_sub,
  proj.name     = "current", 
  models.chosen = "all"
)

# Visualisation ----
plot(spec_ensemble_models_proj_current)
spec_proj_current_spatRast <- terra::unwrap(
  spec_ensemble_models_proj_current@proj.out@val
)
# crs(spec_proj_current_spatRast) <- "epsg:4326"
# clim <- lapply(clim, \(e) {crs(e) <- "epsg:4326"; return(e)})

spec_pjs <- lapply(
  clim, 
  \(rs) return(terra::crop(spec_proj_current_spatRast, rs[["chla"]]))
)

spec_pjs_tb <- lapply(
  spec_pjs, \(sr) {
    tb <- as_tibble(
      crds(sr[[2]]) %>% 
        cbind(terra::values(sr[[2]]) %>% na.omit())
    )
    names(tb)[3] <- "value"
    return(tb)
  }
)

epsg9122 <- wkt(spec_pjs$GLP %>% raster())
spec_pjs_plots <- mapply(
  \(tb, isl) {
    isl <- st_transform(isl, crs = epsg9122)
    p <- ggplot() + 
      geom_tile(data = tb, aes(x = x, y = y, fill = value)) + 
      geom_sf(data = isl) + 
      # scale_fill_gradient(low = "#3e36c2", high = "#7abe76")
      # scale_fill_gradient2(
      #   low = "grey90", mid = "yellow4", high =  "green4", midpoint = 500
      # )
      scale_fill_viridis_c()
    x11()
    print(p)
    return(p)
  },
  spec_pjs_tb, 
  maps, 
  USE.NAMES = T, 
  SIMPLIFY = F
)

thld <- 750
spec_pjs_plots <- mapply(
  \(tb, isl) {
    p <- ggplot() + 
      geom_tile(data = tb, aes(x = x, y = y, fill = value >= thld)) + 
      geom_sf(data = isl, alpha = 0.5) +
      # scale_fill_gradient(low = "#3e36c2", high = "#7abe76")
      # scale_fill_gradient2(
      #   low = "grey90", mid = "yellow4", high =  "green4", midpoint = 500
      # )
      scale_fill_manual(values = c("blue", "darkgreen"))
    x11()
    print(p)
    return(p)
  },
  spec_pjs_tb, 
  maps, 
  USE.NAMES = T, 
  SIMPLIFY = F
)

# Méthode dans Guisan et al. 2017
spec_pjs <- lapply(
  clim, 
  \(rs) {
    stk_spec_ef <- get_predictions(spec_ensemble_models_proj_current)
    stk_spec_ef <- subset(
      stk_spec_ef, 
      grep("EMca", names(stk_spec_ef))
    )
    names(stk_spec_ef) <- sapply(
      strsplit(names(stk_spec_ef), "_"), getElement, 2
    )
    return(terra::crop(stk_spec_ef, rs[["chla"]]))
  }
)

spec_pjs_tb <- lapply(
  spec_pjs, \(sr) {
    tb <- as_tibble(
      crds(sr[[1]]) %>% 
        cbind(terra::values(sr[[1]]) %>% na.omit())
    )
    names(tb)[3] <- "value"
    return(tb)
  }
)

spec_pjs_plots <- lapply(
  spec_pjs_tb,
  \(tb) {
    p <- levelplot(
      x = value ~ x * y,
      data = tb, 
      aspect = "iso",
      main = paste(bn, "ensemble projections"), 
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