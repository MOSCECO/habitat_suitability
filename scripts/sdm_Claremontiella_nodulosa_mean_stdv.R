# HABITAT SUITABILITY MODEL
# One Species
# MOSCECO - Biodiversité benthique de Martinique et de Guadeloupe

# nom du modèle
vec_name_model <- c("rasterized", "nearest", "basic")
pts_name_model <- paste(vec_name_model, collapse = ".")

# Claremontiella nodulosa

# Données biologiques ----
bn <- "Claremontiella nodulosa"
sp  <- pa[[bn]] %>% as.data.frame(xy = T)
binnam <- str_split(bn, " ")[[1]] %>% 
  lapply(substr, 1, 3) %>% 
  paste0(collapse = ".")

# Présences
spp <- sp %>% 
  filter(individualCount > 0)
spp_sf <- st_as_sf(
  spp, 
  coords = c("x", "y"),
  remove = F,
  crs = "EPSG:4326"
)
# Absences
spa <- sp %>% 
  filter(individualCount == 0)
spa_sf <- st_as_sf(
  spa, 
  coords = c("x", "y"),
  remove = F,
  crs = "EPSG:4326"
)

# ggplot() +
#   geom_tile(
#     data = as.data.frame(climosaic_sub, xy = T), 
#     aes(x, y, fill = PC1)
#   ) +
#   geom_sf(data = spa_sf, col = "red") +
#   geom_sf(data = spp_sf, col = "pink")

# Données environnementales ----
clm <- terra::extract(climosaic_sub, spp_sf)

# CRéation de la matrice bio/env
M <- spp %>% cbind(clm)

# Colinéarité ----
# ACP = orthogonalité
varenv_subset <- names(M)[-which(names(M) %in% 
                                   c("ID", "x", "y", "individualCount"))]
m <- M %>% select(all_of(varenv_subset))

# jeux de données pour les SDM ----
clim_sub <- climosaic_sub
spec_sub <- spp %>% rbind(spa)

# ggplot() + 
#   geom_point(
#     data = spec_sub, 
#     aes(x = x, y = y, col = individualCount > 0)
#   ) + 
#   geom_sf(data = maps$GLP) + 
#   geom_sf(data = maps$MTQ)

# Discriminate species presences from the entire 
# Martinique environmental space.
allEnv <- terra::extract(
  climosaic_sub, 
  stations_nearest$ANT %>% 
    filter(survey != "KARUBENTHOS 2") %>% 
    filter(!duplicated(paste(.$decimalLongitude, .$decimalLatitude)))
)
res <- dudi.pca(allEnv[, -1], scannf = F)

m_extract <- stations_nearest$ANT %>% 
  filter(survey != "KARUBENTHOS 2") %>% 
  select(decimalLongitude, decimalLatitude) %>% 
  filter(!duplicated(.)) %>% 
  st_drop_geometry() %>% 
  as.matrix()
spe_env <- terra::cellFromXY(climosaic_sub, m_extract)
spe_cell <- cellFromXY(
  climosaic_sub,
  spp[, c("x", "y")] %>% 
    filter(!duplicated(.))
)

x11()
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

modeling_id <- gsub(
  " ", 
  ".", 
  paste(
    binnam, 
    paste(vec_name_model, collapse = " "), 
    "ensemble"
  )
)

# Niche écologique observée ----
neo_data <- terra::extract(clim_sub, spp_sf, ID = F)
neo_dens <- lapply(
  names(neo_data), 
  \(varenv) {
    p <- ggplot(data = neo_data, aes(x = get(varenv), col = 1, fill = 2)) + 
      geom_density(alpha = 0.6) +
      scale_fill_viridis_c() + 
      scale_color_viridis_c() +
      guides(fill = "none", col = "none") + 
      labs(x = varenv, y = "Densité")
  }
)
neo_grph <- Reduce(`+`, neo_dens)

# Biais environnemental ----
# estimation du biais environnemental présent dans les données
qtl <- 0.01
myAlpha <- 0.5
plot_env_bias <- lapply(
  names(neo_data),
  \(varenv) {
    phst <- ggplot() + 
      geom_histogram(
        data = as.data.frame(clim_sub), 
        aes(x = get(varenv), y = ..density.., fill = "Grille environnementale"), 
        alpha = myAlpha, 
        bins = 30
      ) +
      geom_histogram(
        data = neo_data, 
        aes(x = get(varenv), y = ..density.., fill = "Occurrences de l'espèce"), 
        alpha = myAlpha,
        bins = 30
      ) + 
      scale_fill_manual(values = c("green", "red")) + 
      xlab(varenv) + 
      ylab("Densité") +
      scale_x_continuous(
        limits = c(
          min(
            quantile(neo_data[[varenv]], qtl), 
            quantile(as.data.frame(clim_sub)[[varenv]], qtl)
          ), 
          max(
            quantile(neo_data[[varenv]], 1 - qtl), 
            quantile(as.data.frame(clim_sub)[[varenv]], 1 - qtl)
          )
        )
      ) +
      guides(fill = guide_legend(title = NULL)) +
      theme(
        text = element_text(size = 15),
        legend.position = "bottom"
      )
    
    pbox_env <- ggplot() + 
      geom_boxplot(
        data = as.data.frame(clim_sub), 
        aes(x = get(varenv)),
        fill = "green", 
        col = "darkgreen", 
        alpha = 0.7
      ) +
      xlab(varenv) + 
      scale_x_continuous(
        limits = c(
          min(
            quantile(neo_data[[varenv]], qtl), 
            quantile(as.data.frame(clim_sub)[[varenv]], qtl)
          ), 
          max(
            quantile(neo_data[[varenv]], 1 - qtl), 
            quantile(as.data.frame(clim_sub)[[varenv]], 1 - qtl)
          )
        )
      ) + 
      theme_void()
    pbox_spe <- ggplot() +
      geom_boxplot(
        data = neo_data, 
        aes(x = get(varenv)),
        fill = "red", 
        col = "darkred",
        alpha = 0.7
      ) +
      xlab(varenv) + 
      scale_x_continuous(
        limits = c(
          min(
            quantile(neo_data[[varenv]], qtl), 
            quantile(as.data.frame(clim_sub)[[varenv]], qtl)
          ), 
          max(
            quantile(neo_data[[varenv]], 1 - qtl), 
            quantile(as.data.frame(clim_sub)[[varenv]], 1 - qtl)
          )
        )
      ) + 
      theme_void()
    
    pbox_spe / pbox_env / phst + plot_layout(heights = c(0.1, 0.1, 0.8))
  }
)
names(plot_env_bias) <- names(clim_sub)

# Formatage des données pour le modèle ----
path_models <- here("data", "analysis", "models")
makeMyDir(path_models)
spec_data <- BIOMOD_FormatingData( 
  resp.var       = spec_sub$individualCount,
  expl.var       = clim_sub, 
  resp.xy        = spec_sub[, c("x", "y")], 
  dir.name       = path_models, 
  resp.name      = modeling_id, 
  filter.raster  = TRUE 
)

# Paramétrage du modèle ----
biom_options <- BIOMOD_ModelingOptions(
  MAXENT = list(
    path_to_maxent.jar = here("scripts", "maxent", "maxent.jar")
  )
)
all_biomod2_algos <- c(
  "GLM", "GBM", "GAM", "CTA", "ANN", "SRE", 
  "FDA", "MARS", "RF", "MAXENT", "MAXNET"
)
# Modélisation des habitats favorables selon une méthode ensembliste ----
cl <- startMPIcluster()
registerDoMPI(cl)
spec_models <- BIOMOD_Modeling(
  bm.options      = biom_options,
  bm.format       = spec_data,
  modeling.id     = modeling_id,
  models          = all_biomod2_algos,
  nb.rep          = 5,
  data.split.perc = 80,
  var.import      = 3, 
  do.full.models = F, 
  nb.cpu = 2
)

# Warning messages:
#   1: executing %dopar% sequentially: no parallel backend registered 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 3: glm.fit: fitted probabilities numerically 0 or 1 occurred                                                          Fitting terminated with step failure - check results carefully

# sauvegarde niche écologique/biais environnemental ----
path_neo <- here("data", "analysis", "models", modeling_id, "nicheEcoObs")
makeMyDir(path_neo)
file_name <- binnam %>% 
  paste("ecological", "niche", "observed", sep = "_") %>% 
  paste0(".png")
ggexport(
  plot = neo_grph,
  filename = here(path_neo, file_name), 
  width = 2000, 
  height = 1000, 
  res = 200,
  units = "px",
  device = "png", 
  limitsize = F
)

path_biav <- here("data", "analysis", "models", modeling_id, "biasEnv")
makeMyDir(path_biav)
lapply(
  names(plot_env_bias), 
  \(varenv) {
    file_name <- binnam %>% 
      paste("environmental", "bias", varenv, sep = "_") %>% 
      paste0(".png")
    ggexport(
      plot = plot_env_bias[[varenv]],
      filename = here(path_biav, file_name), 
      width = 2000, 
      height = 1500, 
      res = 200,
      units = "px",
      device = "png", 
      limitsize = F
    )
  }
)

# Évaluation ----
# get model evaluation scores
modScores <- get_evaluations(spec_models)
modScoresSummary <- modScores %>% 
  group_by(algo, metric.eval) %>% 
  summarise(
    cutoff_mean = mean(cutoff),
    cutoff_stdv = sd(cutoff),
    sensitivity_mean = mean(sensitivity),
    sensitivity_stdv = sd(sensitivity),
    specificity_mean = mean(specificity),
    specificity_stdv = sd(specificity),
    calibration_mean = mean(calibration),
    calibration_stdv = sd(calibration),
    validation_mean = mean(validation),
    validation_stdv = sd(validation)
  )

path_eval <- here(
  "data", "analysis", "models", modeling_id, "eval"
)
makeMyDir(path_eval)
file_name <- gsub(" ", ".", bn) %>% 
  paste(pts_name_model, "evaluation", "summary", sep = "_") %>% 
  paste0(".csv")
write.csv(
  modScoresSummary,
  here(path_eval, file_name),
  row.names = F, 
  fileEncoding = "UTF-16"
)

# Graphique des évaluations ----

p1 <- bm_PlotEvalMean(
  bm.out      = spec_models, 
  metric.eval = c("ROC","TSS"),
  group.by    = "algo", 
  # dataset = "validation", 
  do.plot = T
)
# p11 <- bm_PlotEvalBoxplot(
#   bm.out      = spec_models, 
#   group.by    = c("algo", "run")
# ) + geom_boxplot()
ggexport(
  plot = p1$plot,
  filename = here(path_eval, "TSSfROC_algo.png"), 
  width = 1000, 
  height = 800, 
  res = 200,
  units = "px",
  device = "png", 
  limitsize = F
)
p2 <- bm_PlotEvalMean(
  bm.out      = spec_models, 
  metric.eval = c("ROC","TSS"),
  group.by    = "run"
)
ggexport(
  plot = p2$plot,
  filename = here(path_eval, "TSSfROC_runs.png"), 
  width = 1000, 
  height = 800, 
  res = 200,
  units = "px",
  device = "png", 
  limitsize = F
)
p3 <- bm_PlotEvalMean(
  bm.out      = spec_models, 
  metric.eval = c("KAPPA","TSS"),
  group.by    = "algo"
)
ggexport(
  plot = p3$plot,
  filename = here(path_eval, "TSSfKAP_algo.png"), 
  width = 1000, 
  height = 800, 
  res = 200,
  units = "px",
  device = "png", 
  limitsize = F
)

p4 <- bm_PlotEvalMean(
  bm.out      = spec_models, 
  metric.eval = c("KAPPA","TSS"),
  group.by    = "run"
)
ggexport(
  plot = p4$plot,
  filename = here(path_eval, "TSSfKAP_runs.png"), 
  width = 1000, 
  height = 800, 
  res = 200,
  units = "px",
  device = "png", 
  limitsize = F
)

(spec_models_var_import <- get_variables_importance(spec_models))

# calculate the mean of variable importance by algorithm
var_importance <- dcast(
  spec_models_var_import, 
  expl.var ~ algo, 
  fun.aggregate = mean, 
  value.var = "var.imp"
)
vmean <- (apply(var_importance[, -1], 1, mean) %>% round(3))*100
vstdv <- (apply(var_importance[, -1], 1, sd) %>% round(3))*100
vstde <- ((apply(var_importance[, -1], 1, sd)/sqrt(10)) %>% round(3))*100
vmesd <- paste(
  (apply(var_importance[, -1], 1, mean) %>% round(3))*100, 
  "\u00b1", 
  (apply(var_importance[, -1], 1, sd) %>% round(3))*100
)
var_importance$mean <- vmean
var_importance$stdv <- vstdv
var_importance$stde <- vstde
var_importance$`mean+/-se` <- vmesd
var_importance %>% 
  select(expl.var, mean) %>% 
  group_by(mean) %>% 
  arrange(.by_group = T)
# var_importance[, 2:(ncol(var_importance)-1)] <- round(
#   var_importance[, 2:(ncol(var_importance)-1)]*100, 
#   2
# )
file_name <- gsub(" ", ".", bn) %>% 
  paste(pts_name_model, "variables", "importance", sep = "_") %>% 
  paste0(".csv")
write.csv(
  var_importance,
  here(path_eval, file_name),
  row.names = F, 
  fileEncoding = "UTF-16"
)

p5 <- ggplot() +
  geom_col(
    data = var_importance, 
    aes(
      x = expl.var %>%
        factor(
          levels = expl.var[order(mean,decreasing = T)]
        ),
      y = mean
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
spec_ensemble_models <- BIOMOD_EnsembleModeling(
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

### Current projections ####
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

path_EM <- here("data", "analysis", "models", modeling_id, "ensemble")
makeMyDir(path_EM)
path_figEM <- here(path_EM, "figures")
makeMyDir(path_figEM)

spec_pjs_plots <- mapply(
  \(EMalg, col_optn) {
    # EMalg <- "Cla.nod.rasterized.pca.nearest.basic.ensemble_EMcvByTSS_mergedData_mergedRun_mergedAlgo"
    # col_optn <- "E"
    sr <- spec_proj_current_spatRast[[EMalg]]
    ps <- lapply(
      islands, 
      \(nisl) {
        # nisl <- "GLP"
        
        # chargement des éléments du graphe
        isl          <- maps[[nisl]]
        e            <- ext(climatologies[[nisl]])
        sr_crop      <- terra::crop(sr, e) 
        tb           <- as.data.frame(sr_crop, xy = T)
        names(tb)[3] <- "value"
        occ <- spp_sf %>% st_crop(as.vector(e)[c(1,3,2,4)])
        
        # figures ggplot2
        p <- ggplot() + 
          geom_tile(data = tb, aes(x = x, y = y, fill = value)) + 
          geom_sf(data = isl) + 
          scale_fill_viridis_c(option = col_optn) + 
          labs(x = "Longitude", y = "Latitude") + 
          guides(fill = guide_colorbar("Probabilité\nd'occurrence"))
        pocc <- p + 
          geom_sf(data = occ, col = "red", shape = "+", size = 5)
        
        # nom des fichiers de sauvegarde
        file_name <- modeling_id %>% 
          paste(nisl, EMalg, "probability", "occurrence", "map", sep = "_") %>%
          paste0(".png")
        file_name_occ <- modeling_id %>% 
          paste(
            nisl, EMalg, "probability", "occurrence", "map", "occ", sep = "_"
          ) %>%
          paste0(".png")
        
        # sauvegarde
        ggexport(
          plot = p,
          filename = here(path_figEM, file_name),
          width = 1000,
          height = 800, 
          res = 100,
          units = "px",
          device = "png", 
          limitsize = F
        )
        ggexport(
          plot = pocc,
          filename = here(path_figEM, file_name_occ),
          width = 1000,
          height = 800, 
          res = 100,
          units = "px",
          device = "png", 
          limitsize = F
        )
        
        # préparation de la seconde carte sans certains éléments graphiques
        p <- if(nisl == "MTQ") {
          p + 
            theme(
              axis.title.y = element_blank(), 
              axis.line.y  = element_blank(), 
              axis.text.y  = element_blank(), 
              axis.ticks.y = element_blank()
            )
        } else {
          p + theme(legend.position = "none")
        }
        
        pocc <- if(nisl == "MTQ") {
          pocc + 
            theme(
              axis.title.y = element_blank(), 
              axis.line.y  = element_blank(), 
              axis.text.y  = element_blank(), 
              axis.ticks.y = element_blank()
            )
        } else {
          pocc + theme(legend.position = "none")
        }
        return(list(nocc = p, pocc = pocc))
      }
    )
    
    P    <- Reduce(`+`, ps %>% lapply(pluck, 1))
    Pocc <- Reduce(`+`, ps %>% lapply(pluck, 2))
    
    file_name <- modeling_id %>% 
      paste("ANT", EMalg, "probability", "occurrence", "map", sep = "_") %>%
      paste0(".png")
    file_name_occ <- modeling_id %>% 
      paste(
        "ANT", EMalg, "probability", "occurrence", "map", "occ", sep = "_"
      ) %>%
      paste0(".png")
    
    ggexport(
      plot = P,
      filename = here(path_figEM, file_name),
      width = 4200,
      height = 2000, 
      res = 200,
      units = "px",
      device = "png", 
      limitsize = F
    )
    ggexport(
      plot = Pocc,
      filename = here(path_figEM, file_name_occ),
      width = 4200,
      height = 2000, 
      res = 200,
      units = "px",
      device = "png", 
      limitsize = F
    )
    
    return(P)
  },
  names(spec_proj_current_spatRast),
  c("E", "C", "D"),
  SIMPLIFY = F, 
  USE.NAMES = T
)

# Présences au-dessus d'un seuil déterminé ----
spec_inc_plots <- mapply(
  \(EMalg, thld) {
    # EMalg <- "Cla.nod.rasterized.pca.nearest.basic.ensemble_EMcaByTSS_mergedData_mergedRun_mergedAlgo"
    # thld    <- 894
    sr <- spec_proj_current_spatRast[[EMalg]]
    ps <- lapply(
      islands, 
      \(nisl) {
        
        # chargement des éléments du graphe
        isl          <- maps[[nisl]]
        e            <- ext(climatologies[[nisl]])
        sr_crop      <- terra::crop(sr, e) 
        tb           <- as.data.frame(sr_crop, xy = T)
        names(tb)[3] <- "value"
        occ <- spp_sf %>% st_crop(as.vector(e)[c(1,3,2,4)])
        
        # figures ggplot2
        p <- ggplot() + 
          geom_tile(data = tb, aes(x = x, y = y, fill = value >= thld)) + 
          geom_sf(data = isl) +
          scale_fill_manual(
            values = c("lightblue", "darkgreen"),
            labels = c("Absence", "Présence")
          ) + 
          labs(x = "Longitude", y = "Latitude") + 
          guides(fill = guide_legend(paste0("Seuil = ", thld)))
        pocc <- p + 
          geom_sf(data = occ, col = "red", shape = "+", size = 5)
        
        # noms des fichiers de sauvegarde
        file_name <- modeling_id %>% 
          paste(nisl, EMalg, "incidence", "map", sep = "_") %>%
          paste0(".png")
        file_name_occ <- modeling_id %>% 
          paste(nisl, EMalg, "incidence", "map", "occ", sep = "_") %>%
          paste0(".png")
        
        # sauvegarde
        ggexport(
          plot = p,
          filename = here(path_figEM, file_name),
          width = 1000,
          height = 800, 
          res = 100,
          units = "px",
          device = "png", 
          limitsize = F
        )
        ggexport(
          plot = pocc,
          filename = here(path_figEM, file_name_occ),
          width = 1000,
          height = 800, 
          res = 100,
          units = "px",
          device = "png", 
          limitsize = F
        )
        
        # préparation de la seconde carte sans certains éléments graphiques
        p <- if(nisl == "MTQ") {
          p + 
            theme(
              axis.title.y = element_blank(), 
              axis.line.y  = element_blank(), 
              axis.text.y  = element_blank(), 
              axis.ticks.y = element_blank()
            )
        } else {
          p + theme(legend.position = "none")
        }
        
        pocc <- if(nisl == "MTQ") {
          pocc + 
            theme(
              axis.title.y = element_blank(), 
              axis.line.y  = element_blank(), 
              axis.text.y  = element_blank(), 
              axis.ticks.y = element_blank()
            )
        } else {
          pocc + theme(legend.position = "none")
        }
        
        return(list(nocc = p, pocc = pocc))
      }
    )
    
    P    <- Reduce(`+`, ps %>% lapply(pluck, 1))
    Pocc <- Reduce(`+`, ps %>% lapply(pluck, 2))
    
    file_name <- modeling_id %>% 
      paste("ANT", EMalg, "incidence", "map", sep = "_") %>%
      paste0(".png")
    file_name_occ <- modeling_id %>% 
      paste("ANT", EMalg, "incidence", "map", "occ", sep = "_") %>%
      paste0(".png")
    
    ggexport(
      plot = P,
      filename = here(path_figEM, file_name),
      width = 4200,
      height = 2000, 
      res = 200,
      units = "px",
      device = "png", 
      limitsize = F
    )
    ggexport(
      plot = Pocc,
      filename = here(path_figEM, file_name_occ),
      width = 4200,
      height = 2000, 
      res = 200,
      units = "px",
      device = "png", 
      limitsize = F
    )
    
    return(P)
  },
  names(spec_proj_current_spatRast)[-1],
  thlds[-1],
  SIMPLIFY = F, 
  USE.NAMES = T
)