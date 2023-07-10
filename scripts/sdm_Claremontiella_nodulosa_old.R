# HABITAT SUITABILITY MODEL
# One Species
# MOSCECO - Biodiversité benthique de Martinique et de Guadeloupe

# seed
set.seed(2023)

# Claremontiella nodulosa

# Données biologiques ----

sp  <- pa$`Claremontiella nodulosa`
bn <- pa$`Claremontiella nodulosa`$scientificName %>% unique()
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

ggplot() + 
  geom_point(
    data = spec_sub, 
    aes(x = decimalLongitude, y = decimalLatitude, col = individualCount > 0)
  ) + 
  geom_sf(data = maps$GLP) + 
  geom_sf(data = maps$MTQ)

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

# Formatage des données pour le modèle ----
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
spec_models <- BIOMOD_Modeling(
  bm.options      = biom_options,
  bm.format       = spec_data,
  modeling.id     = paste(gsub(" ", ".", bn), "basic", "ensemble", sep = "."),
  models          = all_biomod2_algos,
  nb.rep          = 10,
  data.split.perc = 80,
  var.import      = 10,
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
  "data", "analysis", "models", gsub(" ", ".", bn) %>% 
    paste0(".basic.ensemble"), "eval"
)
makeMyDir(path_eval)
file_name <- gsub(" ", ".", bn) %>% paste0(".basic_evaluation_summary.csv")
write.csv(
  modScoresSummary,
  here(path_eval, file_name),
  row.names = F, 
  fileEncoding = "UTF-16"
)

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
file_name <- gsub(" ", ".", bn) %>% paste0(".basic_variables_importance.csv")
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
      select(ensemble_scores_names)
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
# crs(spec_proj_current_spatRast) <- "epsg:4326"
# clim <- lapply(clim, \(e) {crs(e) <- "epsg:4326"; return(e)})

spec_pjs <- lapply(
  clim, 
  \(rs) return(terra::crop(spec_proj_current_spatRast, rs[["chla"]]))
)

spec_pjs_tb <- lapply(
  islands, 
  \(isl) {
    sr <- spec_pjs[[isl]]
    out <- lapply(
      sr,
      \(r) {
        tb <- as_tibble(
          crds(r) %>% 
            cbind(terra::values(r) %>% na.omit())
        )
        names(tb)[3] <- "value"
        return(tb)
      }
    )
    names(out) <- names(sr) %>% 
      str_split("_") %>% 
      lapply(pluck, 2) %>% 
      unlist()
    return(out)
  }
) %>% 
  transpose()

epsg9122 <- wkt(spec_pjs$GLP %>% raster())

path_EM <- here(
  "data", "analysis", "models", gsub(" ", ".", bn) %>% 
    paste0(".basic.ensemble"), "ensemble"
)
makeMyDir(path_EM)
path_figEM <- here(path_EM, "figures")
makeMyDir(path_figEM)

spec_pjs_plots <- mapply(
  \(EMalg, col_optn) {
    tbs <- spec_pjs_tb[[EMalg]]
    ps <- mapply(
      \(tb, nisl) {
        isl <- maps[[nisl]]
        isl <- st_transform(isl, crs = epsg9122)
        p <- ggplot() + 
          geom_tile(
            data = tb, 
            aes(x = x, y = y, fill = value)
          ) + 
          geom_sf(data = isl) + 
          # scale_fill_gradient(low = "#3e36c2", high = "#7abe76")
          # scale_fill_gradient2(
          #   low = "grey90", mid = "yellow4", high =  "green4", midpoint = 500
          # )
          scale_fill_viridis_c(option = col_optn) + 
          labs(
            x = "Longitude", y = "Latitude" 
          ) + 
          guides(fill = guide_colorbar("Probabilité\nd'occurrence"))
        
        file_name <- gsub(" ", ".", bn) %>% 
          paste("ensemble", "basic", sep = ".") %>% 
          paste(nisl, EMalg, "probability", "occurrence", "map", sep = "_") %>%
          paste0(".png")
        
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
        return(p)
      },
      tbs, 
      names(maps), 
      USE.NAMES = T, 
      SIMPLIFY = F
    )
    
    P <- Reduce(`+`, ps)
    
    file_name <- gsub(" ", ".", bn) %>% 
      paste("ensemble", "basic", sep = ".") %>% 
      paste("ANT", EMalg, "probability", "occurrence", "map", sep = "_") %>%
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
    
    return(P)
  },
  names(spec_pjs_tb),
  c("E", "C", "D"),
  SIMPLIFY = F, 
  USE.NAMES = T
)

# Présences au-dessus d'un seuil déterminé ----
spec_pjs_plots <- mapply(
  \(EMalg, thld) {
    tbs <- spec_pjs_tb[[EMalg]]
    ps <- mapply(
      \(tb, nisl) {
        isl <- maps[[nisl]]
        isl <- st_transform(isl, crs = epsg9122)
        p <- ggplot() + 
          geom_tile(data = tb, aes(x = x, y = y, fill = value >= thld)) + 
          geom_sf(data = isl) +
          scale_fill_manual(
            values = c("lightblue", "darkgreen"),
            labels = c("Absence", "Présence")
          ) + 
          labs(
            x = "Longitude", y = "Latitude" 
          ) + 
          guides(fill = guide_legend(paste0("Seuil = ", thld)))
        
        file_name <- gsub(" ", ".", bn) %>% 
          paste("ensemble", "basic", sep = ".") %>% 
          paste(nisl, EMalg, "incidence", "map", sep = "_") %>%
          paste0(".png")
        
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
        return(p)
      },
      tbs, 
      names(maps), 
      USE.NAMES = T, 
      SIMPLIFY = F
    )
    
    P <- Reduce(`+`, ps)
    
    file_name <- gsub(" ", ".", bn) %>% 
      paste("ensemble", "basic", sep = ".") %>% 
      paste("ANT", EMalg, "incidence", "map", sep = "_") %>%
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
    
    return(P)
  },
  names(spec_pjs_tb)[-1],
  thlds[-1],
  SIMPLIFY = F, 
  USE.NAMES = T
)