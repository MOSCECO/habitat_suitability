# Détermination du nombre optimal de pseudo-absences à partir du jeu de données
# le plus riche en espèces

# occurrences d'espèces
taxon <- "muri"
path_m_taxon <- here(path_models, taxon)
makeMyDir(path_m_taxon)

sp <- species[[taxon]]
aphia <- names(sort(table(sp$aphiaID), decreasing = T)[1])
spe_name <- sp %>% 
  filter(aphiaID == aphia) %>% select(scientificName) %>% unique()
# Claremontiella nodulosa la plus abondante en Guadeloupe et Martinique
path_mt_species <- here(path_m_taxon, gsub(" ", "_", spe_name))
makeMyDir(path_mt_species)
spe <- sp %>% filter(aphiaID == aphia)

spe_pts <- spe %>% select(decimalLongitude, decimalLatitude)
spe_cell <- cellFromXY(
  subset(climosaic_rast, 1),
  spe_pts
)

# Paramètres de sélection des pseudo-absences ----
# pa_sets <- c(
#   seq(10, 100, 10),
#   seq(150, 500, 50),
#   seq(1000, 2500, 500),
#   seq(3000, 5000, 1000),
#   # intégralité des cellules des rasters utilisés : 
#   nrow(climosaic_rast) * ncol(climosaic_rast)
# )
pa_strats <- c("random", "disk")
pa_sets   <- c(100, 500, 1000)
models_set <- c("MAXENT", "RF")

# formatage des données ---
pa_biomod_format <- lapply(
  pa_strats,
  function(pa_strat) {
    
    path_mts_strat <- here(path_mt_species, pa_strat)
    makeMyDir(path_mts_strat)
    
    format_set <- lapply(
      pa_sets, 
      function(pa_set) {
        
        path_mtss_pa <- here(path_mts_strat, pa_set)
        makeMyDir(path_mtss_pa)
        
        resp_name <- paste(
          gsub(" ", "_", tolower(spe_name)), 
          "default", 
          pa_strat, 
          pa_set, 
          sep = "_"
        ) 
        
        out <- BIOMOD_FormatingData( 
          resp.var       = rep(1, nrow(spe)), 
          expl.var       = clim_sub, 
          resp.xy        = spe[, c("decimalLongitude", "decimalLatitude")], 
          PA.nb.rep      = 2, # 5 chez Broussin 2021
          PA.nb.absences = pa_set, 
          PA.strategy    = pa_strat, 
          PA.dist.min    = 0, 
          PA.dist.max    = 500, 
          dir.name       = path_mtss_pa, 
          resp.name      = resp_name, 
          filter.raster  = TRUE
        )
        
        return(out)
      }
    )
    names(format_set) <- paste0("pa", pa_sets)
    return(format_set)
  }
)
names(pa_biomod_format) <- pa_strats  

# Choix et paramétrisation des modèles ----
my_biomod_options <- BIOMOD_ModelingOptions(
  MAXENT = list(
    threshold = FALSE, 
    path_to_maxent.jar = here("scripts", "maxent")
  ),
  RF = list(ntree=1000)
)

# Modèles ----
pa_biomod_models_times <- lapply(
  pa_strats, 
  function(pa_strat) {
    models_paset <- lapply(
      paste0("pa", pa_sets),
      function(pa_set) {
        
        pbf <- pa_biomod_format[[pa_strat]][[pa_set]]
        
        models_output <- lapply(
          models_set, 
          function(this_model) {
            t0 <- Sys.time()
            out_model <- BIOMOD_Modeling(
              bm.format       = pbf,
              modeling.id     = tolower(this_model),
              models          = this_model, 
              bm.options      = my_biomod_options,
              nb.rep          = 2,  # 5 chez Brousin 2021
              data.split.perc = 80, # littérature 
              var.import      = 3,
              metric.eval     = c("ROC", "TSS", "KAPPA"), # pour l'indice de Boyce
              # BIOMOD_PresenceOnly()
              do.full.models  = F
            )
            t1 <- Sys.time()
            deltat <- difftime(t0, t1, units = "secs")
            return(list(model = out_model, time = deltat))
          }
        )
        names(models_output) <- models_set
        return(models_output)
      }
    )
    names(models_paset) <- paste0("pa", pa_sets)
    return(models_paset)
  }
)
names(pa_biomod_models_times) <- pa_strats

# séparation des modèles et de leurs temps de calcul ----
pa_biomod_times  <- pa_biomod_models_times %>% 
  lapply(\(m) lapply(m, pluck, "time")) %>% 
  lapply(as.numeric)
pa_biomod_models <- pa_biomod_models_times %>% lapply(pluck, "model")

# sauvegarde des modèles ----
# mapply(
#   \(m, pa_set) {
#     saveRDS(
#       m,
#       here(
#         path_models,
#         paste(
#           "claremontiella", "nodulosa", "maxent", "basic", pa_set, sep = "_"
#         )
#       )
#     )
#   },
#   pa_biomod_models,
#   pa_sets,
#   SIMPLIFY = F
# )

# pa_biomod_models <- lapply(
#   list.files(
#     path_models, pattern = "claremontiella_nodulosa_maxent*", full.names = T
#   ),
#   readRDS
# )

# Error message 2023-02-16
# Warning messages:
#   1: executing %dopar% sequentially: no parallel backend registered 
# 2: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                Iteration limit reached without full convergence - check carefully

# Évaluations des modèles via le AUC(ROC)
pa_models_scores <- lapply(pa_biomod_models, get_evaluations)
pa_models_scores <- mapply(
  \(tb, n, ti) {
    tb %>% add_column(PA_SET = n, time = ti)
  },
  pa_models_scores,
  pa_sets, 
  as.list(pa_biomod_times),
  SIMPLIFY = F, 
  USE.NAMES = T
)
pa_models_scores_tb <- do.call(rbind, pa_models_scores)
pa_models_scores_sm <- pa_models_scores_tb %>%
  group_by(PA_SET, PA) %>% 
  summarise(validation= mean(validation), time = mean(time))

# Visuasation (Valavi et al. 2022)
coeff <- max(pa_sets) * 2.5
ggplot(
  pa_models_scores_sm, aes(x = factor(PA_SET), y = validation, group = PA_SET)
) + 
  geom_bar(aes(y = time/coeff, fill = time), stat = "identity", alpha = 0.5) +
  geom_boxplot() + 
  # scale_fill_gradient(low = "turquoise1", high = "turquoise4")
  scale_fill_gradient(low = "cyan", high = "deepskyblue4")