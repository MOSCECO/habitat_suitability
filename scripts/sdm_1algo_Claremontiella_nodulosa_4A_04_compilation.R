# Compilation des résultats des différents modèles

# Problème avec les données : variations de la chlorophylle a aberrantes.
# Modèle d'habitat à seulement deux paramètres
# (pente et profondeur avec des NA dans la pente)

# EG : problème : si on fait un premier seuil pour le masque
# des habitats en décidant d'un seuil arbitraire, on perd l'idée
# de filtre
# décider d'un seuil identique pour chaque modèle et aggréger
# les modèles ensemble selon l'idée qu'une absence est rédhibitoire
# pour la cellule.

# paramètrage ----
patterns <- c("Cla.nod.rf5.0", "Cla.nod.maxent5.0", "Cla.nod.basic.+ensemble")
algs     <- c("rf5", "maxent5", "ensemble")

mapply(
  \(my_pattern, my_name) {

    # my_pattern <- patterns[[1]]
    # my_name <- algs[[1]]
    # my_pattern <- patterns[[2]]
    # my_name <- algs[[2]]



    # importation des résultats de modèles ----
    proj_currents <- lapply(
      list.files(
        here("data", "analysis", "models"),
        pattern = my_pattern
      ),
      \(f) {

        list.files(
          here("data", "analysis", "models", f, "proj_current"),
          pattern = "\\.tif",
          full.names = T
        ) %>% rast()
      }
    )
    names(proj_currents) <- c("copernicus", "sextant", "habitat")

    # Aggrégation par weighted mean
    proj_current_wmeans <- lapply(
      proj_currents,
      \(r) {
        subset(r, names(r)[grepl("EMwmeanByTSS", names(r))])
      }
    )
    proj_current_cas <- lapply(
      proj_currents,
      \(r) {
        subset(r, names(r)[grepl("EMcaByTSS", names(r))])
      }
    )

    # stacking
    pj_wmean <- Reduce(c, proj_current_wmeans)
    pj_ca    <- Reduce(c, proj_current_cas)

    # calcul de la moyenne pondérée
    vec_weight         <- c(1, 2, 3)
    proj_current_wmean <- terra::weighted.mean(pj_wmean, vec_weight)
    proj_current_ca    <- terra::weighted.mean(pj_ca, vec_weight)

    # importation du modèle d'ensemble utilisé pour obtenir ses évaluations
    mods <- lapply(
      list.files(
        here("data", "analysis", "models"),
        pattern = my_pattern
      ),
      \(f) {
        print(f)

        setwd(here("data", "analysis", "models", f))
        p <- list.files(pattern = "ensemble\\.models\\.out")
        obj <- load(p)
        return(get(obj))
      }
    )
    names(mods) <- c("copernicus", "sextant", "habitat")

    mod_evals <- lapply(mods, get_evaluations)
    thlds_TSS <- lapply(mod_evals, \(tb) {
      tb %>% filter(metric.eval == "TSS") %>% select(algo, cutoff)
    })
    thlds_TSS_mean <- do.call(rbind, thlds_TSS) %>%
      group_by(algo) %>%
      summarise(cutoff_mean = ceiling(mean(cutoff)))

    # seuil
    s_wmean <- thlds_TSS_mean$cutoff_mean[2]
    s_ca    <- thlds_TSS_mean$cutoff_mean[1]
    r_wmean <- ifel(proj_current_wmean > s_wmean, 1, 0)
    r_ca    <- ifel(proj_current_ca > s_ca, 1, 0)

    # Visualisation
    # plot(r_wmean)
    # plot(r_ca)

    # sauvegarde probabilité d'adéquation de l'habitat
    path_compilation <- here("data", "analysis", "compilation")
    makeMyDir(path_compilation)

    writeRaster(
      proj_current_wmean,
      here(
        path_compilation,
        paste(
          "habitat-suitability",
          my_name,
          "wmean",
          "w" %>% paste0(paste(vec_weight, collapse = "-")),
          sep = "_"
        ) %>%
          paste0(".tif")
      ),
      overwrite = T
    )

    writeRaster(
      proj_current_ca,
      here(
        path_compilation,
        paste(
          "habitat-suitability",
          my_name,
          "ca",
          "w" %>% paste0(paste(vec_weight, collapse = "-")),
          sep = "_"
        ) %>%
          paste0(".tif")
      ),
      overwrite = T
    )

    # sauvegarde présence/absence
    path_compilation <- here("data", "analysis", "compilation")
    makeMyDir(path_compilation)

    writeRaster(
      r_wmean,
      here(
        path_compilation,
        paste(
          "presence-absence",
          my_name,
          "wmean",
          "s" %>% paste0(s_wmean),
          "w" %>% paste0(paste(vec_weight, collapse = "-")),
          sep = "_"
        ) %>%
          paste0(".tif")
      ),
      overwrite = T
    )

    writeRaster(
      r_ca,
      here(
        path_compilation,
        paste(
          "presence-absence",
          my_name,
          "ca",
          "s" %>% paste0(s_ca),
          "w" %>% paste0(paste(vec_weight, collapse = "-")),
          sep = "_"
        ) %>%
          paste0(".tif")
      ),
      overwrite = T
    )




  },
  patterns,
  algs,
  SIMPLIFY = F,
  USE.NAMES = T

)
