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

lapply(
  species$species,
  \(bn) {

    # bn <- "Stenorhynchus seticornis"

    binnam <- bn %>%
      str_split(" ") %>%
      lapply(substr, 1, 3) %>%
      unlist() %>%
      paste(collapse = ".")
    supfam <- species$superFamily[species$species == bn]

    # chemin de sauvegarde général ----
    path_compilation <- here("data", "analysis", "compilation")
    makeMyDir(path_compilation)

    # Filtre de profondeur par connaissance expert...
    # ... dont les proxy -sont- étaient les 1er et 3ème quartiles des
    # distribution de profondeurs des espèces.
    d   <- hab_sub$depth
    bnd <- list.files(
      here("data", "analysis", "depth_boundaries"),
      pattern = gsub(" ", "_", bn),
      full.names = T
    ) %>% readRDS()
    # arrondissons aux dizaines inférieure et supérieure
    # bnd[[1]] <- floor(bnd[[1]]/10)*10
    # bnd[[2]] <- ceiling(bnd[[2]]/10)*10
    dmask <- ifel(d >= bnd[[1]] & d <= bnd[[2]], 1, 0)
    # x11(); plot(dmask)

    # paramètrage ----
    algs     <- c("rf5", "maxent5", "ens5")
    patterns <- paste(binnam, algs, "0", sep = ".")

    mapply(
      \(my_pattern, my_name) {

        # my_pattern <- patterns[[1]]
        # my_name <- algs[[1]]
        # my_pattern <- patterns[[2]]
        # my_name <- algs[[2]]



        # importation des résultats de modèles ----
        proj_currents <- lapply(
          list.files(
            here("data", "analysis", "models_mesu"),
            pattern = my_pattern
          ),
          \(f) {
            # f <- "Cla.nod.rf5.01.global.cpc"
            # f <- "Cla.nod.rf5.02.local.sxt"
            # f <- "Cla.nod.rf5.03.local.hab.2"

            list.files(
              here("data", "analysis", "models_mesu", f, "proj_current"),
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
            here("data", "analysis", "models_mesu"),
            pattern = my_pattern
          ),
          \(f) {
            print(f)

            setwd(here("data", "analysis", "models_mesu", f))
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

        # seuil ####
        s_wmean <- thlds_TSS_mean$cutoff_mean[2]
        s_ca    <- thlds_TSS_mean$cutoff_mean[1]
        r_wmean <- ifel(proj_current_wmean > s_wmean, 1, 0)
        r_ca    <- ifel(proj_current_ca > s_ca, 1, 0)

        # Limites de profondeurs ####
        r_wmean_f <- r_wmean*dmask
        names(r_wmean_f) <- names(r_wmean)
        r_ca_f <- r_ca*dmask
        names(r_ca_f) <- names(r_ca)

        # Visualisation
        # plot(r_wmean)
        # plot(r_ca)

        # sauvegarde probabilité d'adéquation de l'habitat
        path_cpo <- here(path_compilation, "probabilite_occurrence")
        makeMyDir(path_cpo)
        path_cpo_supfam <- here(path_cpo, supfam)
        makeMyDir(path_cpo_supfam)
        path_cpos_species <- here(path_cpo_supfam, bn)
        makeMyDir(path_cpos_species)

        writeRaster(
          proj_current_wmean,
          here(
            path_cpos_species,
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
            path_cpos_species,
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
        path_cpa <- here(path_compilation, "presence_absence")
        makeMyDir(path_cpa)
        path_cpa_supfam <- here(path_cpa, supfam)
        makeMyDir(path_cpa_supfam)
        path_cpas_species <- here(path_cpa_supfam, bn)
        makeMyDir(path_cpas_species)

        writeRaster(
          # r_wmean,
          r_wmean_f,
          here(
            path_cpas_species,
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
          # r_ca,
          r_ca_f,
          here(
            path_cpas_species,
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
  }
)
