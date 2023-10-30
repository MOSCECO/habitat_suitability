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

# Choix de la projection
lapply(
  c("current", "forecast_ssp126", "forecast_ssp585"),
  \(my_proj) {

    # Compilation
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
        algs     <- c(
          # "rf5",
          # "maxent5",
          "ens5",
          NULL
        )
        patterns <- paste(binnam, algs, "0", sep = ".")

        mapply(
          \(my_pattern, my_name) {

            # my_pattern <- patterns[[1]]
            # my_name <- algs[[1]]
            # my_pattern <- patterns[[2]]
            # my_name <- algs[[2]]

            path_models <- here("data", "analysis", "models_mesu", supfam, bn)
            # importation des résultats de modèles ----
            proj_scales <- lapply(
              list.files(path_models, pattern = my_pattern),
              \(f) {
                # f <- "Cla.nod.rf5.01.global.cpc"
                # f <- "Cla.nod.rf5.02.local.sxt"
                # f <- "Cla.nod.rf5.03.local.hab.2"

                list.files(
                  here(path_models, f, paste("proj", my_proj, sep = "_")),
                  pattern = "\\.tif",
                  full.names = T
                ) %>%
                  rast()
              }
            )
            names(proj_scales) <- c("copernicus", "sextant", "habitat")

            proj_compil <- sapply(
              c("ca", "wmean"),
              \(ens_alg) {

                print(ens_alg)

                proj_scales_wmean <- sdmRasterCompilation(
                  sr      = proj_scales,
                  ens_alg = ens_alg,
                  # Seule métrique d'évaluation disponible pour
                  # habitat : ça avait été l'objet d'un bug sur le cluster
                  met_evl = "TSS",
                  # Habitat > Sextant (local) > Copernicus (global)
                  vec_weight = c(1, 2, 3)
                )

                proj_scales_depth <- sapply(
                  c("KAPPA", "ROC", "TSS"),
                  \(met_evl) {

                    # ens_alg <- "ca"
                    # met_evl <- "KAPPA"
                    print(met_evl)

                    # importation du modèle d'ensemble utilisé pour obtenir ses évaluations
                    mods <- lapply(
                      list.files(path_models, pattern = my_pattern),
                      \(f) {
                        print(f)

                        setwd(here(path_models, f))
                        p <- list.files(pattern = "ensemble\\.models\\.out")
                        obj <- load(p)
                        return(get(obj))
                      }
                    )
                    names(mods) <- c("copernicus", "sextant", "habitat")

                    mod_evals <- lapply(mods, get_evaluations)
                    cutoffs_met_evl <- lapply(
                      mod_evals,
                      \(tb) {
                        tb %>%
                          filter(
                            metric.eval == met_evl, algo == paste0("EM",ens_alg)
                          ) %>%
                          summarise(cutoff = ceiling(mean(cutoff)))
                        # summarise(cutoff = max(cutoff))
                      }) %>% unlist()

                    # Transformation en présence/absence selon le cutoff
                    # et suppression des occurences hors limites de profondeurs
                    cutoff <- ceiling(mean(cutoffs_met_evl))
                    proj_scales_cut <- ifel(proj_scales_wmean > cutoff, 1, 0)
                    proj_scales_dpt <- proj_scales_cut*dmask
                    names(proj_scales_dpt) <- paste0("CT", tolower(met_evl), cutoff)

                    return(proj_scales_dpt)
                  },
                  simplify = FALSE, USE.NAMES = T)
                return(
                  list(
                    po = proj_scales_wmean,
                    pa = proj_scales_depth
                  )
                )
              },
              simplify = FALSE, USE.NAMES = T)

            # sauvegarde probabilité d'adéquation de l'habitat
            path_cpo <- here(path_compilation, "adequation_environnementale")
            makeMyDir(path_cpo)
            path_cpo_supfam <- here(path_cpo, supfam)
            makeMyDir(path_cpo_supfam)
            path_cpos_species <- here(path_cpo_supfam, bn)
            makeMyDir(path_cpos_species)

            lapply(
              names(proj_compil),
              \(ens_alg) {

                path_ens_alg <- here(path_cpos_species, ens_alg)
                makeMyDir(path_ens_alg)

                writeRaster(
                  proj_compil[[ens_alg]]$po,
                  here(
                    path_ens_alg,
                    paste(
                      "adequation-environnementale",
                      my_name,
                      my_proj,
                      paste0("EM", ens_alg), # Ensemble modelling algorithm
                      paste0("CP", "wmean"),   # Compilation algorithm
                      # Weights used in the compilation algorithm
                      "w" %>% paste0(paste(vec_weight, collapse = "-")),
                      sep = "_"
                    ) %>%
                      paste0(".tif")
                  ),
                  overwrite = T
                )
              })

            # sauvegarde présence/absence
            path_cpa <- here(path_compilation, "presence_absence")
            makeMyDir(path_cpa)
            path_cpa_supfam <- here(path_cpa, supfam)
            makeMyDir(path_cpa_supfam)
            path_cpas_species <- here(path_cpa_supfam, bn)
            makeMyDir(path_cpas_species)

            lapply(
              names(proj_compil),
              \(ens_alg) {

                path_ens_alg <- here(path_cpas_species, ens_alg)
                makeMyDir(path_ens_alg)

                lapply(
                  names(proj_compil[[ens_alg]]$pa),
                  \(met_evl) {

                    r <- proj_compil[[ens_alg]]$pa[[met_evl]]

                    path_met_evl <- here(path_ens_alg, met_evl)
                    makeMyDir(path_met_evl)

                    writeRaster(
                      r,
                      here(
                        path_met_evl,
                        paste(
                          "presence-absence",
                          my_name,
                          my_proj,
                          paste0("EM", ens_alg), # Ensemble Modelling algorithm
                          paste0("CP", "wmean"),   # ComPilation algorithm
                          # Weights used in the compilation algorithm
                          "w" %>% paste0(paste(vec_weight, collapse = "-")),
                          # CutOff methods and value
                          names(r),
                          sep = "_"
                        ) %>%
                          paste0(".tif")
                      ),
                      overwrite = T
                    )
                  }
                )
              })
          },
          patterns,
          algs,
          SIMPLIFY = F,
          USE.NAMES = T
        )
      })


  }
)
