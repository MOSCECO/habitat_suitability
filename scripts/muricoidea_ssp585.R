# Projection avec valeurs de l'IPCC ----

source(here::here("scripts", "boot.R"))

# Chemin vers MAXENT
biom_options <- BIOMOD_ModelingOptions(
  MAXENT = list(
    path_to_maxent.jar = here("scripts", "maxent", "maxent.jar")
  )
)

# Chargement rasters scénarios
ssp <- "ssp585"
climosaic_ssp <- here(
  "data", "tidy", "climatologies_ipcc", paste0(ssp, ".tif")
) %>% rast()

lapply(
  # superFamilies,
  "Muricoidea",
  \(supfam) {

    lapply(
      species$species[species$superFamily == supfam],
      # "Stenorhynchus seticornis",
      \(bn) {

        # supfam <- "Majoidea"
        # bn     <- "Stenorhynchus seticornis"

        # occurrences locales
        bio <- readRDS(here("data", "tidy", "bio", supfam, bn, "bio_local.rds"))

        # Chemin des modèles générés pour le climat contemporain
        path_models_out <- here("data", "analysis", "models", supfam, bn)

        clim_list <- list(
          "cpc" = cgc_sub[[supfam]][[bn]],
          "sxt" = sxt_sub,
          "hab" = hab_sub
        )

        lapply(
          list.files(path_models_out, full.names = T),
          \(pth) {

            # pth <- list.files(path_models_out, full.names = T)[[3]]
            # Identifiants du modèle
            modeling_id <- str_split(pth, "/")[[1]]
            modeling_id <- modeling_id[[length(modeling_id)]]

            # échelle
            m <- str_split(modeling_id, "\\.")[[1]]
            m <- m[which(grepl("[a-z]", m))]
            clim_name <- m[[length(m)]]

            # Chargement rasters de projection
            clim_sub      <- clim_list[[clim_name]]
            clim_proj_sub <- subset(climosaic_ssp, names(clim_sub))

            # chemin et nom du modèle d'ensemble
            pth_out <- list.files(
              pth, pattern = "ensemble.models.out", full.names = T
            )
            nam_out <- list.files(pth, pattern = "ensemble.models.out")

            # chargement du modèle d'ensemble
            load(pth_out)
            mod_ensemble <- get(nam_out)

            # Get thresholds
            all_ensemble_algos <- c("EMcv", "EMca","EMwmean")
            names(all_ensemble_algos) <- all_ensemble_algos

            (spec_ensemble_models_scores <- get_evaluations(mod_ensemble))
            ensemble_scores_names <- c(
              "metric.eval", "cutoff", "sensitivity", "specificity", "calibration"
            )

            # ensemble scores ----
            EMscores <- all_ensemble_algos %>% lapply(
              \(a) {
                # a <- all_ensemble_algos[[2]]
                spec_ensemble_models_scores %>%
                  filter(algo == a) %>%
                  select(all_of(ensemble_scores_names)) %>%
                  group_by(metric.eval) %>%
                  summarise(cutoff = max(cutoff))
              }
            )

            # suppresion d'un éventuel dossier déjà existant
            fn <- here(
              pth,
              paste("proj", paste("forecast", ssp, sep = "_"), sep = "_")
            )
            if (file.exists(fn)) unlink(fn, recursive = T)

            # Projection dans un scénario ----
            proj_ipcc <- BIOMOD_EnsembleForecasting(
              bm.em         = mod_ensemble,
              new.env       = clim_proj_sub,
              proj.name     = paste("forecast", ssp, sep = "_"),
              models.chosen = "all"
            )

            saveRDS(
              proj_ipcc,
              here(
                pth,
                paste("proj", paste("forecast", ssp, sep = "_"), sep = "_"),
                "proj_ipcc.rds"
              )
            )

            # proj_ipcc <- readRDS(
            #   here(
            #     "data", "analysis", "models", "Majoidea",
            #     "Stenorhynchus seticornis", "Ste.set.ens5.01.global.cpc",
            #     "proj_forecast_ssp126", "proj_ipcc.rds"
            #   )
            # )

            proj_ipcc_df <- cbind(
              crds(clim_proj_sub), data.frame(get_predictions(proj_ipcc))
            )

            saveRDS(
              proj_ipcc_df,
              here(
                pth,
                paste("proj", paste("forecast", ssp, sep = "_"), sep = "_"),
                "proj_ipcc_df.rds"
              )
            )

            # Visualisation ----

            # plot(spec_ensemble_models_proj_current)
            spc_rast <- terra::unwrap(
              proj_ipcc@proj.out@val
            )
            names(spc_rast) <- str_split(
              names(spc_rast), "_"
            ) %>%
              lapply(pluck, 2) %>%
              unlist()

            spc_list <- list(
              ca = terra::subset(
                spc_rast, names(spc_rast)[
                  which(grepl("ca", names(spc_rast)))
                ]
              ),
              cv = terra::subset(
                spc_rast, names(spc_rast)[
                  which(grepl("cv", names(spc_rast)))
                ]
              ),
              wmean = terra::subset(
                spc_rast, names(spc_rast)[
                  which(grepl("wmean", names(spc_rast)))
                ]
              )
            )
            spc_list <- lapply(
              spc_list,
              \(x) {
                names(x) <- str_split(names(x), "By") %>%
                  lapply(pluck, 2) %>%
                  unlist()
                return(x)
              }
            )

            # chemins de sauvegarde
            path_transfert <- here(pth, "transfert")
            makeMyDir(path_transfert)
            path_cast <- here(
              path_transfert, paste("forecast", ssp, sep = "_")
            )
            makeMyDir(path_cast)
            path_figEM <- here(path_cast, "figures")
            makeMyDir(path_figEM)
            path_layEMpo <- here(path_cast, "layers_po") # probabilité d'occurrence
            makeMyDir(path_layEMpo)
            path_layEMpa <- here(path_cast, "layers_pa") # presence absence
            makeMyDir(path_layEMpa)

            # sauvegarde aux formats .shp et .tif
            lapply(
              c("ca", "wmean"),                                 # Ensemble algorithm
              \(ens_alg) {
                # ens_alg <- names(spc_list)[1]
                path_ens_alg <- here(path_layEMpo, ens_alg)
                makeMyDir(path_ens_alg)

                lapply(
                  names(spc_list[[ens_alg]]),
                  \(evl_met) {                                  # Evaluation metric

                    # evl_met <- names(spc_list[[ens_alg]])[1]
                    path_evl_met <- here(path_ens_alg, evl_met)
                    makeMyDir(path_evl_met)

                    # Sauvegarde en .tif
                    sr <- spc_list[[ens_alg]][[evl_met]]
                    writeRaster(
                      sr,
                      here(
                        path_evl_met,
                        paste(
                          modeling_id,
                          paste0("EM", ens_alg),
                          paste0("by", evl_met),
                          sep = "_"
                        ) %>% paste0(".tif")
                      ),
                      overwrite = T
                    )

                    # Sauvegarde en .shp (trop chronophage)
                    # sf <- st_as_stars(sr) %>% st_as_sf()
                    # st_write(sf, here(path_layEMpo, nm %>% paste0(".shp")))
                  })
              })

            # sauvegarde aux formats .tif des présence/absences
            lapply(
              c("ca", "wmean"),                                 # Ensemble algorithm
              \(ens_alg) {
                # ens_alg <- names(spc_list)[1]
                path_ens_alg <- here(path_layEMpa, ens_alg)
                makeMyDir(path_ens_alg)

                lapply(
                  names(spc_list[[ens_alg]]),
                  \(evl_met) {                                  # Evaluation metric

                    # evl_met <- names(spc_list[[ens_alg]])[1]
                    path_evl_met <- here(path_ens_alg, evl_met)
                    makeMyDir(path_evl_met)

                    # Spatial raster des présences/absences
                    sr    <- spc_list[[ens_alg]][[evl_met]]
                    tb    <- EMscores[[paste0("EM", ens_alg)]]
                    thld  <- tb$cutoff[tb$metric.eval == evl_met]
                    sr_pa <- ifel(sr >= thld, 1, 0)

                    # Sauvegarde en .tif
                    writeRaster(
                      sr_pa,
                      here(
                        path_evl_met,
                        paste(
                          modeling_id,
                          paste0("EM", ens_alg),
                          paste0("by", evl_met),
                          sep = "_"
                        ) %>% paste0(".tif")
                      ),
                      overwrite = T
                    )

                    # Sauvegarde en .shp (trop chronophage)
                    # sf <- st_as_stars(sr_pa) %>% st_as_sf()
                    # st_write(
                    #   sf,
                    #   here(
                    #     path_evl_met,
                    #     paste(
                    #       modeling_id,
                    #       paste0("EM", ens_alg),
                    #       paste0("by", evl_met),
                    #       sep = "_"
                    #     ) %>% paste0(".tif")
                    #   )
                    # )
                  })
              })

            # Graphiques ----
            proj_hs_plots <- mapply(
              \(ens_alg, col_optn, label_colorbar) {
                # ens_alg <- names(spc_list)[1]
                # col_optn <- "E"
                path_ens_alg <- here(path_figEM, ens_alg)
                makeMyDir(path_ens_alg)

                lapply(
                  names(spc_list[[ens_alg]]),
                  \(evl_met) {                                  # Evaluation metric

                    # evl_met <- names(spc_list[[ens_alg]])[1]
                    path_evl_met <- here(path_ens_alg, evl_met)
                    makeMyDir(path_evl_met)

                    # Spatial raster des présences/absences
                    sr    <- spc_list[[ens_alg]][[evl_met]]

                    ps <- lapply(
                      islands,
                      \(nisl) {
                        # nisl <- "GLP"
                        path_isl <- here(path_evl_met, nisl)
                        makeMyDir(path_isl)

                        # chargement des éléments du graphe
                        isl          <- maps[[nisl]]
                        e            <- ext(climatologies[[nisl]])
                        sr_crop      <- terra::crop(sr, e)
                        tb           <- as.data.frame(sr_crop, xy = T)
                        names(tb)[3] <- "value"
                        occ <- bio %>%
                          filter(individualCount > 0) %>%
                          st_crop(as.vector(e)[c(1,3,2,4)])
                        # occ <- spp_sf %>% st_crop(as.vector(e)[c(1,3,2,4)])

                        # figures ggplot2
                        p <- ggplot() +
                          geom_tile(data = tb, aes(x = x, y = y, fill = value)) +
                          geom_sf(data = isl) +
                          scale_fill_viridis_c(option = col_optn) +
                          labs(x = "Longitude", y = "Latitude") +
                          guides(fill = guide_colorbar(label_colorbar))
                        pocc <- p +
                          geom_sf(data = occ, col = "red", shape = "+", size = 5)

                        # nom des fichiers de sauvegarde
                        file_name <- modeling_id %>%
                          paste(
                            "hs", "map",
                            tolower(nisl), ens_alg, tolower(evl_met),
                            sep = "_"
                          ) %>% paste0(".png")
                        file_name_occ <- modeling_id %>%
                          paste(
                            "hs", "occ", "map",
                            tolower(nisl), ens_alg, tolower(evl_met),
                            sep = "_"
                          ) %>% paste0(".png")

                        # sauvegarde
                        ggexport(
                          plot = p,
                          filename = here(path_isl, file_name),
                          width = 1000,
                          height = 800,
                          res = 100,
                          units = "px",
                          device = "png",
                          limitsize = F
                        )
                        ggexport(
                          plot = pocc,
                          filename = here(path_isl, file_name_occ),
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

                    # noms des fichiers de sauvegarde antilles
                    file_name <- modeling_id %>%
                      paste(
                        "hs", "map", "ant",
                        ens_alg, tolower(evl_met),
                        sep = "_"
                      ) %>% paste0(".png")
                    file_name_occ <- modeling_id %>%
                      paste(
                        "hs", "occ", "map", "ant",
                        ens_alg, tolower(evl_met),
                        sep = "_"
                      ) %>% paste0(".png")

                    # dossier antilles
                    path_isl <- here(path_evl_met, "ANT")
                    makeMyDir(path_isl)

                    ggexport(
                      plot = P,
                      filename = here(path_isl, file_name),
                      width = 4200,
                      height = 2000,
                      res = 200,
                      units = "px",
                      device = "png",
                      limitsize = F
                    )
                    ggexport(
                      plot = Pocc,
                      filename = here(path_isl, file_name_occ),
                      width = 4200,
                      height = 2000,
                      res = 200,
                      units = "px",
                      device = "png",
                      limitsize = F
                    )
                  })
              },
              c("ca", "cv", "wmean"),                                 # Ensemble algorithm
              c("C", "E", "D"),
              c(
                "Adéquation\nenvironnementale",
                "Variabilité",
                "Adéquation\nenvironnementale"
              ),
              SIMPLIFY  = F,
              USE.NAMES = T
            )

            # Présences au-dessus d'un seuil déterminé ----
            proj_pa_plots <- lapply(
              c("ca", "wmean"),                                 # Ensemble algorithm
              \(ens_alg) {
                # ens_alg <- names(spc_list)[1]
                # col_optn <- "E"
                path_ens_alg <- here(path_figEM, ens_alg)
                makeMyDir(path_ens_alg)

                lapply(
                  names(spc_list[[ens_alg]]),
                  \(evl_met) {                                  # Evaluation metric

                    # evl_met <- names(spc_list[[ens_alg]])[1]
                    path_evl_met <- here(path_ens_alg, evl_met)
                    makeMyDir(path_evl_met)

                    # Spatial raster des présences/absences
                    sr   <- spc_list[[ens_alg]][[evl_met]]
                    tb    <- EMscores[[paste0("EM", ens_alg)]]
                    thld  <- tb$cutoff[tb$metric.eval == evl_met]

                    ps <- lapply(
                      islands,
                      \(nisl) {
                        # nisl <- "GLP"
                        path_isl <- here(path_evl_met, nisl)
                        makeMyDir(path_isl)

                        # chargement des éléments du graphe
                        isl          <- maps[[nisl]]
                        e            <- ext(climatologies[[nisl]])
                        sr_crop      <- terra::crop(sr, e)
                        tb           <- as.data.frame(sr_crop, xy = T)
                        names(tb)[3] <- "value"
                        occ <- bio %>%
                          filter(individualCount > 0) %>%
                          st_crop(as.vector(e)[c(1,3,2,4)])
                        # occ <- spp_sf %>% st_crop(as.vector(e)[c(1,3,2,4)])

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

                        # nom des fichiers de sauvegarde
                        file_name <- modeling_id %>%
                          paste(
                            "pa", "map",
                            tolower(nisl), ens_alg, tolower(evl_met),
                            sep = "_"
                          ) %>% paste0(".png")
                        file_name_occ <- modeling_id %>%
                          paste(
                            "pa", "occ", "map",
                            tolower(nisl), ens_alg, tolower(evl_met),
                            sep = "_"
                          ) %>% paste0(".png")

                        # sauvegarde
                        ggexport(
                          plot = p,
                          filename = here(path_isl, file_name),
                          width = 1000,
                          height = 800,
                          res = 100,
                          units = "px",
                          device = "png",
                          limitsize = F
                        )
                        ggexport(
                          plot = pocc,
                          filename = here(path_isl, file_name_occ),
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

                    # noms des fichiers de sauvegarde antilles
                    file_name <- modeling_id %>%
                      paste(
                        "pa", "map", "ant",
                        ens_alg, tolower(evl_met),
                        sep = "_"
                      ) %>% paste0(".png")
                    file_name_occ <- modeling_id %>%
                      paste(
                        "pa", "occ", "map", "ant",
                        ens_alg, tolower(evl_met),
                        sep = "_"
                      ) %>% paste0(".png")

                    # dossier antilles
                    path_isl <- here(path_evl_met, "ANT")
                    makeMyDir(path_isl)

                    ggexport(
                      plot = P,
                      filename = here(path_isl, file_name),
                      width = 4200,
                      height = 2000,
                      res = 200,
                      units = "px",
                      device = "png",
                      limitsize = F
                    )
                    ggexport(
                      plot = Pocc,
                      filename = here(path_isl, file_name_occ),
                      width = 4200,
                      height = 2000,
                      res = 200,
                      units = "px",
                      device = "png",
                      limitsize = F
                    )
                  })
              })


          })
      })
  })
