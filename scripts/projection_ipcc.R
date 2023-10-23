# Projection avec valeurs de l'IPCC ----

clim_sub      <- cgc_sub[[supfam]][[bn]]
clim_proj_sub <- subset(climosaic_ssp126, names(clim_sub))

lapply(
  # superFamilies,
  "Majoidea",
  \(sufpam) {

    lapply(
      # species$species[species$superFamily == supfam],
      "Stenorhynchus seticornis",
      \(bn) {

        # supfam <- "Majoidea"
        # bn     <- "Stenorhynchus seticornis"
        path_models_out <- here("data", "analysis", "models", supfam, bn)

        lapply(
          list.files(path_models_out, full.names = T),
          \(pth) {

            # pth <- list.files(path_models_out, full.names = T)[[1]]
            pth_out <- list.files(
              pth, pattern = "ensemble.models.out", full.names = T
            )
            nam_out <- list.files(pth, pattern = "ensemble.models.out")

            load(pth_out)
            mod_ensemble <- get(nam_out)
            # mod_ensemble@dir.name        <- path_models_out
            # mod_ensemble@link            <- pth_out
            # mod_ensemble@models.out@link <- list.files(
            # pth, pattern = "cpc.models.out", full.names = T
            # )

            # Projection dans un scénario ----
            proj_ipcc <- BIOMOD_EnsembleForecasting(
              bm.em         = mod_ensemble,
              new.env       = clim_proj_sub,
              proj.name     = "forecast",
              models.chosen = "all"
            )

            proj_ipcc_df <- cbind(
              crds(clim_proj_sub), data.frame(get_predictions(proj_ipcc))
            )

            saveRDS(
              prij_ipcc_df,
              here(pth, "proj_forecast", "proj_ipcc_df.rds")
            )

          })
      })
  })
