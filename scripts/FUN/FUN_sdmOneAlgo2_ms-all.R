sdmOneAlgo2msall <- function(
    alg,            # algorithm
    CV_nb_rep,      # run_number
    binnam,         # binomial_name code 3 letters
    bn,             # binomial full
    vec_name_model, # model_name
    bio,            # biological_data
    clim_sub,       # environmental_data
    clim_proj_sub,  # subprojected_environmental_data
    pts_name_model
) {
  # Identifiant du modèle ----
  modeling_id <- gsub(
    " ",
    ".",
    paste(
      binnam,
      paste(vec_name_model, collapse = " ")
    )
  )

  # Niche écologique observée ----
  clm <- bio %>%
    filter(individualCount > 0) %>%
    select(-c(type, id, scale, individualCount)) %>%
    st_drop_geometry()
  neo_data <- clm %>% select(-c(x, y))
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
          aes(x = get(varenv), y = after_stat(density), fill = "Grille environnementale"),
          alpha = myAlpha,
          bins = 30
        ) +
        geom_histogram(
          data = neo_data,
          aes(x = get(varenv), y = after_stat(density), fill = "Occurrences de l'espèce"),
          alpha = myAlpha,
          bins = 30
        ) +
        scale_fill_manual(values = c("green", "red")) +
        xlab(varenv) +
        ylab("Densité") +
        scale_x_continuous(
          limits = c(
            min(
              quantile(neo_data[[varenv]], qtl, na.rm = T),
              quantile(as.data.frame(clim_sub)[[varenv]], qtl, na.rm = T)
            ),
            max(
              quantile(neo_data[[varenv]], 1 - qtl, na.rm = T),
              quantile(as.data.frame(clim_sub)[[varenv]], 1 - qtl, na.rm = T)
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
              quantile(neo_data[[varenv]], qtl, na.rm = T),
              quantile(as.data.frame(clim_sub)[[varenv]], qtl, na.rm = T)
            ),
            max(
              quantile(neo_data[[varenv]], 1 - qtl, na.rm = T),
              quantile(as.data.frame(clim_sub)[[varenv]], 1 - qtl, na.rm = T)
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
              quantile(neo_data[[varenv]], qtl, na.rm = T),
              quantile(as.data.frame(clim_sub)[[varenv]], qtl, na.rm = T)
            ),
            max(
              quantile(neo_data[[varenv]], 1 - qtl, na.rm = T),
              quantile(as.data.frame(clim_sub)[[varenv]], 1 - qtl, na.rm = T)
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
    # Données initiales
    resp.var       = bio$individualCount,
    expl.var       = clim_sub,
    resp.xy        = st_coordinates(bio) %>%
      as_tibble() %>%  select(x = X, y = Y),
    # Modalités de sauvegarde
    dir.name       = path_models,
    resp.name      = modeling_id,
    # Gestion des occurrences multiples dans une cellule
    filter.raster  = TRUE
  )

  # Paramétrage du modèle ----
  biom_options <- BIOMOD_ModelingOptions(
    MAXENT = list(
      path_to_maxent.jar = here("scripts", "maxent", "maxent.jar")
    )
  )
  ensemble <- c(
    "GLM", "GBM", "GAM", "CTA", "ANN", "SRE",
    "FDA", "MARS", "RF", "MAXENT", "MAXNET"
  )
  all_biomod2_algos <- switch(
    alg,
    ENS = ensemble,
    match.arg(alg, ensemble)
  )
  print(
    paste(
      "Les algorithmes utilisés lors de cette modélisation sont :",
      paste(all_biomod2_algos, collapse = ", ")
    )
  )
  # Modélisation des habitats favorables selon une méthode ensembliste ----
  # cl <- startMPIcluster()
  # registerDoMPI(cl)
  spec_models <- BIOMOD_Modeling(
    bm.options      = biom_options,
    bm.format       = spec_data,
    modeling.id     = modeling_id,
    models          = all_biomod2_algos,
    CV.nb.rep       = CV_nb_rep,
    CV.perc         = 0.8,
    var.import      = 3
    # nb.cpu          = cl$workerCount
  )
  # closeCluster()

  # error avec "method(depth)"
  # vérifier la longueur du nom du modèle

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

  # fichier d'accueil
  path_eval <- here(
    "data", "analysis", "models", modeling_id, "eval"
  )
  makeMyDir(path_eval)

  # get model evaluation scores
  modScores <- get_evaluations(spec_models)
  # sauvegarde
  file_name <- gsub(" ", ".", bn) %>%
    paste(pts_name_model, "evaluations", sep = "_") %>%
    paste0(".csv")
  write.csv(
    modScores,
    here(path_eval, file_name),
    row.names = F,
    fileEncoding = "UTF-16"
  )

  modScoresSummary01 <- modScores %>%
    filter(run != "allRun") %>%
    group_by(metric.eval) %>%
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
  # sauvegarde
  file_name <- gsub(" ", ".", bn) %>%
    paste(pts_name_model, "evaluation", "summary", sep = "_") %>%
    paste0(".csv")
  write.csv(
    modScoresSummary01,
    here(path_eval, file_name),
    row.names = F,
    fileEncoding = "UTF-16"
  )
  # attention : moyenne des runs != indices calculés sur tous les runs
  modScoresSummary02 <- modScores %>%
    filter(run == "allRun")
  file_name <- gsub(" ", ".", bn) %>%
    paste(pts_name_model, "evaluation", "allRun", sep = "_") %>%
    paste0(".csv")
  write.csv(
    modScoresSummary02,
    here(path_eval, file_name),
    row.names = F,
    fileEncoding = "UTF-16"
  )

  # Graphique des évaluations ----

  p1 <- bm_PlotEvalMean_gm(
    bm.out      = spec_models,
    metric.eval = c("ROC","TSS"),
    group.by    = "algo",
    dataset = "calibration",
    do.plot = T,
    main = NULL,
    ylim = c(0, 1),
    xlim = c(0, 1)
  )

  saveRDS(p1, here(path_eval, "TSSfROC_algo.rds"))

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
  p2 <- bm_PlotEvalMean_gm(
    bm.out      = spec_models,
    metric.eval = c("ROC","TSS"),
    group.by    = "run",
    dataset = "calibration",
    do.plot = T,
    main = NULL,
    ylim = c(0, 1),
    xlim = c(0, 1)
  )

  saveRDS(p2, here(path_eval, "TSSfROC_runs.rds"))

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

  p3 <- bm_PlotEvalMean_gm(
    bm.out      = spec_models,
    metric.eval = c("KAPPA","TSS"),
    group.by    = "algo",
    dataset = "calibration",
    main = NULL,
    ylim = c(0, 1),
    xlim = c(0, 1)
  )
  saveRDS(p3, here(path_eval, "TSSfKAP_algo.rds"))

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

  p4 <- bm_PlotEvalMean_gm(
    bm.out      = spec_models,
    metric.eval = c("KAPPA","TSS"),
    group.by    = "run",
    dataset = "calibration",
    main = NULL,
    ylim = c(0, 1),
    xlim = c(0, 1)
  )
  saveRDS(p4, here(path_eval, "TSSfKAP_runs.rds"))

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

  p5 <- if (alg != "ENS") {
    ggplot() +
      geom_col(
        data = var_importance,
        aes(
          x = expl.var %>%
            factor(
              levels = expl.var[order(get(alg), decreasing = T)]
            ),
          y = get(alg)
        )
      ) +
      xlab("Variable environnementale") +
      ylab("Contribution (%)")
  } else {
    var_importance_ens <- var_importance %>%
      add_column(median = apply(.[, -1], 1, median))
    var_importance_ens$expl.var <- factor(
      var_importance_ens$expl.var,
      levels = var_importance_ens$expl.var[
        order(var_importance_ens$median, decreasing = T)
      ]
    )
    var_importance_ens <- var_importance_ens %>%
      select(-median) %>%
      melt()
    ggplot(
      data = var_importance_ens,
      aes(
        x = expl.var,
        y = value * 100
      )
    ) +
      geom_boxplot() +
      xlab("Variable environnementale (médiane décroissante)") +
      ylab("Contribution (%)")
  }

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
    bm.mod        = spec_models,
    models.chosen = get_built_models(spec_models),
    em.by         = "all",
    em.algo       = all_ensemble_algos,
    metric.select = "all"
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
    # new.env         = clim_sub, # mondial
    new.env         = clim_proj_sub, # local
    proj.name       = "current",
    metric.binary   = "TSS",
    output.format   = ".img",
    do.stack        = FALSE
  )

  spec_ensemble_models_proj_current <- BIOMOD_EnsembleForecasting(
    bm.em         = spec_ensemble_models,
    # bm.proj       = spec_models_proj_current,
    # new.env         = clim_sub, # mondial
    new.env         = clim_proj_sub, # local
    proj.name     = "current",
    models.chosen = "all"
  )

  # [rast] CRS do not match
  # n'affecte pas la sortie
  # terra::crs(clim_sub, proj = TRUE)
  # terra::crs(get_predictions(spec_ensemble_models_proj_current),
  #            proj = TRUE)

  # Visualisation ----
  plot(spec_ensemble_models_proj_current)
  spec_proj_current_spatRast <- terra::unwrap(
    spec_ensemble_models_proj_current@proj.out@val
  )

  # chemins de sauvegarde
  path_EM <- here("data", "analysis", "models", modeling_id, "transfert")
  makeMyDir(path_EM)
  path_figEM <- here(path_EM, "figures")
  makeMyDir(path_figEM)
  path_layEMpo <- here(path_EM, "layers_po") # probabilité d'occurrence
  makeMyDir(path_layEMpo)
  path_layEMpa <- here(path_EM, "layers_pa") # presence absence
  makeMyDir(path_layEMpa)

  # sauvegarde aux formats .shp et .tif
  lapply(
    names(spec_proj_current_spatRast),
    \(nm) {
      # nm <- names(spec_proj_current_spatRast)[2]
      sr <- subset(spec_proj_current_spatRast, nm)
      writeRaster(sr, here(path_layEMpo, nm %>% paste0(".tif")))
      # sf <- st_as_stars(sr) %>% st_as_sf()
      # st_write(sf, here(path_layEMpo, nm %>% paste0(".shp")))
    })

  # sauvegarde aux formats .shp et .tif des présence/absences
  mapply(
    \(nm, thld) {
      # nm <- names(spec_proj_current_spatRast)[2]
      sr <- subset(spec_proj_current_spatRast, nm)
      sr_pa <- ifel(sr >= thld, 1, 0)
      writeRaster(
        sr_pa,
        here(path_layEMpa, nm %>% paste0(".tif")),
        overwrite = T
      )
      # sf <- st_as_stars(sr_pa) %>% st_as_sf()
      # st_write(sf, here(path_layEMpa, nm %>% paste0(".shp")))
    },
    names(spec_proj_current_spatRast)[-1],
    thlds[-1],
    SIMPLIFY = F
  )

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
          occ <- bio %>%
            filter(individualCount > 0) %>%
            st_crop(as.vector(e)[c(1,3,2,4)])

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
}
