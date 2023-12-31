# Visualisation des modèles compilées (copernicus, sextant, gebco)
# avec densités des probabilités d'occurrences et
# nombre d'occurrences correctement prédites

sauvegarde <- T

pout <- sapply(
  species$species,
  \(bn) {

    supfam <- species$superFamily[species$species == bn]
    binnam <- bn %>%
      str_split(" ") %>%
      lapply(substr, 1, 3) %>%
      unlist() %>%
      paste(collapse = ".")
    sf <- supfam %>% substr(1, 4) %>% tolower()

    # importation des modèles compilées ----
    fs <- list.files(
      here(
        "data", "analysis", "compilation",
        "presence_absence",
        supfam, bn
      ),
      full.names = T
    )
    ns <- list.files(
      here(
        "data", "analysis", "compilation",
        "presence_absence",
        supfam, bn
      )
    )
    x1 <- ns %>% str_split("_") %>% lapply(pluck, 2) %>% unlist()
    x2 <- ns %>% str_split("_") %>% lapply(pluck, 3) %>% unlist()
    mods <- lapply(fs, rast)
    names(mods) <- paste(x1, x2, sep = "_")

    # constitution en liste organisée en ----
    # algorithme de compilation > algorithme de modélisation
    mods <- list(
      wmean = Reduce(c, mods[names(mods)[grepl("wmean", names(mods))]]),
      ca    = Reduce(c, mods[names(mods)[grepl("ca", names(mods))]])
    )
    mods <- lapply(mods, \(m) {
      names(m) <- ns[grepl("wmean", ns)] %>%
        lapply(str_split, "_") %>%
        lapply(pluck, 1, 2) %>%
        unlist(use.names = F) %>%
        gsub("[0-9]", "", .)
      return(m)
    })

    # Importation des occurrences d'espèces Guadeloupe/Martinique ----
    spp_local_sf <- st_as_sf(
      pa[[bn]] %>% as.data.frame(xy = T),
      coords = c("x", "y"),
      remove = F,
      crs = "EPSG:4326"
    )

    # On extrait aux localisation des présences/absences observées les valeurs
    # sur les modèles en présence/absence. On va ensuite les comparer pour
    # identifier lesquelles ont été correctement prédites ou non.
    pa_extract <- lapply(
      mods, terra::extract, spp_local_sf, method = "simple", xy = T
    )

    # Complétion des NaN
    buffer = 150
    pa_extract <- mapply(
      \(m, tb) {

        # m <- mods$wmean
        # tb <- pa_extract$wmean
        tb_extract_sf <- tibble()

        while(length(table(is.na(tb[, 2]))) > 1) {

          print(buffer)

          # sélection des valeurs non disponibles
          tb_na <- tb %>% filter(is.na(rf))

          # projection
          tb_na_sf_pts <- st_as_sf(
            tb_na,
            coords = c("x", "y"),
            remove = F,
            crs = "EPSG:4326"
          )
          tb_na_sfproj <- st_transform(tb_na_sf_pts, crs = utm20n)

          # zone tampon
          tb_na_sf_plg <- st_buffer(tb_na_sfproj, buffer)

          # reprojection
          tb_na_sf_plg <- st_transform(tb_na_sf_plg, crs = "EPSG:4326")

          # nouvelle extraction
          tb_extract <- cbind(
            ID = tb_na_sf_plg$ID,
            terra::extract(
              m, tb_na_sf_plg, fun = \(x) ifelse(1 %in% x, 1, 0), ID = F, xy = T
            )
          )

          # ajout des nouvelles données dans la table initiale
          id0_nona <- tb_extract$ID[!is.nan(tb_extract[, 2])]

          if(length(id0_nona) > 0) {

            id1_nona <- which(!is.nan(tb_extract[, 2]))
            nm_na <- names(tb)[apply(tb, 2, \(x) TRUE %in% is.na(x))]
            tb[id0_nona, nm_na] <- tb_extract %>% select(-ID) %>% na.omit()

            # sauvegarde des polygones utilisées pour extraire les valeurs moyennes
            coords <- tb_na_sf_plg[id1_nona, c("x", "y")]
            tb_extract <- tb_extract %>%
              na.omit() %>%
              cbind(coords, buffer = buffer) %>%
              st_as_sf()

            tb_extract_sf <- tb_extract_sf %>% rbind(tb_extract)

          }

          # incrémentation de la zone tampon
          buffer <- buffer + 25

        }

        return(
          list(pa_val = tb, pa_plg = tb_extract_sf[order(tb_extract_sf$ID), ])
        )
      },
      mods,
      pa_extract,
      SIMPLIFY = F,
      USE.NAMES = T
    )

    pa_val <- pa_extract %>% lapply(pluck, "pa_val")
    pa_val <- pa_val %>% lapply(cbind, obs = spp_local_sf$individualCount)

    # Liste d'occurrences selon leurs coordonnées géographiques
    # Guadeloupe, Martinique et Antilles (Guadeloupe + Martinique)
    pa_val <- lapply(
      pa_val,
      \(tb) {
        # tb <- pa_val$wmean
        tb_sf <- st_as_sf(tb, coords = c("x", "y"), remove = F, crs = "EPSG:4326")

        tb_sf_isl <- lapply(
          islands,
          \(nisl) {
            # nisl <- "GLP"
            bb <- st_bbox(climatologies[[nisl]])
            tb_sf %>% st_crop(bb)
          }
        )

        list(ANT = tb_sf) %>% append(tb_sf_isl)

      }
    )

    # proportions de présences/absences correctement prédites
    prop_pa_occ <- sapply(
      names(pa_val),
      \(alg) {
        # alg <- "wmean"

        sapply(
          names(pa_val[[alg]]),
          \(nisl) {

            # nisl <- "GLP"

            tb <- pa_val[[alg]][[nisl]]

            # Calcul du nombre de prédictions correctes ou non
            # tb0 <- tb %>%
            #   pivot_longer(cols = c("rf", "maxent", "ens")) %>%
            #   st_drop_geometry(.) %>%
            #   filter(value == 0) %>%
            #   group_by(., name) %>%
            #   summarise(., count = n()) %>%
            #   cbind(model_val = "absence")
            #
            # tb1 <- tb %>%
            #   pivot_longer(cols = c("rf", "maxent", "ens")) %>%
            #   st_drop_geometry(.) %>%
            #   filter(value == 1) %>%
            #   group_by(., name) %>%
            #   summarise(., count = n()) %>%
            #   cbind(model_val = "presence")
            #
            # tb2 <- tb1 %>% rbind(tb0)
            # tb2$model_val <- factor(tb2$model_val, ordered = T)

            a <- tb %>%
              st_drop_geometry() %>%
              select(all_of(c("rf", "maxent", "ens")))
            b <- apply(
              a, 2, \(x) x == (tb %>% st_drop_geometry() %>% select(obs))
            )

            tb1 <- tb
            tb1[, c("rf", "maxent", "ens")] <- b
            tb1 <- tb1 %>%
              mutate(obs = ifelse(tb$obs == 1, "presence", "absence"))

            return(tb1)
          },
          simplify = F,
          USE.NAMES = T
        )

      },
      simplify = F,
      USE.NAMES = T
    )

    # présences uniquement pour les couleurs des occurrences observées
    # correctement prédites
    prop_pa_prs <- sapply(
      names(prop_pa_occ),
      \(alg) {
        # alg <- "wmean"
        sapply(
          names(prop_pa_occ[[alg]]),
          \(nisl) {
            # nisl <- "GLP"
            tb <- prop_pa_occ[[alg]][[nisl]]
            # tb %>% filter(obs == "presence")
          },
          simplify = F,
          USE.NAMES = T
        )
      },
      simplify = F,
      USE.NAMES = T
    )


    # Barplot : graphiques des occurrences correctement prédites ou non
    proportions_pa <- sapply(
      names(prop_pa_occ),
      \(alg) {

        # alg <- names(pa_val)[[1]]
        # alg <- "wmean"

        alg_lab <- switch(
          alg, wmean = "Moyenne pondérée", ca = "Moyenne d'ens"
        )

        sapply(
          names(prop_pa_occ[[alg]]),
          \(nisl) {

            # nisl <- names(pa_val[[alg]])[[1]]
            # nisl <- "GLP"

            isl_lab <- switch(
              nisl,
              ANT = "Guadeloupe et Martinique",
              GLP = "Guadeloupe",
              MTQ = "Martinique"
            )

            tb1 <- prop_pa_occ[[alg]][[nisl]]

            tb2 <- tb1 %>%
              pivot_longer(cols = c("rf", "maxent", "ens")) %>%
              st_drop_geometry(.) %>%
              group_by(., name, obs) %>%
              mutate(value = factor(value, levels = c("FALSE", "TRUE"))) %>%
              reframe(., count = as.vector(table(value))) %>%
              cbind(
                predictions = rep(
                  c("Incorrectes", "Correctes"),
                  length(unique(.$name))
                )
              )


            sapply(
              c("rf", "maxent", "ens"),
              \(alg_mod) {

                # alg_mod <- "rf"

                tb3 <- tb2 %>%
                  filter(name == alg_mod) %>%
                  select(-name)

                ggplot(
                  tb3,
                  aes(
                    y     = predictions,
                    x     = count,
                    group = predictions,
                    fill  = obs
                  )
                ) +
                  geom_col(
                    col = "black", width = 0.7, position = "stack", alpha = 0.8
                  ) +
                  scale_fill_manual(
                    values = c("white", "#03a700")
                  ) +
                  geom_text(
                    aes(label = count),
                    position = position_stack(),
                    hjust = 1.5,
                    size = 5
                  ) +
                  xlab("Correspondances observations/prédictions") +
                  ylab("Prédictions") +
                  theme(
                    panel.background = element_blank(),
                    legend.position = "none"
                  )

              },
              simplify = F,
              USE.NAMES = T
            )
          },
          simplify = F,
          USE.NAMES = T
        )

      },
      simplify = F,
      USE.NAMES = T
    )

    # préparation des cartes ----
    # pour chaque combinaison algorithme compilation / île
    p_alg_pa <- sapply(
      names(mods),
      \(alg_compilation) {

        # alg_compilation <- "wmean"

        alg_comp <- switch(
          alg_compilation, wmean = "Moyenne pondérée", ca = "Moyenne d'ens"
        )

        sapply(
          c("rf", "maxent", "ens"),
          \(alg_modelisation) {

            # alg_modelisation <- "rf"

            alg_modl <- switch(
              alg_modelisation,
              ens    = "Ensemble",
              maxent = "Maximum d'entropie (MAXENT)",
              rf     = "Forêt aléatoire (Random Forest)"
            )

            # raster des antilles avec présence/absences
            sr <- mods[[alg_compilation]][[alg_modelisation]]

            # vecteur des prédictions erronées ou non
            pred <- prop_pa_prs[[alg_compilation]][["ANT"]] %>%
              select(all_of(c("x", "y", alg_modelisation, "obs")))

            p_pa <- plotComparaisonOccurrences_pa(
              sr,
              spp_sf   = spp_local_sf,
              pred     = pred,
              title    = alg_comp,
              subtitle = alg_modl
            )

          },
          simplify = F,
          USE.NAMES = T
        )
      },
      simplify = F,
      USE.NAMES = T
    )

    # génération des figures ----
    path_fig_compilation <- here("figures", "compilation")
    makeMyDir(path_fig_compilation)
    path_fig_com_px <- here(path_fig_compilation, "presence_absence")
    makeMyDir(path_fig_com_px)
    path_fc_supfam <- here(path_fig_com_px, supfam)
    makeMyDir(path_fc_supfam)
    path_fcs_spe <- here(path_fc_supfam, bn)
    makeMyDir(path_fcs_spe)

    sapply(
      c("wmean", "ca"),
      \(alg) {
        mapply(
          \(nisl, hisl, wisl, risl) {
            sapply(
              # c("pocc", "nocc", "aocc"),
              "aocc",
              \(xocc) {

                # alg  <- "wmean"
                # nisl <- "ANT"
                # xocc <- "pocc"

                alg_lab <- switch(
                  alg, wmean = "Moyenne pondérée", ca = "Moyenne d'ens"
                )

                file_name <- paste(
                  "compilation", "pa_val", sf, binnam,
                  alg, tolower(nisl), xocc, sep = "_"
                ) %>%
                  paste0(".png")

                bool_title <- if(xocc != "aocc") {
                  TRUE
                } else { FALSE }

                P_hig <- joinedMaps(
                  p_alg_pa[[alg]] %>% lapply(pluck, nisl, xocc),
                  collect_guides = T,
                  keep_title = bool_title,
                  plot_title = alg_lab
                )
                P_low <- joinedMaps(
                  proportions_pa[[alg]][[nisl]], collect_guides = T
                )

                P <- if(xocc != "aocc") {
                  (P_hig / P_low) +
                  plot_layout(heights = c(0.8, 0.2))
                } else {
                  P_hig / patchwork::plot_spacer() +
                    plot_layout(heights = c(0.8, 0.2))
                }

                if (sauvegarde) {
                  ggexport(
                    P,
                    filename = here(path_fcs_spe, file_name),
                    width  = wisl,
                    height = hisl,
                    res    = risl
                  )
                }

                return(
                  list(
                    phig = P_hig,
                    plow = P_low
                  )
                )
              },
              simplify = F,
              USE.NAMES = T
            )
          },
          c("ANT", "GLP", "MTQ"),
          c( 5000,  4000,  2500), # hauteur
          c(20000, 10000,  5000), # largeur
          c(  800,   400,   300), # résolution
          SIMPLIFY = F,
          USE.NAMES = T
        )
      },
      simplify = F,
      USE.NAMES = T
    )
  },
  simplify = F,
  USE.NAMES = T
)
