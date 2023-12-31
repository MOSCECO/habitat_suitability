# Visualisation des modèles compilées (copernicus, sextant, gebco)
# avec densités des probabilités d'occurrences et
# nombre d'occurrences correctement prédites

lapply(
  species$species,
  \(bn) {
    # bn <- "Stenorhynchus seticornis"
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
        "probabilite_occurrence",
        supfam, bn
      ),
      full.names = T
    )
    ns <- list.files(
      here(
        "data", "analysis", "compilation",
        "probabilite_occurrence",
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
    mods <- lapply(
      mods,
      \(m) {
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
    ) %>%
      filter(individualCount > 0)

    # Densité des adéquations de l'habitat associées aux occurrences ----
    hs_extract <- lapply(
      mods, terra::extract, spp_local_sf, method = "bilinear", xy = T
    )
    # Complétion des NaN
    buffer = 150
    hs_extract <- mapply(
      \(m, tb) {

        # m <- mods$wmean
        # tb <- vals_hs$wmean
        tb_extract_sf <- tibble()

        while(length(table(is.nan(tb[, 2]))) > 1) {

          print(buffer)

          # sélection des valeurs non disponibles
          tb_na <- tb %>% filter(is.nan(rf))

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
              m, tb_na_sf_plg, fun = \(x) mean(x, na.rm = T), ID = F, xy = T
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
          list(hs_val = tb, hs_plg = tb_extract_sf[order(tb_extract_sf$ID), ])
        )
      },
      mods,
      hs_extract,
      SIMPLIFY = F,
      USE.NAMES = T
    )

    hs <- hs_extract %>% lapply(pluck, "hs_val")
    hs <- hs %>% lapply(cbind, obs = spp_local_sf$individualCount)

    # Séparation des points selon leurs coordonnées géographiques
    hs <- lapply(
      hs,
      \(tb) {
        # tb <- hs$wmean
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

    # densités d'adéquation environnementale ----
    densities_hs <- sapply(
      names(hs),
      \(alg) {

        # alg <- names(hs)[[1]]

        alg_lab <- switch(
          alg, wmean = "Moyenne pondérée", ca = "Moyenne d'ensemble"
        )

        sapply(
          names(hs[[alg]]),
          \(nisl) {

            # nisl <- names(hs[[alg]])[[2]]

            isl_lab <- switch(
              nisl,
              ANT = "Guadeloupe et Martinique",
              GLP = "Guadeloupe",
              MTQ = "Martinique"
            )

            tb <- hs[[alg]][[nisl]]

            mapply(
              \(alg_mod, alg_col) {

                # alg_mod <- "ensemble"
                # alg_col <- "red"

                tb_long <- tb %>%
                  pivot_longer(cols = c("rf", "maxent", "ens"))
                tb_long <- tb_long %>%
                  filter(name == alg_mod) %>%
                  rbind(tb_long %>% filter(name != alg_mod))
                tb_long$col <- ifelse(
                  tb_long$name == alg_mod, alg_col, "darkgrey"
                )
                tb_long$col <- factor(
                  tb_long$col, levels = unique(tb_long$col)
                )

                ggplot(
                  tb_long,
                  aes(
                    x     = value,
                    group = name,
                    col   = col,
                    fill  = col,
                    alpha = col,
                    after_stat(count)
                  )
                ) +
                  geom_density() +
                  scale_color_manual(values = levels(tb_long$col)) +
                  scale_fill_manual(values = levels(tb_long$col)) +
                  scale_alpha_manual(values = c(0.6, 0.2)) +
                  xlab("Adéquation environnementale") +
                  ylab("Fréquence (présences uniquement)") +
                  theme(
                    legend.position = "none",
                    plot.margin = unit(rep(0.01, 4), "pt")
                  )

              },
              c("rf", "maxent", "ens"),
              c("red", "green", "blue"),
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

    # préparation des cartes ----
    # pour chaque combinaison algorithme compilation / île
    p_alg_hs <- sapply(
      names(mods),
      \(alg_compilation) {

        # alg_compilation <- "wmean"

        alg_comp <- switch(
          alg_compilation, wmean = "Moyenne pondérée", ca = "Moyenne d'ensemble"
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

            sr <- mods[[alg_compilation]][[alg_modelisation]]
            p_hs <- plotComparaisonOccurrences_hs(
              sr,
              spp_sf = spp_local_sf,
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
    path_fig_com_px <- here(path_fig_compilation, "habitat_suitability")
    makeMyDir(path_fig_com_px)
    path_fc_supfam <- here(path_fig_com_px, supfam)
    makeMyDir(path_fc_supfam)
    path_fcs_spe <- here(path_fc_supfam, bn)
    makeMyDir(path_fcs_spe)

    lapply(
      c("wmean", "ca"),
      \(alg) {
        mapply(
          \(nisl, hisl, wisl, risl) {
            lapply(
              c("pocc", "nocc"),
              \(xocc) {

                # alg  <- "wmean"
                # nisl <- "ANT"
                # xocc <- "pocc"

                alg_lab <- switch(
                  alg, wmean = "Moyenne pondérée", ca = "Moyenne d'ensemble"
                )

                file_name <- paste(
                  "compilation", "hs", sf, binnam,
                  alg, tolower(nisl), xocc, sep = "_"
                ) %>%
                  paste0(".png")

                P_hig <- joinedMaps(
                  p_alg_hs[[alg]] %>% lapply(pluck, nisl, xocc),
                  collect_guides = T,
                  keep_title = F,
                  plot_title = alg_lab
                )
                P_low <- joinedMaps(densities_hs[[alg]][[nisl]])

                P <- (P_hig / P_low) +
                  plot_layout(heights = c(0.8, 0.2))

                ggexport(
                  P,
                  filename = here(path_fcs_spe, file_name),
                  width  = wisl,
                  height = hisl,
                  res    = risl
                )

              }
            )
          },
          c("ANT", "GLP", "MTQ"),
          c( 5000,  4000,  2500), # hauteur
          c(20000, 10000,  5000), # largeur
          c(  800,   400,   300), # résolution
          SIMPLIFY = F,
          USE.NAMES = T
        )
      }
    )

  }
)
