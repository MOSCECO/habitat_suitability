# Visualisation des modèles compilées (copernicus, sextant, gebco)
# avec densités des probabilités d'occurrences et
# nombre d'occurrences correctement prédites

# Espèce considérée ----
binomial_name <- "Claremontiella nodulosa"

# importation des modèles compilées ----
fs <- list.files(
  here("data", "analysis", "compilation"),
  full.names = T,
  pattern = "presence-absence"
)
ns <- list.files(
  here("data", "analysis", "compilation"), pattern = "presence-absence"
)
mods <- lapply(fs, rast)
names(mods) <- gsub("\\.tif", "", ns)

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
  sp,
  coords = c("x", "y"),
  remove = F,
  crs = "EPSG:4326"
)

# Valeurs modélisées associées aux occurrences ----
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

pa <- pa_extract %>% lapply(pluck, "pa_val")
pa <- pa %>% lapply(cbind, obs = sp$individualCount)

# Séparation des points selon leurs coordonnées géographiques
pa <- lapply(
  pa,
  \(tb) {
    # tb <- pa$wmean
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

# présences/absences à l'habitat
proportions_pa <- sapply(
  names(pa),
  \(alg) {

    # alg <- names(pa)[[1]]

    alg_lab <- switch(
      alg, wmean = "Moyenne pondérée", ca = "Moyenne d'ensemble"
    )

    sapply(
      names(pa[[alg]]),
      \(nisl) {

        # nisl <- names(pa[[alg]])[[1]]

        isl_lab <- switch(
          nisl,
          ANT = "Guadeloupe et Martinique",
          GLP = "Guadeloupe",
          MTQ = "Martinique"
        )

        tb <- pa[[alg]][[nisl]]


        mapply(
          \(alg_mod) {

            # alg_mod <- "ensemble"

            tb0 <- tb %>%
              pivot_longer(cols = c("rf", "maxent", "ensemble")) %>%
              st_drop_geometry(.) %>%
              filter(value == 0) %>%
              group_by(., name) %>%
              summarise(., count = n()) %>%
              cbind(model_val = "absence")

            tb1 <- tb %>%
              pivot_longer(cols = c("rf", "maxent", "ensemble")) %>%
              st_drop_geometry(.) %>%
              filter(value == 1) %>%
              group_by(., name) %>%
              summarise(., count = n()) %>%
              cbind(model_val = "presence")

            tb2 <- tb1 %>% rbind(tb0)
            tb2$model_val <- factor(tb2$model_val, ordered = T)

            a <- tb %>%
              st_drop_geometry() %>%
              select(all_of(c("rf", "maxent", "ensemble")))
            b <- apply(
              a, 2, \(x) x == tb %>% st_drop_geometry() %>% select(obs)
            )

            tb1 <- tb
            tb1[, c("rf", "maxent", "ensemble")] <- b

            tb2 <- tb1 %>%
              pivot_longer(cols = c("rf", "maxent", "ensemble")) %>%
              st_drop_geometry(.) %>%
              group_by(., name) %>%
              reframe(., count = as.vector(table(value))) %>%
              cbind(
                predictions = rep(
                  c(
                    "Incorrectes",
                    "Correctes"
                  ),
                  length(unique(.$name))
                )
              )
            tb3 <- tb2 %>%
              filter(name == alg_mod) %>%
              select(-name)

            ggplot(
              tb3,
              aes(
                y = predictions,
                x = count,
                group = predictions
              )
            ) +
              geom_col(
                fill = "grey", width = 0.7, position = "dodge2", alpha = 0.8
              ) +
              geom_text(
                aes(label = count),
                position = position_dodge2(width = 0.7),
                hjust = 1.5,
                size = 5
              ) +
              xlab("Correspondances observations/prédictions") +
              ylab("Prédictions") +
              theme(panel.background = element_blank())

          },
          c("rf", "maxent", "ensemble"),
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
p_alg_pa <- sapply(
  names(mods),
  \(alg_compilation) {

    # alg_compilation <- "wmean"

    alg_comp <- switch(
      alg_compilation, wmean = "Moyenne pondérée", ca = "Moyenne d'ensemble"
    )

    sapply(
      c("rf", "maxent", "ensemble"),
      \(alg_modelisation) {

        alg_modl <- switch(
          alg_modelisation,
          ensemble = "Ensemble",
          maxent   = "Maximum d'entropie (MAXENT)",
          rf       = "Forêt aléatoire (Random Forest)"
        )

        sr <- mods[[alg_compilation]][[alg_modelisation]]
        p_pa <- plotComparaisonOccurrences_pa(sr, alg_comp, alg_modl)

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

            file_name <- paste("compilation", "pa", alg, tolower(nisl), xocc, sep = "_") %>%
              paste0(".png")

            P_hig <- joinedMaps(
              p_alg_pa[[alg]] %>% lapply(pluck, nisl, xocc),
              collect_guides = T,
              keep_title = F,
              plot_title = alg_lab
            )
            P_low <- joinedMaps(
              proportions_pa[[alg]][[nisl]], collect_guides = T
            )

            P <- (P_hig / P_low) +
              plot_layout(heights = c(0.8, 0.2))

            ggexport(
              P,
              filename = here(path_fig_compilation, file_name),
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
