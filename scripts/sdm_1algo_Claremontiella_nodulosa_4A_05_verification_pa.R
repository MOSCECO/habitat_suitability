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
spp_local <- sp %>%
  filter(individualCount > 0) %>%
  select(x, y)
spp_local_sf <- st_as_sf(
  spp_local,
  coords = c("x", "y"),
  remove = F,
  crs = "EPSG:4326"
)

# Valeurs modélisées associées aux occurrences ----
df <- as.data.frame(pa$`Claremontiella nodulosa`, xy = T) %>%
  filter(individualCount == 1) %>%
  select(-individualCount)
pa_extract <- lapply(
  mods, terra::extract, df, method = "simple", xy = T
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

# densités d'adéquation à l'habitat
densities_pa <- sapply(
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

        tb0 <- tb %>%
          pivot_longer(cols = c("ensemble", "maxent", "rf")) %>%
          st_drop_geometry(.) %>%
          filter(value == 0) %>%
          group_by(., name) %>%
          summarise(., count = n()) %>%
          cbind(model_val = "absence")

        tb1 <- tb %>%
          pivot_longer(cols = c("ensemble", "maxent", "rf")) %>%
          st_drop_geometry(.) %>%
          filter(value == 1) %>%
          group_by(., name) %>%
          summarise(., count = n()) %>%
          cbind(model_val = "presence")

        tb2 <- tb1 %>% rbind(tb0)
        tb2$model_val <- factor(tb2$model_val, ordered = T)

        ggplot(
          tb2,
          aes(x = name, y = count, group = name, fill = model_val)
        ) +
          geom_col(col = "black", width = 0.8, position = "dodge2") +
          geom_text(
            aes(label = count),
            position = position_dodge2(width = 0.8),
            vjust = -0.7
          ) +
          scale_fill_manual(values = c("white", "green")) +
          guides(
            fill = guide_legend(title = NULL),
            col = guide_legend(title = "Algorithme\nde modélisation")
          ) +
          labs(
            title = paste0(binomial_name, " (", isl_lab, ")"),
            subtitle = paste("Algorithme de compilation :", alg_lab)
          ) +
          xlab("Algorithme de modélisation") +
          ylab("Densité * Nombre de points")

      },
      simplify = F,
      USE.NAMES = T
    )

  },
  simplify = F,
  USE.NAMES = T
)
