# Visualisation des modèles compilées (copernicus, sextant, gebco)
# avec densités des probabilités d'occurrences et
# nombre d'occurrences correctement prédites

# Espèce considérée ----
binomial_name <- "Claremontiella nodulosa"

# importation des modèles compilées ----
fs <- list.files(
  here("data", "analysis", "compilation"),
  full.names = T,
  pattern = "habitat-suitability"
)
ns <- list.files(
  here("data", "analysis", "compilation"), pattern = "habitat-suitability"
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

# densités d'adéquation à l'habitat
densities <- sapply(
  names(hs),
  \(alg) {

    # alg <- names(hs)[[1]]

    alg_lab <- switch(
      alg, wmean = "Moyenne pondérée", ca = "Moyenne d'ensemble"
    )

    sapply(
      names(hs[[alg]]),
      \(nisl) {

        # nisl <- names(hs[[alg]])[[1]]

        isl_lab <- switch(
          nisl,
          ANT = "Guadeloupe et Martinique",
          GLP = "Guadeloupe",
          MTQ = "Martinique"
        )

        tb <- hs[[alg]][[nisl]]

        tb_long <- tb %>% pivot_longer(cols = c("ensemble", "maxent", "rf"))
        ggplot(
          tb_long,
          aes(x = value, group = name, col = name, fill = name, after_stat(count))
        ) +
          geom_density(alpha = 0.6) +
          guides(
            fill = guide_legend(title = "Algorithme\nde modélisation"),
            col = guide_legend(title = "Algorithme\nde modélisation")
          ) +
          labs(
            title = paste0(binomial_name, " (", isl_lab, ")"),
            subtitle = paste("Algorithme de compilation :", alg_lab)
          ) +
          xlab("Adéquation de l'habitat") +
          ylab("Densité * Nombre de points")

      },
      simplify = F,
      USE.NAMES = T
    )

  },
  simplify = F,
  USE.NAMES = T
)
