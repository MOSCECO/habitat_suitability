# Prise en compte de la profondeur maximale de l'espèce
# à partir des données locales et globales.

# Sorties : profondeurs minimales et maximales qui servent à découper le
# modèle de distribution de sortie (comme "dire d'expert") si les profondeurs
# sont aberrantes.

summaries_depths <- sapply(
  species$species,
  \(bn) {
    sf <- species$superFamily[species$species == bn]

    # Global
    world_depth <- read_stars(
      here("data", "raw", "dpth", "gebco_2023_n63.1055_s-60.293_w-143.4375_e-27.4219.tif")
    )
    world_occur <- list.files(
      here("data", "analysis", "bio", sf, bn), pattern = "global", full.names = T
    ) %>% readRDS()
    world_occur <- world_occur %>%
      filter(scale == "global" & type == "pr")
    res <- st_extract(world_depth, world_occur)
    names(res) <- c("depth", "geometry")
    res$depth <- as.numeric(res$depth)
    res$depth[res$depth > 0] <- 0

    # Local
    local_occur <- list.files(
      here("data", "analysis", "bio", sf, bn), pattern = "local", full.names = T
    ) %>% readRDS()
    local_occur <- local_occur %>% filter(type == "pr")

    # Quantile très similaires entre données globales et locales, bien qu'il y a
    # sûrement un biais pour les occurrences tombant dans des NA au niveau global.
    # Visualisation (fait planter l'ordi) :
    # library(units)
    # x11()
    # ggplot() +
    #   geom_stars(data = world_depth) +
    #   geom_sf(data = world_occur, col = "red")
    return(list(global = res$depth, local = local_occur$depth))
  },
  simplify = F,
  USE.NAMES = T
)

summaries_depths %>%
  lapply(\(sp) lapply(sp, summary))

summaries_depths %>%
  lapply(\(sp) lapply(sp, quantile, c(0.1, 0.9)))

summaries_depths %>%
  lapply(pluck, "local") %>%
  lapply(quantile, c(0.1, 0.9))
summaries_depths %>%
  lapply(pluck, "global") %>%
  lapply(summary)

# Les différences ne sont pas aussi importantes qu'observées sans modification
# du raster de climatologies global d'entrée (limité à 150m)

# Pour l'instant on garde les minima et maxima de distribution
boundaries_depths <- summaries_depths %>%
  lapply(
    \(l) {
      summary((Reduce(c, l)))[c("Min.", "Max.")]
    }
  )

p_depth_boundaries <- here("data", "analysis", "depth_boundaries")
makeMyDir(p_depth_boundaries)

lapply(
  names(boundaries_depths),
  \(n) {
    bnd <- boundaries_depths[[n]]
    file_name <- paste(
      gsub(" ", "_", n), "depth", "boundaries", sep = "_"
    ) %>% paste0(".rds")
    saveRDS(bnd, here(p_depth_boundaries, file_name))
  }
)
