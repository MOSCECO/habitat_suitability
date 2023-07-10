# Regroupement et standardisation des climatologies

# Importation
env_vars_raster <- lapply(
  mtds, 
  \(mtd) {
    lapply(
      islands, 
      function(isl) {
        env_vars <- lapply(
          list.files(here("data", "tidy", "climatology_mesu", isl)),
          function(vrv) {
            r <- list.files(
              here("data", "tidy", "climatology_mesu", isl, vrv),
              pattern = mtd,
              full.names = T
            ) %>% 
              rast()
            # ext(r) <- ext(c(extents[[isl]]$x, extents[[isl]]$y))
            crs(r) <- crs("epsg:4326")
            return(r)
          }
        )
        names(env_vars) <- list.files(
          here("data", "tidy", "climatology_mesu", isl)
        )
        return(env_vars)
      }
    )
  }
)
names(env_vars_raster) <- mtds
env_vars_raster <- env_vars_raster %>% transpose()

# décalage empirique trouvé pour corriger les données sextant 
# glp
# décalage chez sextant ?
# lapply(
#   c("chla", "ssm", "sst", "tur"),
#   \(sextant) {
#     rsex <- env_vars_raster$GLP$mean[[sextant]]
#     p <- ggplot() + 
#       geom_raster(
#         data = as.data.frame(rsex, xy = T),
#         aes(x, y, fill = layer),
#       )  +
#       geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5) + 
#       labs(title = sextant)
#     return(p)
#   }
# )
# décalage chez copernicus ?
# lapply(
#   names(env_vars_raster$GLP$mean)[which(!names(env_vars_raster$GLP$mean) %in%
#                                           c("chla", "ssm", "sst", "tur"))],
#   \(copern) {
#     rsex <- env_vars_raster$GLP$mean[[copern]]
#     p <- ggplot() + 
#       geom_raster(
#         data = as.data.frame(rsex, xy = T),
#         aes(x, y, fill = layer),
#       )  +
#       geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5) +
#       labs(title = copern)
#     return(p)
#   }
# )

# rsex <- env_vars_raster$GLP$mean$chla
# rsex_shift <- terra::shift(rsex, 0.003, 0.002)
# p1 <- ggplot() + 
#   geom_raster(
#     data = as.data.frame(env_vars_raster$GLP$mean$ww, xy = T),
#     aes(x, y, fill = layer),
#   )  +
#   geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5)
# p2 <- ggplot() + 
#   geom_raster(
#     data = as.data.frame(rsex, xy = T),
#     aes(x, y, fill = layer),
#   ) +
#   geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5)
# p3 <- ggplot() + 
#   geom_raster(
#     data = as.data.frame(rsex_shift, xy = T),
#     aes(x, y, fill = layer),
#   ) +
#   geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5)
# p1 + p2 + p3
# 
# mtq
# rsex <- env_vars_raster$MTQ$mean$chla
# rsex_shift <- terra::shift(rsex, 0.003, 0.003)
# p1 <- ggplot() + 
#   geom_raster(
#     data = as.data.frame(env_vars_raster$MTQ$mean$ww, xy = T),
#     aes(x, y, fill = layer),
#   )  +
#   geom_sf(data = maps$MTQ, col = NA, fill = "lightgreen", alpha = 0.5)
# p2 <- ggplot() + 
#   geom_raster(
#     data = as.data.frame(rsex, xy = T),
#     aes(x, y, fill = layer),
#   ) +
#   geom_sf(data = maps$MTQ, col = NA, fill = "lightgreen", alpha = 0.5)
# p3 <- ggplot() + 
#   geom_raster(
#     data = as.data.frame(rsex_shift, xy = T),
#     aes(x, y, fill = layer),
#   ) +
#   geom_sf(data = maps$MTQ, col = NA, fill = "lightgreen", alpha = 0.5)
# p1 + p2 + p3

# standardisation de l'emprise
df_exts <- lapply(
  env_vars_raster, 
  \(env_vars_isl) {
    do.call(
      rbind, 
      lapply(
        env_vars_isl, 
        \(env_vars) {
          do.call(
            rbind, lapply(env_vars, \(x) x %>% ext() %>% as.vector())
          ) %>% as_tibble()
        }
      )
    ) %>% as_tibble()
  }
)

# Rognage
exts_to_crop <- lapply(
  df_exts, 
  \(df_ext) {
    c(
      xmin = max(df_ext$xmin) %>% round(3),
      xmax = min(df_ext$xmax) %>% round(3),
      ymin = max(df_ext$ymin) %>% round(3),
      ymax = min(df_ext$ymax) %>% round(3)
    ) %>% ext()
  }
)
# tentative de diminution des effets de bord pour la Martinique
v <- as.vector(exts_to_crop$MTQ)
v[2] <- v[2] - 0.026
v[4] <- v[4] - 0.021
exts_to_crop$MTQ <- ext(v)

env_vars_raster_crop <- lapply(
  names(env_vars_raster), 
  \(isl) {
    env_vars_isl <- env_vars_raster[[isl]]
    ext_to_crop <- exts_to_crop[[isl]]
    lapply(
      env_vars_isl, 
      \(env_vars) {
        lapply(
          env_vars, 
          terra::crop,
          ext_to_crop
        )
      }
    )
  }
)

# Ré-échantillonnage
# rasters de destination
env_depths <- lapply(
  names(islands),
  \(isl) {
    r  <- terra::rast(
      list.files(
        here("data", "tidy", "climatology", isl, "depth"), full.names = T
      )
    )
    cr <- exts_to_crop[[isl]]
    r_crop <- terra::crop(r, cr)
    r_project <- terra::project(r_crop, "epsg:4326")
    names(r_project) <- "depth"
    return(r_project)
  }
)
names(env_depths) <- islands

env_vars_raster_resample <- lapply(
  names(env_vars_raster), 
  \(isl) {
    env_vars_isl <- env_vars_raster[[isl]]
    sr0 <- env_depths[[isl]]
    lapply(
      env_vars_isl, 
      \(env_vars) {
        
        lapply(
          env_vars, 
          terra::resample,
          y = sr0
        )
      }
    )
  }
)
names(env_vars_raster_resample) <- islands

# aggrégation
env_vars_spatRaster <- lapply(
  env_vars_raster_resample, 
  \(env_vars_isl) {
    Reduce(
      c, 
      lapply(
        names(env_vars_isl), 
        \(mtd) {
          vn <- names(env_vars_isl[[mtd]])
          rout <- Reduce(c, env_vars_isl[[mtd]])
          rout <- subset(rout, 1:12) # bizarre: Reduce semble rajouter une 
          # couche au spatial raster des minimum ?.?
          names(rout) <- paste(mtd, vn, sep = ".")
          return(rout)
        })
    )
  })

env_vars_spatRaster <- mapply(
  \(sr, rd) c(sr, rd), 
  env_vars_spatRaster,
  env_depths,
  SIMPLIFY = F, 
  USE.NAMES = T
)

# sauvegarde des rasters à même emprise, même résolution mais polygons des îles
# différents
pclim <- here("data", "tidy", "climatologies_spatRaster")
makeMyDir(pclim, del = TRUE)

# sauvegarde
mapply(
  \(r, isl) {
    writeRaster(
      r, 
      here(
        pclim, paste("climatologies", tolower(isl), sep = "_") %>% 
          paste0(".tif")
      ), 
      filetype = "GTiff",
      overwrite = TRUE
    )
  },
  env_vars_spatRaster,
  names(env_vars_spatRaster),
  SIMPLIFY = F, 
  USE.NAMES = T
)


# rd <- as.data.frame(env_vars_spatRaster$GLP$depth, xy = T)
# rc <- as.data.frame(env_vars_spatRaster$GLP$mean.chla, xy = T)
# rw <- as.data.frame(env_vars_spatRaster$GLP$mean.ww, xy = T)
# 
# x11()
# ggplot() +
#   geom_raster(data = rd, aes(x, y, fill = depth)) +
#   geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5)
# x11()
# ggplot() +
#   geom_raster(data = rc, aes(x, y, fill = mean.chla)) +
#   geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5)
# x11()
# ggplot() +
#   geom_raster(data = rw, aes(x, y, fill = mean.ww)) +
#   geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5)

# Le décalage vient des données Sextant !!! J'ai dû mal projeter ces couches
# pendant les interpolations...
# solution provisoire : just cropper comme ça, en sachant qu'il y a un 
# décalage...
# màj : le problème vient du type de grille nourri à l'algorithme 
# d'interpolation qui diffère entre sextant + hm0 et le reste. 
# Il n'y a pas nécessairement de problème d'interpolation je dirais...
# On rogne tel quel pour le moment, il faudra relancer toute la chaîne
# de production des données environnementales.
# Il y a des décalages avec la couche de profondeur... NA à prévoir dans les
# données. 

# Rognage des polygones des îles
# utilisation du paquet stars qui pourrait améliorer la pipeline des données
# environnementales
env_stars <- lapply(
  list.files(here("data", "tidy", "climatologies_spatRaster"), full.names = T),
  read_stars
)
names(env_stars) <- islands
env_stars_cropped <- islands %>% 
  lapply(
    \(isl) {
      # importation
      es <- env_stars[[isl]]
      sr <- env_vars_spatRaster[[isl]]$mean.chla
      
      # création de la zone à rogner (négatif du polygone de l'île)
      d <- ext(sr)[c(1,3,2,4)]
      plgz <- st_sfc(
        st_polygon(
          list(
            matrix(
              c(
                d[[1]], d[[2]],
                d[[1]], d[[4]],
                d[[3]], d[[4]],
                d[[3]], d[[2]],
                d[[1]], d[[2]]
              ),
              nrow = 5, 
              ncol = 2,
              byrow = T
            )
          )
        ),
        crs = "EPSG:4326"
      )
      plg_dif <- plgz %>% 
        st_difference(maps[[isl]][1, ]) %>% 
        st_difference(maps[[isl]][2, ]) %>% 
        st_difference(maps[[isl]][3, ])
      
      # Rognage
      es_out <- es[plg_dif]
      
      return(es_out)
    }
  )

# sauvegarde
mapply(
  \(r, isl) {
    write_stars(
      r,
      dsn = here(
        pclim, paste("climatologies", "crop", tolower(isl), sep = "_") %>% 
          paste0(".tif")
      ),
      driver  = "GTiff",
      progress  = TRUE,
      overwrite = TRUE
    )
  },
  env_stars_cropped,
  names(env_stars_cropped),
  SIMPLIFY = F, 
  USE.NAMES = T
)

# importation en spatial raster
env_vars_spatRaster_crop <- lapply(
  list.files(pclim, full.names = T, pattern = "crop"),
  rast
)
names(env_vars_spatRaster_crop) <- islands

# visualisation de la correspondance avec le polygone
lapply(
  names(env_vars_spatRaster_crop), 
  \(isl) {
    sr <- env_vars_spatRaster_crop[[isl]]
    ggplot() + 
      geom_raster(
        data = as.data.frame(sr$mean.chla, xy = T), 
        aes(x, y, fill = mean.chla)
      ) + 
      geom_sf(data = maps[[isl]], fill = "lightgreen", col = NA, alpha = 0.5)
  }
)

# Quelles sont les valeurs manquantes à travers toutes les couches ?
# Guadeloupe
gp <- as.data.frame(env_vars_spatRaster_crop$GLP, xy = T)
table(is.na(gp))
# FALSE     TRUE 
# 71294337      246 
# apply(gp, 2, \(x) table(is.na(x)))
# $depth
# FALSE    TRUE 
# 1397687     246
gp_sub <- gp[, c("x", "y", "depth")]
gp_sub$depth <- is.na(gp_sub$depth)
# ggplot() + 
#   geom_raster(data = gp_sub, aes(x, y, fill = depth))
# cellules ultra-côtières = mise à 0
gp[is.na(gp)] <- 0
env_vars_spatRaster_crop$GLP <- rast(gp, crs = "epsg:4326")

# Martinique
mq <- as.data.frame(env_vars_spatRaster_crop$MTQ, xy = T)
table(is.na(mq))
# FALSE     TRUE 
# 19812392      241 
# apply(mq, 2, \(x) table(is.na(x)))
# $depth
# FALSE   TRUE 
# 418723    241 
mq_sub <- mq[, c("x", "y", "depth")]
mq_sub$depth <- is.na(mq_sub$depth)
# ggplot() + 
#   geom_raster(data = mq_sub, aes(x, y, fill = depth))
# idem que pour la guadeloupe
# mise à 0
mq$depth[is.na(mq$depth)] <- 0

# mq_sub <- mq[, c("x", "y", "mean.chla")]
# mq_sub$mean.chla <- is.na(mq_sub$mean.chla)
# ggplot() + 
#   geom_raster(data = mq_sub, aes(x, y, fill = mean.chla))
# 
# mq_sub <- mq[, c("x", "y", "maxi.ww")]
# mq_sub$maxi.ww <- is.na(mq_sub$maxi.ww)
# ggplot() + 
#   geom_raster(data = mq_sub, aes(x, y, fill = maxi.ww))

# effets de bords supprimés en diminuant l'emprise pour les latitudes et 
# longitudes maximales. 
env_vars_spatRaster_crop$MTQ <- rast(mq, crs = "epsg:4326")

# sauvegarde
mapply(
  \(r, isl) {
    writeRaster(
      r, 
      here(
        pclim, 
        paste("climatologies", "nona", tolower(isl), sep = "_") %>% 
          paste0(".tif")
      ), 
      filetype = "GTiff",
      overwrite = TRUE
    )
  },
  env_vars_spatRaster_crop,
  names(env_vars_spatRaster_crop),
  SIMPLIFY = F, 
  USE.NAMES = T
)