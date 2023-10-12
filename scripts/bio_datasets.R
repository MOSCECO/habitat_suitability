lapply(
  species$species,
  \(bn) {

    # Données biologiques ----
    # bn <- "Claremontiella nodulosa"
    supfam <- species$superFamily[species$species == bn]
    sp  <- pa[[bn]] %>% as.data.frame(xy = T)

    # Données locales ----
    # Présences ----
    spp_local <- sp %>%
      filter(individualCount > 0) %>%
      cbind(type = "pr", id = paste0("pr", 1:nrow(.)), scale = "local") %>%
      select(type, id, scale, x, y, individualCount)
    spp_local_sf <- st_as_sf(
      spp_local,
      coords = c("x", "y"),
      remove = F,
      crs = "EPSG:4326"
    )
    spp_local_sf <- spp_local_sf %>%
      cbind(terra::extract(climosaic, spp_local_sf, ID = F))

    # Absences ----
    spa_local <- sp %>%
      filter(individualCount == 0) %>%
      cbind(type = "ab", id = paste0("ab", 1:nrow(.)), scale = "local") %>%
      select(type, id, scale, x, y, individualCount)
    spa_local_sf <- st_as_sf(
      spa_local,
      coords = c("x", "y"),
      remove = F,
      crs = "EPSG:4326"
    )
    climosaic$slope <- terra::terrain(climosaic$depth)
    spa_local_sf <- spa_local_sf %>%
      cbind(terra::extract(climosaic, spa_local_sf, ID = F))

    # sauvegarde des données biologiques locales ----
    path_bio <- here("data", "tidy", "bio")
    makeMyDir(path_bio)
    path_bio_sf <- here(path_bio, supfam)
    makeMyDir(path_bio_sf)
    path_biosf_species <- here(path_bio_sf, bn)
    makeMyDir(path_biosf_species)

    # Aggrégation données biologiques locales ----
    bio_list_local <- list(spp_local_sf, spa_local_sf)
    bio_local <- do.call(rbind, bio_list_local)
    bio_local <- bio_local %>%
      arrange(desc(individualCount), type)

    saveRDS(
      bio_local,
      here(path_biosf_species, "bio_local.rds")
    )

    # Données mondiales ----

    # jeux de données environnementales pour calibration du SDM ----
    # carte globale des variables environnementales
    clim_sub      <- cgc

    # Présences ----
    pas <- nrow(spp_local_sf) + 1
    spp_global_sf <- cgc_sub_sf[[supfam]][[bn]] %>%
      cbind(st_coordinates(cgc_sub_sf[[supfam]][[bn]])) %>%
      cbind(
        type = "pr",
        id = paste0("pr", pas:(pas + nrow(cgc_sub_sf[[supfam]][[bn]]) - 1)),
        scale = "global",
        individualCount = 1
      ) %>%
      select(
        type, id, scale,
        x = X, y = Y,
        individualCount,
        all_of(names(cgc_sub_sf[[supfam]][[bn]]))
      )

    # Pseudo-absences ----
    set.seed(123)
    pa_set <- bm_PseudoAbsences(
      resp.var    = rep(1, nrow(spp_global_sf)),
      expl.var    = subset(cgc, names(clim_sub)),
      nb.absences = nrow(spp_global_sf)*2,
      strategy    = "random"
    )
    # i <- 1
    # while(
    #   2 %in% (apply(
    #     pa_set$env[grepl("pa", row.names(pa_set$xy)), ], 2, \(x) table(is.na(x))
    #   ) %>% lengths())
    # ) {
    #   i <- i + 1
    #   print(i)
    #   set.seed(i)
    #   pa_set <- bm_PseudoAbsences(
    #     resp.var    = rep(1, nrow(spp_global_sf)),
    #     expl.var    = subset(cgc, names(clim_sub)),
    #     nb.absences = nrow(spp_global_sf)
    #   )
    print(
      apply(
        pa_set$env[grepl("pa", row.names(pa_set$xy)), ], 2, \(x) table(is.na(x))
      )
    )
    # )
    # }
    pa_set_nona <- (pa_set$env[grepl("pa", row.names(pa_set$xy)), ] %>%
                      na.omit())
    pa_set_nona <- pa_set_nona[
      sample(1:nrow(pa_set_nona), size = nrow(spp_global_sf)),
    ]

    spa_global_sf <- cbind(
      type = "pa",
      id = row.names(pa_set$xy),
      scale = "global",
      pa_set$xy,
      individualCount = 0,
      pa_set$env
    ) %>%
      filter(grepl("pa", .$id)) %>%
      as_tibble() %>%
      st_as_sf(
        .,
        coords = c("x", "y"),
        remove = F,
        crs = "EPSG:4326"
      ) %>%
      select(all_of(names(spp_global_sf)))

    # Aggrégation données biologiques ----
    bio_list_global <- list(
      spp_local_sf,
      spa_local_sf,
      spp_global_sf,
      spa_global_sf
    )
    vec_names <- Reduce(intersect, lapply(rev(bio_list_global), names))
    bio_list_global <- lapply(bio_list_global, select, all_of(vec_names))
    bio_global <- do.call(rbind, bio_list_global)
    bio_global <- bio_global %>%
      arrange(desc(individualCount), type)

    saveRDS(
      bio_global,
      here(path_biosf_species, "bio_global.rds")
    )

  }
)
