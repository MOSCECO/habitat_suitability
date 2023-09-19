################################################################################
#### MODÈLE GLOBAL - DONNÉES ENVIRONNEMENTALES COPERNICUS                   ####
################################################################################

source(here::here("scripts", "boot.R"))

# PARAMÉTRAGE ####
# "GLM", "GBM", "GAM", "CTA", "ANN", "SRE",
# "FDA", "MARS", "RF", "MAXENT", "MAXNET"
lapply(
  species$species,
  \(bn) {
    lapply(
      c("RF", "MAXENT", "ENS"),
      \(alg) {

        cat(
          "Début de routine\n",
          "Espèce : ", bn, "\n",
          "Algorithme : ", alg, "\n"
        )

        # alg <- "RF"
        # alg <- "MAXENT"
        # alg <- "ensemble"
        # Nombre de répétitions (nombre de jeux de validation croisées)
        CV_nb_rep <- 5

        # nom du modèle
        vec_name_model <- c(paste0(tolower(alg), CV_nb_rep), "01", "global", "cpc", "2")
        pts_name_model <- paste(vec_name_model, collapse = ".")

        # Données biologiques ----
        # bn <- "Claremontiella nodulosa"
        supfam <- species$superFamily[species$species == bn]
        sp  <- pa[[bn]] %>% as.data.frame(xy = T)
        binnam <- str_split(bn, " ")[[1]] %>%
          lapply(substr, 1, 3) %>%
          paste0(collapse = ".")

        # jeux de données environnementales pour calibration du SDM ----
        # carte globale des variables environnementales
        clim_sub      <- cgc_sub[[supfam]][[bn]]
        clim_proj_sub <- subset(climosaic, names(clim_sub))

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
          cbind(terra::extract(clim_proj_sub, spp_local_sf, ID = F))

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
        spa_local_sf <- spa_local_sf %>%
          cbind(terra::extract(clim_proj_sub, spa_local_sf, ID = F))

        # Données mondiales ----
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
            type, id, scale, x = X, y = Y, individualCount, all_of(names(clim_sub))
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
          )

        # Aggrégation données biologiques ----
        bio_list <- list(spp_local_sf, spa_local_sf, spp_global_sf, spa_global_sf)
        bio <- do.call(rbind, bio_list)
        bio <- bio %>%
          arrange(desc(individualCount), type)

        # sauvegarde des données biologiques
        # saveRDS(
        #   bio,
        #   here("data", "analysis", "bio_data_cpc.rds")
        # )

        # Application de la fonction de production d'un SDM pour un seul algorithme
        sdmOneAlgo2(
          alg            = alg,
          CV_nb_rep      = CV_nb_rep,
          binnam         = binnam,
          bn             = bn,
          vec_name_model = vec_name_model,
          bio            = bio,
          clim_sub       = clim_proj_sub,
          clim_proj_sub  = clim_proj_sub,
          pts_name_model = pts_name_model
        )

        cat(
          "Fin de routine\n"
        )

      }
    )
  }
)
