# Méthode avec les climatologies acquises pour chaque occurrence d'espèces
# (old)
global_clims <- readRDS(here("data", "raw", "ENFA", "clims.rds"))

# global_clim <- lapply(
#   list.files(
#     here("data", "raw", "clim_global"),       # superfamille
#     full.names = T
#   ),
#   \(p_sf) {
#     l_sp <- lapply(
#       list.files(p_sf, full.names = T),
#       \(p_sp) {
#         l_cl <- lapply(
#           list.files(p_sp, full.names = T),
#           read.csv
#         )
#         names(l_cl) <- list.files(p_sp) %>% substr(1, nchar(.) - 4)
#         return(l_cl)
#       } )
#     names(l_sp) <- list.files(p_sf)
#     return(l_sp)
#   } )
# names(global_clim) <- list.files(here("data", "raw", "clim_global"))

global_clim <- sapply(
  names(global_occf),
  \(supfam) {
    # supfam <- "Majoidea"
    sapply(
      names(global_occf[[supfam]]),
      \(sp) {
        # sp <- names(global_occf[[supfam]])[2]
        occ <- global_occf[[supfam]][[sp]]
        st_extract(global_clims, occ, bilinear = T)
      },
      simplify = F, USE.NAMES = T
    )
  },
  simplify = F, USE.NAMES = T
)

# Complétion des NaN
buffer = 1000
global_clim_completed <- sapply(
  names(global_clim),
  \(supfam) {
    # supfam <- "Majoidea"
    sapply(
      names(global_clim[[supfam]]),
      \(sp) {
        # sp <- names(global_clim[[supfam]])[2]
        sf <- global_clim[[supfam]][[sp]]
        sf <- cbind(ID = 1:nrow(sf), sf)
        tb <- sf %>% st_drop_geometry()

        tb_extract_sf <- tibble()

        while(length(table(is.nan(tb[, 2]))) > 1) {

          print(buffer)

          # sélection des valeurs non disponibles
          tb_na_sf_pts <- sf %>% filter(is.nan(tb[, 2]))

          # projection
          tb_na_sfproj <- tb_na_sf_pts %>%
            st_wrap_dateline() %>%
            st_transform(crs = "EPSG:4087")

          # zone tampon
          tb_na_sf_plg <- st_buffer(tb_na_sfproj, buffer)

          # reprojection
          # tb_na_sf_plg <- st_transform(tb_na_sf_plg, crs = "EPSG:4326")
          global_clims_proj <- st_transform_proj(global_clims, "EPSG:4087")
          global_clims_proj_sf <- st_as_sf(global_clims_proj)
          global_clims_proj_rast <- rast(global_clims_proj_sf)
          # nouvelle extraction

          stars_extract <- st_extract(
            global_clims_proj, tb_na_sf_plg, FUN = mean
          )
          sf_extract <- st_as_sf(stars_extract)
          tb_extract <- cbind(
            ID = tb_na_sf_plg$ID,
            sf_extract %>% st_drop_geometry()
          )
          # ajout des nouvelles données dans la table initiale
          id0_nona <- tb_extract$ID[!is.na(tb_extract[, 2])]

          if(length(id0_nona) > 0) {

            id1_nona <- which(!is.na(tb_extract[, 2]))
            nm_na <- names(tb)[apply(tb, 2, \(x) TRUE %in% is.na(x))]
            tb[id0_nona, nm_na] <- tb_extract %>% select(-ID) %>% na.omit()
            # sauvegarde des polygones utilisées pour extraire les valeurs moyennes
            tb_na_sf_plg <- tb_na_sf_plg %>% cbind(
              st_coordinates(tb_na_sf_pts) %>%
                as.data.frame() %>%
                select(x = X, y = Y)
            )
            coords <- tb_na_sf_plg[id1_nona, c("x", "y")]
            tb_extract <- tb_extract %>%
              na.omit() %>%
              cbind(coords, buffer = buffer) %>%
              st_as_sf()

            tb_extract_sf <- tb_extract_sf %>% rbind(tb_extract)

          }

          # incrémentation de la zone tampon
          buffer <- buffer + 1000

        }

        return(
          list(hs_val = tb, hs_plg = tb_extract_sf[order(tb_extract_sf$ID), ])
        )

      },
      simplify = F, USE.NAMES = T
    )
  },
  simplify = F, USE.NAMES = T
)
hs_extract <- mapply(
  \(m, tb) {

    # m <- mods$wmean
    # tb <- vals_hs$wmean


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

# association données environnementales
source(here("scripts", "global_cla_nod.R"))

# Méthode pour ENFA avec climatologies sur tout le pourtour des amériques
# copernicus global climatologies
cgc <- here("data", "raw", "clim_cenfa_clanod", "clims.rds") %>%
  readRDS() %>%
  rast()
cgc_clanod <- readRDS(
  here("data", "raw", "clim_cenfa_clanod", "dataset_occ_clims.rds")
)

# Troisième approche
# Modèle global projeté sur le local (donnés copernicus)
source(here("scripts", "ENFA_global_clanod2.R"))
