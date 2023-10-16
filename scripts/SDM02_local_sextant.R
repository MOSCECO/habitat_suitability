################################################################################
#### MODÈLE LOCAL - DONNÉES ENVIRONNEMENTALES SEXTANT                       ####
################################################################################

source(here::here("scripts", "boot.R"))

# PARAMÉTRAGE ####
# "GLM", "GBM", "GAM", "CTA", "ANN", "SRE",
# "FDA", "MARS", "RF", "MAXENT", "MAXNET"
lapply(
  # species$species,
  species$species[species$superFamily == "Muricoidea"],
  \(bn) {
    lapply(
      # c("RF", "MAXENT", "ENS"),
      c("ENS"),
      \(alg) {

        # bn <- "Mithraculus forceps"
        # alg <- "ENS"

        cat(
          "Début de routine\n",
          "Espèce : ", bn, "\n",
          "Algorithme : ", alg, "\n"
        )

        CV_nb_rep <- 5

        # nom du modèle
        vec_name_model <- c(
          paste0(tolower(alg), CV_nb_rep), "02", "local", "sxt"
        )
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
        clim_sub      <- sxt_sub
        clim_proj_sub <- clim_sub

        # Données d'occurrences ----
        bio <- here("data", "tidy", "bio", supfam, bn, "bio_local.rds") %>%
          readRDS()
        bio_info <- bio[, 1:6] %>% st_drop_geometry()
        bio_data <- bio[, 7:ncol(bio)] %>% st_drop_geometry()
        bio_data <- bio_data %>% select(all_of(names(clim_sub)))
        bio      <- bio_info %>%
          cbind(bio_data) %>%
          st_as_sf(geometry = st_geometry(bio))

        # Application de la fonction de production d'un SDM pour un seul algorithme
        sdmOneAlgo2(
          alg            = alg,
          CV_nb_rep      = CV_nb_rep,
          supfam         = supfam,
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
