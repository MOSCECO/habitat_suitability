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

        # Données d'occurrences ----
        bio <- here("data", "tidy", "bio", supfam, bn, "bio_global.rds") %>%
          readRDS()

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
