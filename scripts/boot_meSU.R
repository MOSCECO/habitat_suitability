# boot data_occ_preparation

# libraries ----
libs_to_call <- list(
  
  "data.table",
  "devtools",
  "doMPI",
  "foreach", 
  "ggnewscale",
  "ggplot2",
  "ggpubr", 
  "here",
  # "ncdf4", 
  "patchwork", 
  "purrr",
  "raster",
  "reshape2",
  "sf", 
  "sp", 
  "stringr",
  "terra",
  "tidync",
  "tidyverse",
  "vegan"
  
)

# library calls
lapply(libs_to_call, function(i) {
  
  bool <- is.element(i, .packages(all.available = TRUE))
  
  if (!bool) {
    install.packages(i)
  }
  
  library(i, character.only = TRUE)
  
}
)

# remote libraries ----
# remote_libs_to_call <- list(
#   # "RCMEMS"
#   "ClimateOperators"
# )
# 
# github_accounts <- list(
#   "markpayneatwork"
# )
# 
# mapply(
#   function(pckg, usr) {
# 
#     bool <- is.element(pckg, .packages(all.available = TRUE))
#     
#     if (!bool) {
#       path_url <- paste0(usr, "/", pckg)
#       print(path_url)
#       devtools::install_github(path_url)
#     }
# 
#     library(pckg, character.only = TRUE)
# 
#   },
#   remote_libs_to_call,
#   github_accounts,
#   SIMPLIFY = FALSE
# )

# functions
lapply(
  list.files(
    here("scripts", "FUN"),
    full.names = T
  ),
  source
)

# importations d'autres projets R ----
# data_occ_analyses : "data", "raw", "shp"
#                (polygones des îles, masses d'eau DCE, bathymétrie mondiale,
#                stations et masses d'eaux modifiées)
#                 "data", "tidy", "occ"
#                (occurrences formatées et filtrées selon un seuil (10 occ))
#                01/02/2023
#                "data", "tidy", "occ", "ANT", + comm_species_* ( 2 fichiers)
#                (occurrences formatées et filtrées selon un seuil (10 occ)
#                pour les espèces communes aux deux îles)

# shapefiles ----
sf::sf_use_s2(FALSE)
wgs <- "EPSG:4326"
utm20n <- "EPSG:32620"
islands <- c("GLP", "MTQ")
names(islands) <- islands
Taxa <- c("Majoidea", "Muricidae")
names(Taxa) <- Taxa
taxa <- c("majo", "muri")
names(taxa) <- taxa
colors_taxa <- c("#d04c4e", "#5765b4")
names(colors_taxa) <- Taxa

# polygones îles ----
# maps <- list.files(
#   here("data", "raw", "shp", "polygones_iles"),
#   pattern = "*.shp", 
#   full.names = T
# ) %>% 
#   lapply(st_read)
# names(maps) <- islands

# stations & évènements de collectes
stations <- readRDS(
  here("data", "raw", "shp", "stations_me_sf.rds")
)

# masses d'eau de la DCE et artificielles
# me <- readRDS(
#   here("data", "raw", "shp", "ART_masses_d-eaux", "me.rds")
# )

# profondeurs
# bathy <- readRDS(
#   here("data", "raw", "shp", "bathymetries.rds")
# )

# variables environnementales
# env <- lapply(
#   islands, 
#   function(isl) {
#     l <- list.files(
#       here("data", "raw", "env", isl),
#       full.names = T,
#       pattern = "env_dataset*"
#     ) %>% 
#       lapply(readRDS)
#     names(l) <- list.files(
#       here("data", "raw", "env", isl),
#       pattern = "env_dataset*"
#     ) %>% 
#       substr(17, nchar(.) - 4)
#     return(l)
#   }
# )
# names(env) <- islands

# ocurrences d'espèces  ----
# species <- readRDS(
#   here("data", "raw", "occ", "common_species_threshold_incidence.rds")
# )
# species_abs <- readRDS(
#   here("data", "raw", "occ", "common_species_threshold_absences.rds")
# )
# values_varenvs_tax2 <- readRDS(
#   here("data", "analysis", "valeurs_varenv_comme_sp_threshold.rds")
# )

# netcdf des données environnementales ----
# copernicus <- lapply(
#   islands,
#   function(isl) {
#     path <- here("data", "tidy", "nc", isl, "copernicus")
#     
#     # variables environnementales en format netcdf
#     ncs <- list.files(path, full.names = T) %>%
#       lapply(tidync)
#     names(ncs) <- list.files(path) %>% 
#       substr(39, nchar(.) - 3)
#     ncs <- ncs[sort(names(ncs))]
#     
#     return(ncs)
#   }
# )
# copernicus_raster_brick <- lapply(
#   islands,
#   function(isl) {
#     path <- here("data", "tidy", "nc", isl, "copernicus")
#     
#     # variables environnementales en format netcdf
#     ncs <- list.files(path, full.names = T) %>%
#       lapply(brick)
#     names(ncs) <- list.files(path) %>% 
#       substr(39, nchar(.) - 3)
#     ncs <- ncs[sort(names(ncs))]
#     
#     return(ncs)
#   }
# )
# sextant <- lapply(
#   islands,
#   function(isl) {
#     path <- here("data", "tidy", "nc", isl, "sextant")
#     
#     # variables environnementales en format netcdf
#     ncs <- list.files(path, full.names = T) %>%
#       lapply(tidync)
#     names(ncs) <- list.files(path) %>% 
#       substr(36, nchar(.) - 3)
#     ncs <- ncs[sort(names(ncs))]
#     
#     return(ncs)
#   }
# )
# sextant_raster_brick <- lapply(
#   islands,
#   function(isl) {
#     path <- here("data", "tidy", "nc", isl, "sextant")
#     
#     # variables environnementales en format netcdf
#     ncs <- list.files(path, full.names = T) %>%
#       lapply(brick)
#     names(ncs) <- list.files(path) %>% 
#       substr(36, nchar(.) - 3)
#     ncs <- ncs[sort(names(ncs))]
#     
#     return(ncs)
#   }
# )

env_vars <- c("hm0", "sbt", "so130", "so25", "so77", "so9", "sw1", "ww")

# attribution de chaque donnée environnementale au jour-même
# source(here("scripts", "extraction_parametre_abiotique_netcdf.R"))

# attribution de chaque donnée environnementale à valeur issue d'une 
# climatologie
# source(here("scripts", "extraction_parametre_abiotique_climatologies_netcdf_meSU.R"))