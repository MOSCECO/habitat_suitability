# boot data_occ_preparation

# libraries ----
libs_to_call <- list(
  
  "ade4", 
  "biomod2", 
  "data.table",
  "devtools",
  "factoextra",
  "FactoMineR",
  "ggnewscale",
  "ggplot2",
  "ggpubr", 
  "here",
  "missMDA", 
  "ncdf4", 
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
remote_libs_to_call <- list(
  # "RCMEMS"
  "ClimateOperators"
)

github_accounts <- list(
  "markpayneatwork"
)

mapply(
  function(pckg, usr) {
    
    bool <- is.element(pckg, .packages(all.available = TRUE))
    
    if (!bool) {
      path_url <- paste0(usr, "/", pckg)
      print(path_url)
      devtools::install_github(path_url)
    }
    
    library(pckg, character.only = TRUE)
    
  },
  remote_libs_to_call,
  github_accounts,
  SIMPLIFY = FALSE
)

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
superFamilies <- c("Majoidea", "Muricoidea")
names(superFamilies) <- superFamilies
Taxa <- c("Majoidea", "Muricidae")
names(Taxa) <- Taxa
taxa <- c("majo", "muri")
names(taxa) <- taxa
colors_taxa <- c("#d04c4e", "#5765b4")
names(colors_taxa) <- Taxa

# directories ----
path_models <- here("data", "analysis", "models")
makeMyDir(path_models)

# polygones îles ----
maps <- list.files(
  here("data", "raw", "shp", "polygones_iles"),
  pattern = "*.shp", 
  full.names = T
) %>% 
  lapply(st_read)
names(maps) <- islands

# polygons géologie Martinique ----
# mq <- list.files(
#   here("data", "raw", "shp", "brgm_cartes_geologiques", "MTQ"),
#   pattern    = "*.shp", 
#   full.names = T
# ) %>% 
#   lapply(st_read)
# 
# mq <- mq[
#   lapply(mq, \(x) x %>% st_geometry_type() %>% unique()) %>% 
#     unlist() %in% c("MULTIPOLYGON", "POLYGON")
# ]
# mq <- mq[
#   lapply(mq, nrow) > 0
# ]
# mq <- lapply(
#   mq, 
#   select, 
#   Reduce(intersect, lapply(mq, names))
# )
# mq_collec <- do.call(rbind, mq)
# mq_collec <- mq_collec %>% 
#   filter(DESCR != "Zone non cartographiée")
# mq_collec <- st_transform(mq_collec, st_crs(stations$MTQ))

# stations & évènements de collectes
stations <- readRDS(
  here("data", "raw", "shp", "stations_me_sf.rds")
)

# masses d'eau de la DCE et artificielles
me <- readRDS(
  here("data", "raw", "shp", "ART_masses_d-eaux", "me.rds")
)

# profondeurs
extents <- list(
  GLP = list(
    x = c(-62.00949, -60.647),
    y = c(15.65, 16.802)
  ),
  MTQ = list(
    x = c(-61.33667, -60.64367 ),
    y = c(14.27667, 15.02067)
  )
)

# Profondeurs ----
# GEBCO, un chouïa moins précis que MNT (au dix-millième près)
bathy <- readRDS(
  here("data", "raw", "shp", "bathymetries.rds")
)

# attribution de chaque donnée environnementale à valeur issue d'une 
# climatologie
# source(here("scripts", "climatologies_copernicus.R"))
# source(here("scripts", "climatologies_sextant.R"))
# Pendant meSU
# source(here("scripts", "boot_meSU.R"))
# source(here("scripts", "climatologies_copernicus_mesu.R"))
# source(here("scripts", "climatologies_sextant_mesu.R"))

# raster de salinité qui correspond aux profondeurs des zones d'études
# source(here("scripts", "raster_hybride_salinite.R"))

# regroupement des climatologies
mtd <- "mean"
source(here("scripts", "regroupement_climatologies.R"))


# climatologies ----
env_vars_spatRaster <- lapply(
  list.files(here("data", "tidy", "climatologies_spatRaster"), full.names = T),
  rast
)
names(env_vars_spatRaster) <- islands

# aggrégation en mosaïque
env_vars_spatRaster_resample <- env_vars_spatRaster %>% 
  lapply(
    \(r) {
      sr0 <- r
      res(sr0) <- rep(0.001, 2)
      return(terra::resample(r, sr0, method = "near"))
    }
  )
climosaic <- Reduce(mosaic, env_vars_spatRaster_resample)

# climatologies avec des valeurs issues d'une ACP sur toutes les variables 
source(here("scripts", "climatologies_pca.R"))

# espèces communes filtrées pour 30 spécimens minimum (présences/absences)
pa <- readRDS(
  here("data", "raw", "occ_threshold", "list_occ_thresh.rds")
)
# pa <- readRDS(
#   here("data", "raw", "occ_threshold", "list_occ_thresh_nearest.rds")
# )

# "rasterisation" des données de présences-absences obtenues par le Muséum
source(here("scripts", "rasterisation_pa.R"))
