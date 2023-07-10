# boot data_occ_preparation

# libraries ----
libs_to_call <- list(
  
  "ade4", 
  "adehabitatHS",
  "adehabitatMA",
  "biomod2", 
  "CENFA",
  "data.table",
  "devtools",
  "dismo",
  "doMPI",
  "factoextra",
  "FactoMineR",
  "ggnewscale",
  "ggplot2",
  "ggpubr", 
  "ggthemes",
  "here",
  "missMDA", 
  "ncdf4", 
  "patchwork", 
  "purrr",
  "raster", 
  "reshape2",
  "sf", 
  "sp", 
  "stars",
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
    install.packages(i, dependencies = T)
  }
  
  library(i, character.only = TRUE)
  
}
)

# remote libraries (github)
Sys.getenv("GITHUB_PAT")
Sys.unsetenv("GITHUB_PAT")
Sys.getenv("GITHUB_PAT")

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
m <- st_read(here("data", "raw", "shp", "mappemonde", "mappemonde.shp"))

# polygones îles ----
maps <- list.files(
  here("data", "raw", "shp", "polygones_iles"),
  pattern = "*.shp", 
  full.names = T
) %>% 
  lapply(st_read)
names(maps) <- islands

# climatologies avec salinités hybrides ----
climatologies <- lapply(
  list.files(
    here("data", "tidy", "climatologies_spatRaster"), 
    full.names = T,
    pattern = "updated_so"
  ),
  rast
)
names(climatologies) <- islands

# aggrégation en mosaïque
climosaic <- Reduce(mosaic, climatologies)

# Visualisation des données
mtds <- c("mean", "stdv")
lapply(
  mtds, 
  \(mtd) {
    vmtd <- names(climosaic)[grepl(mtd, names(climosaic))]
    x11() ; plot(climosaic[[vmtd]])
  }
)

# espèces communes filtrées pour 30 spécimens minimum (présences/absences)
# pa <- readRDS(
#   here("data", "raw", "occ_threshold", "list_occ_thresh.rds")
# )
pa <- readRDS(
  here("data", "raw", "occ_threshold", "list_occ_thresh_nearest.rds")
)

# "rasterisation" des données de présences-absences obtenues par le Muséum
source(here("scripts", "rasterisation_pa.R"))
cgc <- here("data", "raw", "clim_cenfa_clanod", "clims.rds") %>% 
  readRDS() %>% 
  rast()
cgc_clanod <- readRDS(
  here("data", "raw", "clim_cenfa_clanod", "dataset_occ_clims.rds")
)
source(here("scripts", "ENFA_global_clanod3.R"))