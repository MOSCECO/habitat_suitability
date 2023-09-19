# boot data_occ_preparation

# libraries ----
libs_to_call <- list(

  "ade4",
  "biomod2",
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
  # "usdm",
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
# Sys.getenv("GITHUB_PAT")
# Sys.unsetenv("GITHUB_PAT")
# Sys.getenv("GITHUB_PAT")

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

# chemin vers le dossier des projets R
pp <- "/home/borea/Documents/mosceco/r_projects/MOSCECO_L2"

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

# stations & évènements de collectes
stations <- readRDS(
  here("data", "raw", "shp", "stations_me_sf.rds")
)
stations_nearest <- readRDS(
  here("data", "raw", "shp", "stations_ant_nearest.rds")
)

# masses d'eau de la DCE et artificielles
me <- readRDS(
  here("data", "raw", "shp", "ART_masses_d-eaux", "me.rds")
)

# mtds <- c("mean", "stdv", "mini", "maxi", "qt01", "qt05", "qt95", "qt99")
mtds <- c("mean", "stdv")

# climatologies avec salinités hybrides ----
climatologies <- lapply(
  list.files(
    here("data", "raw", "climatologies_spatRaster"),
    full.names = T,
    pattern = "updated_so"
  ),
  rast
)
names(climatologies) <- islands

# aggrégation en mosaïque
climosaic <- mapply(
  \(nx, ny) {
    x <- climatologies$GLP[[nx]]
    y <- climatologies$MTQ[[ny]]
    terra::mosaic(x, y)
  },
  names(climatologies$GLP),
  names(climatologies$MTQ),
  SIMPLIFY = F,
  USE.NAMES = T
)
climosaic <- Reduce(c, climosaic)

# Visualisation des données
# lapply(
#   mtds,
#   \(mtd) {
#     vmtd <- names(climosaic)[grepl(mtd, names(climosaic))]
#     x11() ; plot(climosaic[[vmtd]])
#   }
# )

# réduction de la colinéarité des données brutes : climosaic_sub
# climatologies avec des valeurs issues d'une ACP sur toutes les variables
# source(here("scripts", "climatologies_pca.R"))

# espèces communes filtrées pour 30 spécimens minimum (présences/absences)
# pa <- readRDS(
#   here("data", "raw", "occ_threshold", "list_occ_thresh.rds")
# )
pa <- readRDS(
  here("data", "raw", "occ", "list_occ_thresh_nearest.rds")
)

# "rasterisation" des données de présences-absences obtenues par le Muséum
source(here("scripts", "rasterisation_pa.R"))

# Species Distribution Modeling
# (1) Climatologies brutes
# source(here("scripts", "sdm_Claremontiella_nodulosa_pca_new.R"))
# (2) ACP sur les climatologies
# source(here("scripts", "sdm_Claremontiella_nodulosa_mean_stdv.R"))
# (3) Filtres successifs : niche mondiale, niche observée
# copié de data_occ_prep > data > tidy > occ > global_occ_filtered

# donées biologiques mondiales ----
# cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/data_environment/data/tidy/occurrences_with_depths_150m.rds /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/raw/occ/"
# system(cmd)
global_occf <- readRDS(here("data", "raw", "occ", "occurrences_with_depths_150m.rds"))

# climatologies au niveau mondial ----

# Copie des climatologies nécessaires pour le niveau global
# points de données par espèces
# cmd <- paste(
#   "rsync",
#   "-avuc",
#   "--delete",
#   paste(pp, "data_environment/data/tidy/clim_global/", sep = "/"),
#   here("data", "raw", "climatologies_globales_points")
# )
# system(cmd)

# raster mondiaux
p_cgc <- here("data", "raw", "climatologies_globales_raster")
makeMyDir(p_cgc)
# cmd <- paste(
#   "rsync",
#   "-avuc",
#   "--delete",
#   paste(
#     pp,
#     "data_environment/data/analysis",
#     "climatologies_global/climatologies_globales_copernicus.tif",
#     sep = "/"),
#   p_cgc
# )
# system(cmd)

cgc <- p_cgc %>%
  list.files(full.names = T) %>%
  rast()
names(cgc) <- gsub("bottomt", "sbt", names(cgc))
names(cgc) <- gsub("vhm0", "hm0", names(cgc))

# Espèces à modéliser
species <- tibble(
  superFamily = c(rep("Majoidea", 2), rep("Muricoidea", 2)),
  species     = c(
    # "Amphithrax hemphilli",
    # "Macrocoeloma nodipes",
    # "Mithraculus coryphe",
    "Mithraculus forceps",
    # "Mithrax pleuracanthus",
    # "Omalacantha bicornuta",
    "Stenorhynchus seticornis",
    # "Teleophrys ruber",
    "Claremontiella nodulosa",
    # "Coralliophila galea",
    # "Coralliophila salebrosa",
    # "Favartia alveata",
    # "Favartia varimutabilis",
    # "Phyllonotus pomum",
    # "Siratus consuelae",
    "Stramonita rustica",
    # "Trachypollia didyma",
    # "Vasula deltoidea",
    NULL
  )
)
# clim_global <- sapply(
#   superFamilies,
#   \(supfm) {
#     sapply(
#       species$species[species$superFamily == supfm],
#       \(spe) {
#         cg <- list.files(
#           here("data", "raw", "climatologies_globales_points", supfm, spe),
#           full.names = T
#         ) %>%
#           lapply(read_csv, show_col_types = FALSE)
#         names(cg) <- list.files(
#           here("data", "raw", "climatologies_globales_points", supfm, spe)
#         ) %>%
#           str_split("\\.") %>%
#           lapply(pluck, 1) %>%
#           str_split("_") %>%
#           lapply(., \(x) pluck(x, length(x))) %>%
#           unlist()
#         names(cg)[which(names(cg) %in% "bottomt")] <- "sbt"
#         names(cg)[which(names(cg) %in% "vhm0")]    <- "hm0"
#         return(cg)
#       },
#       simplify  = F,
#       USE.NAMES = T
#     )
#   },
#   simplify  = F,
#   USE.NAMES = T
# )

# Modification de la forme des climatologies global pour avoir une matrice
# clim_global_tb <- sapply(
#   names(clim_global),
#   \(supfm) {
#     sapply(
#       names(clim_global[[supfm]]),
#       \(spe) {
#
#         # supfm <- "Majoidea"
#         # spe <- "Stenorhynchus seticornis"
#         cg <- clim_global[[supfm]][[spe]]
#
#         # coords <- cg$sbt[, c("x", "y")]
#
#         # uniformisation des stations présentes (problème avec so chais pas pk)
#         z <- names(which.min(lapply(cg, nrow) %>% unlist()))
#         tbm <- cg[[z]]
#         stn <- paste(tbm$x, tbm$y)
#
#         coords <- tbm[, c("x", "y")]
#
#         cg2 <- lapply(
#           names(cg),
#           \(n) {
#             tb <- cg[[n]]
#
#             # uniformisation des stations présentes (problème avec so chais pas pk)
#             tb <- if (nrow(tb) > length(stn)) {
#               tbi <- tb[paste(tb$x, tb$y) %in% stn, ]
#               tbi <- tbi[which(paste(tbi$x, tbi$y) %in% stn), ]
#               tbi
#             } else {
#               tb[which(paste(tb$x, tb$y) %in% stn), ]
#             }
#
#             tb <- tb %>%
#               select(-c(x, y))
#             names(tb) <- names(tb) %>%
#               str_split("_") %>%
#               lapply(pluck, 2) %>%
#               paste(n, sep = ".")
#
#             return(tb)
#           })
#
#         cbind(coords, do.call(cbind, cg2)) %>%
#           na.omit()
#       },
#       simplify = F,
#       USE.NAMES = T
#     )
#   },
#   simplify = F,
#   USE.NAMES = T
# )

# sélection des variables environnementales uniquement
# et les métriques que l'on veut sélectionner (ici sd et mean)
# cgc_sub <- sapply(
#   names(clim_global_tb),
#   \(supfm) {
#     sapply(
#       names(clim_global_tb[[supfm]]),
#       \(spe) {
#         tb <- clim_global_tb[[supfm]][[spe]]
#         tb <- tb %>%
#           select(names(.)[grepl("stdv|mean", names(.))])
#         return(tb)
#       },
#       simplify = F,
#       USE.NAMES = T
#     )
#   },
#   simplify = F,
#   USE.NAMES = T
# )

# Filtre des variables colinéaires
# cgc_sub <- sapply(
#   names(cgc_sub),
#   \(supfm) {
#     sapply(
#       names(cgc_sub[[supfm]]),
#       \(spe) {
#         tb <- cgc_sub[[supfm]][[spe]]
#         var_col <- usdm::vifstep(as.data.frame(tb %>% na.omit()))@excluded
#         tb <- tb[, !names(tb) %in% var_col]
#         return(tb)
#       },
#       simplify = F,
#       USE.NAMES = T
#     )
#   },
#   simplify = F,
#   USE.NAMES = T
# )
# saveRDS(cgc_sub, here("data", "raw", "cgc_sub.rds"))
cgc_sub <- readRDS(here("data", "raw", "cgc_sub.rds"))

# Même object en sf
# cgc_sub_sf <- sapply(
#   names(cgc_sub),
#   \(supfm) {
#     sapply(
#       names(cgc_sub[[supfm]]),
#       \(spe) {
#         nms <- names(cgc_sub[[supfm]][[spe]])
#         tb <- clim_global_tb[[supfm]][[spe]]
#         st_as_sf(
#           tb %>% select(all_of(c("x", "y", nms))),
#           coords = c("x", "y"),
#           crs = "EPSG:4326"
#         )
#       },
#       simplify = F,
#       USE.NAMES = T
#     )
#   },
#   simplify = F,
#   USE.NAMES = T
# )
# saveRDS(cgc_sub_sf, here("data", "raw", "cgc_sub_sf.rds"))
cgc_sub_sf <- readRDS(here("data", "raw", "cgc_sub_sf.rds"))

# Import des climatologies locales ----

# sextant local climatologies
# sxt_sub <- climosaic %>%
#   terra::subset(names(.)[grepl("ssm|tur|chla|sst", names(.))])
# var_col <- usdm::vifstep(as.data.frame(sxt_sub %>% na.omit()))@excluded
# sxt_sub <- subset(sxt_sub, names(sxt_sub)[!names(sxt_sub) %in% var_col])
# saveRDS(sxt_sub, here("data", "raw", "sxt_sub.rds"))
sxt_sub <- readRDS(here("data", "raw", "sxt_sub.rds"))

# Habitat local spatial raster
# hab_sub <-  climosaic %>%
#   terra::subset(names(.)[grepl("depth", names(.))])
# hab_sub$slope <- terra::terrain(hab_sub)
# var_col <- usdm::vifstep(as.data.frame(hab_sub %>% na.omit()))@excluded
# hab_sub <- subset(hab_sub, names(hab_sub)[!names(hab_sub) %in% var_col])
# saveRDS(hab_sub, here("data", "raw", "hab_sub.rds"))
hab_sub <- readRDS(here("data", "raw", "hab_sub.rds"))
