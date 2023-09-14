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
    here("data", "tidy", "climatologies_spatRaster"),
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
lapply(
  mtds,
  \(mtd) {
    vmtd <- names(climosaic)[grepl(mtd, names(climosaic))]
    x11() ; plot(climosaic[[vmtd]])
  }
)

# réduction de la colinéarité des données brutes : climosaic_sub
# climatologies avec des valeurs issues d'une ACP sur toutes les variables
source(here("scripts", "climatologies_pca.R"))

# espèces communes filtrées pour 30 spécimens minimum (présences/absences)
# pa <- readRDS(
#   here("data", "raw", "occ_threshold", "list_occ_thresh.rds")
# )
pa <- readRDS(
  here("data", "raw", "occ_threshold", "list_occ_thresh_nearest.rds")
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

# donées biologiques globales
global_occf <- lapply(
  list.files(
    here("data", "raw", "occ", "global_occ_filtered"), full.names = T
  ),
  \(path) {
    d <- list.files(
      here(path),
      pattern = ".rds",
      full.names = T
    ) %>% lapply(readRDS)
    names(d) <- list.files(here(path), pattern = ".rds") %>%
      substr(21, nchar(.) - 4)
    return(d)
  }
)
names(global_occf) <- superFamilies

# climatologies au niveau mondial ----

# Copie des climatologies nécessaires pour le niveau global
cmd <- "rsync -avuc --delete /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/data_env_prep/data/tidy/clim_global/ /home/borea/Documents/mosceco/r_projects/MOSCECO_L2/habitat_suitability/data/raw/clim_global/"
system(cmd)

# Import des climatologies au niveau global
cgc <- here("data", "raw", "clim_cenfa_clanod", "clims.rds") %>%
  readRDS() %>%
  rast()

species <- #tibble(
  # superFamily = c(rep("Majoidea", 8), rep("Muricoidea", 10)),
  # species     = c(
  c(
    # "Amphithrax_hemphilli",
    # "Macrocoeloma_nodipes",
    # "Mithraculus_coryphe",
    "Mithraculus_forceps",
    # "Mithrax_pleuracanthus",
    # "Omalacantha_bicornuta",
    # "Stenorhynchus_seticornis",
    # "Teleophrys_ruber",
    "Claremontiella_nodulosa",
    # "Coralliophila_galea",
    # "Coralliophila_salebrosa",
    # "Favartia_alveata",
    # "Favartia_varimutabilis",
    # "Phyllonotus_pomum",
    # "Siratus_consuelae",
    # "Stramonita_rustica",
    # "Trachypollia_didyma",
    # "Vasula_deltoidea",
    NULL
  )
# )
clim_global <- mapply(
  \(supfm, spe) {
    cg <- list.files(
      here("data", "raw", "clim_global", supfm, spe), full.names = T
    ) %>%
      lapply(read_csv, show_col_types = FALSE)
    names(cg) <- c("sbt", "so", "sw1", "ww", "hm0")
    cg <- list(cg)
    names(cg) <- spe
    return(cg)
  },
  superFamilies,
  species,
  SIMPLIFY = F,
  USE.NAMES = T
)

# Modification de la forme des climatologies global pour avoir une matrice
clim_global2 <- mapply(
  \(supfm, spe) {

    cg <- clim_global[[supfm]][[spe]]

    # coords <- cg$sbt[, c("x", "y")]

    # uniformisation des stations présentes (problème avec so chais pas pk)
    z <- names(which.min(lapply(cg, nrow) %>% unlist()))
    tbm <- cg[[z]]
    stn <- paste(tbm$x, tbm$y)

    coords <- tbm[, c("x", "y")]

    cg2 <- lapply(
      names(cg),
      \(n) {
        tb <- cg[[n]]

        # uniformisation des stations présentes (problème avec so chais pas pk)
        tb <- if (nrow(tb) > length(stn)) {
          tbi <- tb[paste(tb$x, tb$y) %in% stn, ]
          tbi <- tbi[which(paste(tbi$x, tbi$y) %in% stn), ]
          tbi
        } else {
          tb[which(paste(tb$x, tb$y) %in% stn), ]
        }

        tb <- tb %>%
          select(-c(x, y))
        names(tb) <- names(tb) %>%
          str_split("_") %>%
          lapply(pluck, 2) %>%
          paste(n, sep = ".")

        return(tb)
      }
    )

    cg2 <- cbind(coords, do.call(cbind, cg2)) %>%
      na.omit() %>%
      list()
    names(cg2) <- spe

    return(cg2)
  },
  superFamilies,
  species,
  SIMPLIFY = F,
  USE.NAMES = T
)

# Tant que je n'ai pas refait tourner les codes pour C. nodulosa
names(clim_global2$Muricoidea$Claremontiella_nodulosa) <-
  gsub("NULL", "mean", names(clim_global2$Muricoidea$Claremontiella_nodulosa))

cgc_sub <- mapply(
  \(supfm, spe) {
    tb <- clim_global2[[supfm]][[spe]]
    tb <- tb %>%
      select(names(.)[grepl("stdv|mean", names(.))])
    tbl <- list(tb)
    names(tbl) <- spe
    return(tbl)
  },
  superFamilies,
  species,
  SIMPLIFY = F,
  USE.NAMES = T
)

cgc_sub <- mapply(
  \(supfm, spe) {
    tb <- cgc_sub[[supfm]][[spe]]
    var_col <- usdm::vifstep(as.data.frame(tb %>% na.omit()))@excluded
    tb <- tb[, !names(tb) %in% var_col]
    tbl <- list(tb)
    names(tbl) <- spe
    return(tbl)
  },
  superFamilies,
  species,
  SIMPLIFY = F,
  USE.NAMES = T
)

# Import des climatologies locales ----

# sextant local climatologies
sxt_sub <- climosaic %>%
  terra::subset(names(.)[grepl("ssm|tur|chla|sst", names(.))])
var_col <- usdm::vifstep(as.data.frame(sxt_sub %>% na.omit()))@excluded
sxt_sub <- subset(sxt_sub, names(sxt_sub)[!names(sxt_sub) %in% var_col])

# sextant local means climatologies
sxt_sub_means <- climosaic %>%
  terra::subset(names(.)[grepl("ssm|tur|chla|sst", names(.))]) %>%
  terra::subset(names(.)[grepl("mean", names(.))])
var_col <- usdm::vifstep(as.data.frame(sxt_sub_means %>% na.omit()))@excluded
sxt_sub_means <- subset(
  sxt_sub_means, names(sxt_sub_means)[!names(sxt_sub_means) %in% var_col]
)

# Habitat local spatial raster
hab_sub <-  climosaic %>%
  terra::subset(names(.)[grepl("depth", names(.))])
hab_sub$slope <- terra::terrain(hab_sub)
var_col <- usdm::vifstep(as.data.frame(hab_sub %>% na.omit()))@excluded
hab_sub <- subset(hab_sub, names(hab_sub)[!names(hab_sub) %in% var_col])

# Fermeture des fenêtres externes
for (i in 1:4) { dev.off() }
