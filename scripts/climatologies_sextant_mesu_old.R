# Création de climatologies sur une période donnée qui serviront ensuite à
# créer un rasterStack qui sera nourrit à biomod pour des modèles de niche
source(here::here("scripts", "boot_meSU.R"))

path_climatologies <- here("data", "tidy", "climatology")
makeMyDir(path_climatologies)

# Période de 5 ans : 2012-01-01 à 2017-01-01 
# (demi-décennie des missions scientifiques du Muséum aux Antilles)
# pour les variables qui le peuvent...

my_origin <- "1970-01-01"

# isl <- "GLP"
# varenv <- "sst"

# doMPI initialization
cl <- startMPIcluster()
registerDoMPI(cl)

climatologies_islands <- lapply(
  islands, 
  function(isl) {
    
    print(isl)
    writeLines(c(""), here("data", "tidy", "log.txt"))
    
    ncs       <- sextant[[isl]]
    ncs_brick <- sextant_raster_brick[[isl]]
    
    path_clim_isl <- here(path_climatologies, isl)
    makeMyDir(path_clim_isl)
    
    # itération sur les variables
    climatologies_varenv <- foreach(
      i = 1:length(names(ncs)), 
      .packages = c(
        "here", 
        "raster", 
        "reshape2",
        "sf",
        "terra",
        "tidync", 
        "tidyverse"
      ),
      .export = c(
        "isl", 
        "ncs", 
        "ncs_brick", 
        "makeMyDir", 
        "my_origin", 
        "path_clim_isl", 
        "sextant_raster_brick"
      )
    ) %dopar% {
      varenv <- names(ncs)[i]
      
      borne_inf <- "2012-01-01" %>% as.Date()
      borne_sup <- "2017-01-01" %>% as.Date()
      
      path_clim_isl_vrv <- here(path_clim_isl, varenv)
      makeMyDir(path_clim_isl_vrv)
      
      print(varenv)
      # données interpolées
      nc <- ncs[[varenv]]
      br <- ncs_brick[[varenv]]
      
      # sélection des couches du raster à sélectionner
      # grâce à la plage temporelle définie
      nc_time <- nc$transforms$time
      nc_time$time <- nc_time$time %>% 
        as.Date(origin = my_origin)
      my_slice <- nc_time %>% filter(
        between(time, borne_inf, borne_sup)
      ) %>% 
        select(index) %>% 
        unlist(use.names = F)
      
      my_raster_slice  <- sextant_raster_brick[[isl]][[varenv]][[my_slice]]
      
      # réduction
      
      # Moyenne
      file_name_mean <- paste(
        "climatology", tolower(isl), varenv, "mean", borne_inf, borne_sup,
        sep = "_"
      ) %>% paste0(".tif")
      climatology_mean <- if (
        !file.exists(here(path_clim_isl_vrv, file_name_mean))
      ) {
        print("moyenne")
        calc(my_raster_slice, mean, na.rm = T)
      }
      if (!file.exists(here(path_clim_isl_vrv, file_name_mean))) {
        writeRaster(
          climatology_mean, 
          here(path_clim_isl_vrv, file_name_mean), 
          format="GTiff"
        )
      }
      
      # Médiane
      file_name_medn <- paste(
        "climatology", tolower(isl), varenv, "medn", borne_inf, borne_sup,
        sep = "_"
      ) %>% paste0(".tif")
      climatology_medn <- if (
        !file.exists(here(path_clim_isl_vrv, file_name_medn))
      ) {
        print("median")
        calc(my_raster_slice, median, na.rm = T)
      }
      if (!file.exists(here(path_clim_isl_vrv, file_name_medn))) {
        writeRaster(
          climatology_medn, 
          here(path_clim_isl_vrv, file_name_medn), 
          format="GTiff"
        )
      }
      
      # Écart-type
      file_name_stdv <- paste(
        "climatology", tolower(isl), varenv, "stdv", borne_inf, borne_sup,
        sep = "_"
      ) %>% paste0(".tif")
      climatology_stdv <- if (
        !file.exists(here(path_clim_isl_vrv, file_name_stdv))
      ) {
        print("écart-type")
        calc(my_raster_slice, sd, na.rm = T)
      }
      if (!file.exists(here(path_clim_isl_vrv, file_name_stdv))) {
        writeRaster(
          climatology_stdv, 
          here(path_clim_isl_vrv, file_name_stdv), 
          format="GTiff"
        )
      }
      
      clim_list <- list(
        mean = climatology_mean, 
        medn = climatology_medn, 
        stdv = climatology_stdv
      )
      
      return(clim_list)
      
    }
    
    names(climatologies_varenv) <- names(ncs)
    
    return(climatologies_varenv)
  }
)

closeCluster(cl)
mpi.quit()

# sauvegarde
saveRDS(
  climatologies_islands, 
  here(
    "data", 
    "tidy", 
    paste("climatologies", "sextant", "mean", "medn", "stdv", sep = "_") %>% 
      paste0(".rds")
  )
)
