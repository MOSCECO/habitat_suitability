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
# varenv <- "hm0"

# doMPI initialization
cl <- startMPIcluster()
registerDoMPI(cl)

climatologies_islands <- lapply(
  islands, 
  function(isl) {
    
    print(isl)
    writeLines(c(""), here("data", "tidy", "log.txt"))
    
    ncs       <- copernicus[[isl]]
    ncs_brick <- copernicus_raster_brick[[isl]]
    
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
        "ncs", "ncs_brick", "path_clim_isl", 
        "copernicus_raster_brick", "isl", "makeMyDir", "my_origin"
      )
    ) %dopar% {
      varenv <- names(ncs)[i]
      # trois années pour les vagues
      borne_inf <- if (varenv %in% c("hm0", "ww", "sw1")) {
        "2019-01-01" %>% as.Date()
      } else {
        "2012-01-01" %>% as.Date()
      }
      borne_sup <- if (varenv %in% c("hm0", "ww", "sw1")) {
        "2022-01-01" %>% as.Date()
      } else {
        "2017-01-01" %>% as.Date()
      }
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
        as.POSIXct(origin = my_origin) %>% 
        as.Date()
      my_slice <- nc_time %>% filter(
        between(time, borne_inf, borne_sup)
      ) %>% 
        select(index) %>% 
        unlist(use.names = F)
      
      my_raster_slice  <- copernicus_raster_brick[[isl]][[varenv]][[my_slice]]
      
      # réduction
      
      # Moyenne
      file_name_mean <- paste(
        "climatology", tolower(isl), varenv, "mean", borne_inf, borne_sup,
        sep = "_"
      # ) %>% paste0(".tif")
      ) %>% paste0(".rds")
      climatology_mean <- if (
        !file.exists(here(path_clim_isl_vrv, file_name_mean))
      ) {
        print("moyenne")
        calc(my_raster_slice, mean, na.rm = T)
      }
      if (!file.exists(here(path_clim_isl_vrv, file_name_mean))) {
        # terra::writeRaster(
        #   climatology_mean, 
        #   here(path_clim_isl_vrv, file_name_mean), 
        #   format="GTiff"
        # )
        saveRDS(
          climatology_mean, 
          here(path_clim_isl_vrv, file_name_mean)
        )
      }
      
      # Médiane
      file_name_medn <- paste(
        "climatology", tolower(isl), varenv, "medn", borne_inf, borne_sup,
        sep = "_"
      # ) %>% paste0(".tif")
      ) %>% paste0(".rds")
      climatology_medn <- if (
        !file.exists(here(path_clim_isl_vrv, file_name_medn))
      ) {
        print("median")
        calc(my_raster_slice, median, na.rm = T)
      }
      if (!file.exists(here(path_clim_isl_vrv, file_name_medn))) {
        # terra::writeRaster(
        #   climatology_medn, 
        #   here(path_clim_isl_vrv, file_name_medn), 
        #   format="GTiff"
        # )
        saveRDS(
          climatology_medn, 
          here(path_clim_isl_vrv, file_name_medn)
        )
      }
      
      # Écart-type
      file_name_stdv <- paste(
        "climatology", tolower(isl), varenv, "stdv", borne_inf, borne_sup,
        sep = "_"
        # ) %>% paste0(".tif")
      ) %>% paste0(".rds")
      climatology_stdv <- if (
        !file.exists(here(path_clim_isl_vrv, file_name_stdv))
      ) {
        print("écart-type")
        calc(my_raster_slice, sd, na.rm = T)
      }
      if (!file.exists(here(path_clim_isl_vrv, file_name_stdv))) {
        # terra::writeRaster(
        #   climatology_stdv, 
        #   here(path_clim_isl_vrv, file_name_stdv), 
        #   format="GTiff"
        # )
        saveRDS(
          climatology_stdv, 
          here(path_clim_isl_vrv, file_name_stdv)
        )
      }
      
      # Quantiles ----
      #   # 1 % ----
      # file_name_qt01 <- paste(
      #   "climatology", tolower(isl), varenv, "qt01", borne_inf, borne_sup,
      #   sep = "_"
      # # ) %>% paste0(".tif")
      # ) %>% paste0(".rds")
      # climatology_qt01 <- if (
      #   !file.exists(here(path_clim_isl_vrv, file_name_qt01))
      # ) {
      #   print("quantile 1%")
      #   calc(my_raster_slice, quantile, ... = list(probs = 0.99), na.rm = T)
      # }
      # if (!file.exists(here(path_clim_isl_vrv, file_name_qt01))) {
      #   # terra::writeRaster(
      #   #   climatology_qt01, 
      #   #   here(path_clim_isl_vrv, file_name_qt01), 
      #   #   format="GTiff"
      #   # )
      #   saveRDS(
      #     climatology_qt01, 
      #     here(path_clim_isl_vrv, file_name_qt01)
      #   )
      # }
      # 
      # # 99 % ----
      # file_name_qt99 <- paste(
      #   "climatology", tolower(isl), varenv, "qt99", borne_inf, borne_sup,
      #   sep = "_"
      # # ) %>% paste0(".tif")
      # ) %>% paste0(".rds")
      # climatology_qt99 <- if (
      #   !file.exists(here(path_clim_isl_vrv, file_name_qt99))
      # ) {
      #   print("quantile 99%")
      #   calc(my_raster_slice, quantile, ... = list(probs = 0.99), na.rm = T)
      # }
      # if (!file.exists(here(path_clim_isl_vrv, file_name_qt99))) {
      #   # terra::writeRaster(
      #   #   climatology_qt99, 
      #   #   here(path_clim_isl_vrv, file_name_qt99), 
      #   #   format="GTiff"
      #   # )
      #   saveRDS(
      #     climatology_qt99, 
      #     here(path_clim_isl_vrv, file_name_qt99)
      #   )
      # }
      # 
      # # 5% ----
      # file_name_qt05 <- paste(
      #   "climatology", tolower(isl), varenv, "qt05", borne_inf, borne_sup,
      #   sep = "_"
      #   # ) %>% paste0(".tif")
      # ) %>% paste0(".rds")
      # climatology_qt05 <- if (
      #   !file.exists(here(path_clim_isl_vrv, file_name_qt05))
      # ) {
      #   print("quantile 5%")
      #   calc(my_raster_slice, quantile, ... = list(probs = 0.05), na.rm = T)
      # }
      # if (!file.exists(here(path_clim_isl_vrv, file_name_qt05))) {
      #   # terra::writeRaster(
      #   #   climatology_qt05, 
      #   #   here(path_clim_isl_vrv, file_name_qt05), 
      #   #   format="GTiff"
      #   # )
      #   saveRDS(
      #     climatology_qt05, 
      #     here(path_clim_isl_vrv, file_name_qt05)
      #   )
      # }
      # 
      # # 95 % ----
      # file_name_qt95 <- paste(
      #   "climatology", tolower(isl), varenv, "qt95", borne_inf, borne_sup,
      #   sep = "_"
      # # ) %>% paste0(".tif")
      # ) %>% paste0(".rds")
      # climatology_qt95 <- if (
      #   !file.exists(here(path_clim_isl_vrv, file_name_qt95))
      # ) {
      #   print("quantile 95%")
      #   calc(my_raster_slice, quantile, ... = list(probs = 0.95), na.rm = T)
      # }
      # if (!file.exists(here(path_clim_isl_vrv, file_name_qt95))) {
      #   # terra::writeRaster(
      #   #   climatology_qt95, 
      #   #   here(path_clim_isl_vrv, file_name_qt95), 
      #   #   format="GTiff"
      #   # )
      #   saveRDS(
      #     climatology_qt95, 
      #     here(path_clim_isl_vrv, file_name_qt95)
      #   )
      # }
      # 
      # # Minima ----
      # file_name_mini <- paste(
      #   "climatology", tolower(isl), varenv, "mini", borne_inf, borne_sup,
      #   sep = "_"
      #   # ) %>% paste0(".tif")
      # ) %>% paste0(".rds")
      # climatology_mini <- if (
      #   !file.exists(here(path_clim_isl_vrv, file_name_mini))
      # ) {
      #   print("quantile 5%")
      #   calc(my_raster_slice, quantile, ... = list(probs = 0.05), na.rm = T)
      # }
      # if (!file.exists(here(path_clim_isl_vrv, file_name_mini))) {
      #   # terra::writeRaster(
      #   #   climatology_mini, 
      #   #   here(path_clim_isl_vrv, file_name_mini), 
      #   #   format="GTiff"
      #   # )
      #   saveRDS(
      #     climatology_mini, 
      #     here(path_clim_isl_vrv, file_name_mini)
      #   )
      # }
      # 
      # # Maxima ----
      # file_name_maxi <- paste(
      #   "climatology", tolower(isl), varenv, "maxi", borne_inf, borne_sup,
      #   sep = "_"
      #   # ) %>% paste0(".tif")
      # ) %>% paste0(".rds")
      # climatology_maxi <- if (
      #   !file.exists(here(path_clim_isl_vrv, file_name_maxi))
      # ) {
      #   print("quantile 95%")
      #   calc(my_raster_slice, quantile, ... = list(probs = 0.95), na.rm = T)
      # }
      # if (!file.exists(here(path_clim_isl_vrv, file_name_maxi))) {
      #   # terra::writeRaster(
      #   #   climatology_maxi, 
      #   #   here(path_clim_isl_vrv, file_name_maxi), 
      #   #   format="GTiff"
      #   # )
      #   saveRDS(
      #     climatology_maxi, 
      #     here(path_clim_isl_vrv, file_name_maxi)
      #   )
      # }
      
      clim_list <- list(
        mean = climatology_mean, 
        medn = climatology_medn, 
        stdv = climatology_stdv#, 
        # qt01 = climatology_qt01,
        # qt05 = climatology_qt05,
        # qt95 = climatology_qt95,
        # qt99 = climatology_qt99, 
        # mini = climatology_mini, 
        # maxi = climatology_maxi
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
    paste(
      "climatologies", 
      "mean", 
      "medn",
      "stdv",
      # "qt01-05-95-99",
      # "maxi", 
      # "mini",
      sep = "_"
    ) %>% 
      paste0(".rds")
  )
)