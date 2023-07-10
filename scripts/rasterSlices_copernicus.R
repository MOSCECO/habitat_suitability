# Création de climatologies sur une période donnée qui serviront ensuite à
# créer un rasterStack qui sera nourrit à biomod pour des modèles de niche

path_climatologies <- here("data", "tidy", "climatology")
makeMyDir(path_climatologies)

# Période de 5 ans : 2012-01-01 à 2017-01-01 
# (demi-décennie des missions scientifiques du Muséum aux Antilles)
# pour les variables qui le peuvent...

my_origin <- "1970-01-01"

# isl <- "GLP"
# varenv <- "hm0"

climatologies_islands <- lapply(
  islands, 
  function(isl) {
    
    print(isl)
    ncs       <- copernicus[[isl]]
    ncs_brick <- copernicus_raster_brick[[isl]]
    
    path_clim_isl <- here(path_climatologies, isl)
    makeMyDir(path_clim_isl)
    
    # itération sur les variables
    climatologies_varenv <- lapply(
      names(ncs),
      function(varenv) {
        
        # trois années pour les vagues
        borne_inf <- if (varenv %in% c("hm0", "ww", "sw1")) {
          "2019-01-01" %>% as.Date()
        } else {
          "2012-01-01" %>% as.Date()
        }
        borne_sup <- if (varenv %in% c("hm0", "ww", "sw1")) {
          "2019-01-08" %>% as.Date()
        } else {
          "2012-01-08" %>% as.Date()
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
        
        file_name_mean <- paste(
          "rasterSlice", tolower(isl), varenv, borne_inf, borne_sup,
          sep = "_"
        ) %>% paste0(".tif")
        
        terra::writeRaster(
          my_raster_slice, 
          here(path_clim_isl, file_name_mean), 
          format="GTiff"
        )
        
        return(my_raster_slice)
        
      }
    )
    
    names(climatologies_varenv) <- names(ncs)
    
    return(climatologies_varenv)
  }
)

# sauvegarde
saveRDS(
  climatologies_islands, 
  here(
    "data", 
    "tidy", 
    paste(
      "rasterSlice", "copernicus", sep = "_"
    ) %>% 
      paste0(".rds")
  )
)