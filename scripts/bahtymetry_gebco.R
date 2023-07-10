# ajout des bathymétrie selon les données GEBCO

bathy_rast <- raster(
  here(
    "data", 
    "raw", 
    "shp", 
    "MNT_FACADE_ANTS_HOMONIM_PBMA", 
    "MNT_FACADE_ANTS_HOMONIM_PBMA", 
    "DONNEES", 
    "MNT_ANTS100m_HOMONIM_WGS84_PBMA_ZNEG.asc"
  )
)

offsets <- 
  list(
    MTQ = c(
      xmin = -0.1, 
      xmax =  0.1,
      ymin = -0.1, 
      ymax =  0.1
    ),
    GLP = c(
      xmin = -0.1, 
      xmax =  0.1,
      ymin = -0.1,
      ymax =  0.1
    )
  )
emprises <- lapply(
  stations, 
  st_bbox
)
emprises <- emprises %>% 
  lapply(
    function(x) x <- x[c(1,3,2,4)]
  )

emprises_off <- mapply(
  function(x, y) z <- x + y,
  emprises, 
  offsets,
  SIMPLIFY = F,
  USE.NAMES = T
)

bathy_crop <- lapply(
  X = emprises_off,
  FUN = function(y) {
    z <- raster::crop(bathy_rast, y = y)
    return(z)
  } 
)

stn_bathy <- lapply(
  islands, 
  function(isl) {
    rval <- raster::extract(bathy_crop[[isl]], stations[[isl]])
    return(stations[[isl]] %>% cbind(depthGebco = rval))
  }
)

lapply(
  stn_bathy, 
  function(sf) {
    print(sf$collectEvent[is.na(sf$depthGebco)])
  }
)

stn_bathy <- lapply(
  islands, 
  function(isl) {
    r <- bathy[[isl]] %>% rasterFromXYZ()
    rval <- raster::extract(r, stations[[isl]])
    # mise à zéro des altitudes positives
    # rval[rval > 0] <- 0
    # toutes les stations en NA seront associées aux profondeurs
    # relevées pendant la mission
    # rval[is.na(rval)] <- stations[[isl]]$depth[is.na(rval)] 
    return(stations[[isl]] %>% cbind(depthGebco = rval))
  }
)
View(stn_bathy$GLP$depthGebco + stn_bathy$GLP$depth %>% as_tibble())
require(ggrepel)
lapply(
  islands, 
  function(isl) {
    ggplot() + 
      geom_sf(data = maps[[isl]]) + 
      geom_sf(data = stn_bathy[[isl]], (aes(col = is.na(depthGebco)))) + 
      ggrepel::geom_label_repel(
        data = stn_bathy[[isl]] %>% 
          filter(is.na(depthGebco)), 
        aes(label = collectEvent, geometry = geometry),
        stat = "sf_coordinates"
      )
    View(stn_bathy[[isl]] %>% filter(is.na(depthGebco)) %>% 
           dplyr::select(collectEvent, depth))
    
  }
)
