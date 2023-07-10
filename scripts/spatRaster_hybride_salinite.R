# raster hybride des salinités selon la profondeur

# saucissonage des rasters de salinité
hybrid_salinity_rasters <- lapply(
  islands, 
  \(isl) {
    sr <- climatologies[[isl]]
    sr_outs <- lapply(
      mtds,
      \(mtd) {
        # sélection des rasters de salinité
        my_mtds <- names(sr)[-length(names(sr))] %>% 
          str_split(pattern = "\\.") %>% 
          lapply(pluck, 1) %>% 
          unlist()
        my_vrvs <- names(sr)[-length(names(sr))] %>% 
          str_split(pattern = "\\.") %>% 
          lapply(pluck, 2) %>% 
          unlist()
        my_sbst <- names(sr)[grepl(mtd, my_mtds) & grepl("so[0-9]", my_vrvs)]
        sor <- sr[[my_sbst]]
        
        # traitement pour créer un raster hybride so selon la bathymétrie
        dpth  <- climatologies[[isl]]$depth
        bathy <- as.data.frame(dpth, xy = T)
        # création d'un raster de profondeurs catégorielles
        # correspondant à quatre tranches bathymétriques
        breaks <- c(
          Inf, 
          -(9.57+25.21)/2, 
          -(25.21+75.81)/2,
          -(75.81+130.6)/2,
          -Inf
        )
        
        v <- cut(bathy$depth, breaks, include.lowest = TRUE)
        v <- factor(v)
        levels(v) <- c("so130", "so77", "so25", "so9")
        b <- bathy
        b$value <- v
        bs <- b %>% split(f = b$value)
        
        # Aggrégation
        so <- do.call(
          rbind, 
          lapply(
            bs,
            \(tby) {
              sal <- paste(mtd, unique(tby$value), sep = ".")
              tbx <- sor[[sal]]
              cbind(
                tby[, -c(3,4)], 
                value = terra::extract(tbx, tby[, -c(3,4)])[[sal]]
              )
            }
          )
        ) %>% as_tibble()
        
        # référentiel
        sosr <- rast(so)
        crs(sosr) <- crs(dpth)
        names(sosr) <- paste(mtd, "so", sep = ".")
        
        return(sosr)
        
      }
    )
  }
)

# stacking
hybrid_salinity_rasters <- lapply(
  hybrid_salinity_rasters, 
  \(x) {
    Reduce(c, x)
  }
)
lapply(hybrid_salinity_rasters, \(x) {x11(); plot(x)})

# suppression des rasters non-hybrides de salinité dans le fichier initial
my_sbst <- names(climatologies$GLP)[!grepl("so[0-9]", names(climatologies$GLP))]
climatologies_updated <- mapply(
  \(x, y) {
    z <- c(x[[my_sbst]], y)
    return(z[[sort(names(z))]])
  },
  climatologies,
  hybrid_salinity_rasters,
  SIMPLIFY = F, 
  USE.NAMES = T
)

# sauvegarde
pclim <- here("data", "tidy", "climatologies_spatRaster")
makeMyDir(pclim)

mapply(
  \(sr, isl) {
      writeRaster(
        sr, 
        here(
          pclim, 
          paste("climatologies", "updated", "so",tolower(isl), sep = "_") %>% 
            paste0(".tif")
        ), 
        filetype = "GTiff",
        overwrite = TRUE
      )
  },
  climatologies_updated,
  names(climatologies_updated),
  SIMPLIFY = F, 
  USE.NAMES = T
)