# raster hybride des salinités selon la profondeur

# importations des rasters de salinité
lapply(
  islands,
  \(isl) {
    lapply(
      c("mean", "medn", "stdv"),
      \(mtd) {
        dpth <- here("data", "tidy", "climatology", isl) %>% 
          list.files(pattern = "depth", full.names = T) %>% 
          list.files(pattern = mtd, full.names = T) %>% 
          rast()
        pc <- here("data", "tidy", "climatology_mesu", isl)
        sor <- pc %>% 
          list.files(pattern = "so[0-9]", full.names = T) %>% 
          lapply(
            \(p) {
              rast(list.files(p, pattern = mtd, full.names = T))
            }
          )
        names(sor) <- pc %>% list.files(pattern = "so[0-9]")
        
        # création d'un raster de profondeurs catégorielles
        # correspondant à quatre tranches bathymétriques
        breaks <- c(
          Inf, 
          -(9.57+25.21)/2, 
          -(25.21+75.81)/2,
          -(75.81+130.6)/2,
          -Inf
        )
        v <- cut(bathy[[isl]]$value, breaks, include.lowest = TRUE)
        v <- factor(v)
        levels(v) <- c("so130", "so77", "so25", "so9")
        b <- bathy[[isl]]
        b$value <- v
        
        # Aggrégation
        so <- do.call(
          rbind, 
          lapply(
            b %>% split(f = b$value),
            \(tby) {
              sal <- unique(tby$value)
              tbx <- sor[[sal]]
              cbind(tby[, -3], value = terra::extract(tbx, tby[, -3])$layer)
            }
          )
        ) %>% as_tibble()
        
        # référentiel
        sosr <- rast(so)
        crs(sosr) <- crs(dpth)
        ext(sosr) <- ext(dpth)
        
        # gros problèmes de résolutions, en attendant de refaire les 
        # interpolations sur la résolution du GEBCO, mise au même format
        # des différentes climatos
        # r01 <- resample(sor$so9, sosr, method = "near")
        # mask <- r01
        # values(mask) <- !(is.na(values(r01)) == is.na(values(sosr)))
        # values(sosr)[values(mask) == 1] <- values(r01)[values(mask) == 1]
        # plot(sosr)
        
        # sosr_resampled <- terra::resample(sosr, rast(dpth))
        
        # sauvegarde
        pc2 <- here(pc, "so")
        makeMyDir(pc2)
        
        terra::writeRaster(
          sosr,
          here(
            pc2, 
            paste(
              "climatology", 
              tolower(isl), 
              "so",
              mtd,
              "2012-01-01",
              "2017-01-01",
              sep = "_"
            ) %>% paste0(".tif")
          ), 
          filetype = "GTiff",
          overwrite = TRUE
        )
      }
    )
  }
)