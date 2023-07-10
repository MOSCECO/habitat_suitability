# Conversion des climatologies en une seule climatologies multi-couches
# format spatRasters

env_vars_spatRaster <- sapply(
  names(env_vars_raster), 
  function(isl) {
    spatrasts <- sapply(
      names(env_vars_raster[[isl]]),
      function(nr) {
        r <- env_vars_raster[[isl]][[nr]]
        return(as(r, "SpatRaster"))
      },
      USE.NAMES = T,
      simplify = F
    )
    return(spatrasts)
  },
  USE.NAMES = T,
  simplify = F
)

# mise à la même résolution
clim  <- lapply(env_vars_spatRaster, rast)

climx_mtq <- rast(
  x          = ext(clim$MTQ), 
  resolution = rep(max(res(clim$GLP)), 2),
  crs        = terra::crs(clim$MTQ)
)
climx_glp <- rast(
  x          = ext(clim$GLP), 
  resolution = rep(max(res(clim$GLP)), 2),
  crs        = terra::crs(clim$MTQ)
)
# Ajustement de la résolution de la Martinique à celle de la Guadeloupe
clim$MTQ <- resample(clim$MTQ, climx_mtq, method = "near")
clim$GLP <- resample(clim$GLP, climx_glp, method = "near")
climosaic_rast <- mosaic(x = clim$GLP, y = clim$MTQ)