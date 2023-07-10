# Regroupement et standardisation des climatologies
env_vars_raster <- lapply(
  islands, 
  function(isl) {
    env_vars <- lapply(
      list.files(here("data", "tidy", "climatology", isl)),
      function(vrv) {
        r <- list.files(
          here("data", "tidy", "climatology", isl, vrv),
          pattern = mtd,
          full.names = T
        ) %>% 
          rast()
        ext(r) <- ext(c(extents[[isl]]$x, extents[[isl]]$y))
        crs(r) <- crs("epsg:4326")
        return(r)
      }
    )
    names(env_vars) <- list.files(here("data", "tidy", "climatology", isl))
    
    # a <- lapply(env_vars, res)
    # b <- paste0(a$chla, collapse = ";")
    # p <- lapply(a, function(x) paste0(x, collapse = ";") != b) %>% unlist()
    # env_resample <- lapply(
    #   env_vars[p], 
    #   resample, 
    #   y = env_vars[!p][[1]],
    #   method = "ngb"
    # )
    # env_vars[p] <- env_resample
    sr0 <- rast(
      nrows = nrow(env_vars$depth),
      ncols = ncol(env_vars$depth),
      crs = "epsg:4326", 
      # resolution = rep(0.001, 2),
      resolution = res(env_vars$depth),
      extent = ext(env_vars$depth)
    )
    env_vars_resample <- names(env_vars) %>% lapply(
      \(nr) {
        srout <- if(nr != "so") {
          # r01 <- rast(env_vars[[nr]]) %>% crop(sr0)
          r01 <- resample(env_vars[[nr]], sr0)
          mask <- r01
          values(mask) <- !(
            is.na(values(mask)) == is.na(values(env_vars$depth))
          )
          values(r01)[values(mask) == 1] <- NA
          r01
        } else {
          env_vars[[nr]]
        }
      }
    )
    names(env_vars_resample) <- names(env_vars)
    
    return(rast(env_vars_resample))
  }
)

# lapply(
#   env_vars,
#   function(v) {
# 
#     # x11()
#     # plot(v)
# 
#     extent(v)
# 
#     # raster::res(v)
# 
#   }
# )

lapply(
  env_vars_raster,
  function(x) {
    x11()
    plot(x)
  }
)

pclim <- here("data", "tidy", "climatologies_spatRaster")
makeMyDir(pclim)

# sauvegarde
mapply(
  \(r, isl) {
    writeRaster(
      r, 
      here(
        pclim, paste("climatologies", tolower(isl), sep = "_") %>% 
          paste0(".tif")
      ), 
      filetype = "GTiff",
      overwrite = TRUE
    )
  },
  env_vars_raster,
  names(env_vars_raster),
  SIMPLIFY = F, 
  USE.NAMES = T
)