# ajout des profondeurs comme d'une climatologie

ggplot() + 
  geom_tile(data = bathy$GLP, aes(x = x, y = y, fill = value))

rs <- bathy %>% lapply(rasterFromXYZ)
lapply(
  names(rs), 
  function(isl) {
    r <- rs[[isl]]
    path_depth <- here("data", "tidy", "climatology", isl, "depth")
    makeMyDir(path_depth)
    writeRaster(
      r, 
      here(
        path_depth, paste0("depths_mean_gebco_", tolower(isl)) %>% 
          paste0(".tif")
      ),
      format = "GTiff"
    )
    writeRaster(
      r, 
      here(
        path_depth, paste0("depths_medn_gebco_", tolower(isl)) %>% 
          paste0(".tif")
      ),
      format = "GTiff"
    )
    writeRaster(
      r, 
      here(
        path_depth, paste0("depths_stdv_gebco_", tolower(isl)) %>% 
          paste0(".tif")
      ),
      format = "GTiff"
    )
  }
)
