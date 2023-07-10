r <- env_vars_spatRaster$GLP$chla
spp_sf2 <- spp_sf %>% st_crop(ext(r))
i1 <- ggplot() + 
  geom_raster(data = as.data.frame(r, xy = T), aes(x, y, fill = chla)) + 
  geom_sf(data = spp_sf2)

i2 <- ggplot() + 
  geom_raster(data = bathy$GLP, aes(x, y, fill = value))
i1 + i2

e1 <- ext(rast(bathy$GLP))
e2 <- ext(ext(r))
ve1 <- as.vector(ext(rast(bathy$GLP)))
ve2 <- as.vector(ext(ext(r)))

my_shift <- (ve2[[2]] - ve2[[1]] - (ve1[[2]] - ve1[[1]])) / 2

ggplot() + 
  geom_raster(
    data = as.data.frame(terra::shift(r, dx = 0.003), xy = T), 
    aes(x, y, fill = chla)
  ) + 
  geom_sf(data = spp_sf2) +
  geom_raster(data = bathy$GLP, aes(x, y), fill = "red", alpha = 0.4) + 
  geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5)


m <- terra::extract(
  climosaic_pca_resample, cells(pa$`Claremontiella nodulosa`, 1
  )$individualCount, xy = T)
ggplot() + 
  geom_raster(
    data = as.data.frame(climosaic_pca_resample, xy = T), 
    aes(x, y, fill = PC1)
  ) + 
  geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5) + 
  geom_point(data= m, aes(x, y, col = is.na(PC1)))

require(ggnewscale)
ggplot() + 
  geom_tile(
    data = as.data.frame(
      climosaic_pca_resample %>% 
        crop(y = e1), 
      xy = T
    ),
    aes(x, y, fill = PC1)
  ) +
  geom_sf(data = maps$GLP, col = NA, fill = "lightgreen", alpha = 0.5) +
  new_scale_fill() + 
  geom_tile(
    data = as.data.frame(
      pa$`Claremontiella nodulosa` %>% 
        crop(y = e1),
      xy = T), 
    aes(x, y, fill = factor(individualCount))
  ) + 
  scale_fill_manual(values = c("pink", "red"))

