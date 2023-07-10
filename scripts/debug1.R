env_vars %>% lapply(ext)
env_vars %>% lapply(res)
env_vars %>% lapply(dim)
a <- env_vars %>% 
  lapply(as.data.frame) %>% 
  lapply(\(x) table(is.na(x)))

# spatial raster données à arranger
srd <- env_vars$chla
# spatial raster modèle
sr0 <- env_vars$depth
sr02 <- rast(
  nrows = nrow(env_vars$depth),
  ncols = ncol(env_vars$depth),
  crs = "epsg:4326", 
  resolution = rep(0.001, 2),
  # resolution = res(env_vars$depth),
  extent = ext(env_vars$depth)
)

# Première pipeline : ré-échantillonnage, puis vérification des données
srd_rs  <- resample(srd, sr0)
srd_rs  <- resample(srd, sr0)
srd_rs2 <- resample(srd, sr02)
sr0_rs  <- resample(sr0, sr02)

# projection
srd_rs2_32620 <- terra::project(srd_rs2, "espg:32620")
crs(sr0_rs) <- st_crs(maps$GLP)$wkt
sr0_rs_32620 <- terra::project(sr0_rs, "EPSG:32620")
maps_proj32620 <- lapply(maps, st_transform, "EPSG:32620")


srd_tb    <- as.data.frame(srd, xy = T, cells = T)
sr0_tb    <- as.data.frame(sr0, xy = T, cells = T)
srd_rs_tb <- as.data.frame(srd_rs, xy = T, cells = T)

dim(srd_tb)
dim(sr0_tb)
dim(srd_rs_tb)

zz <- as.data.frame(z, xy = T)
table(is.na(zz))
zzz <- apply(zz[, -c(1:2)], 2, \(x) {
  return(cbind(zz[, 1:2], value = x))
})
mapply(
  \(tb, n) {
    ggplot() + 
      geom_tile(data = tb, aes(x = x, y = y, fill = is.na(value)))  +
      labs(title = n)
  },
  zzz, 
  names(zzz),
  SIMPLIFY = F, 
  USE.NAMES = T
)


tbd <- as.data.frame(srd, xy = T)
p0 <- ggplot() + 
  geom_raster(data = tbd, 
              aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = maps$MTQ, col = NA, alpha = 0.5, fill = "red")


tbd <- as.data.frame(srd_rs2, xy = T)
p1 <- ggplot() + 
  geom_raster(data = tbd, 
            aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = maps$GLP, col = NA, alpha = 0.5, fill = "red")

tb0 <- as.data.frame(sr0, xy = T)
p2 <- ggplot() + 
  geom_raster(data = tb0, 
              aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = maps$GLP, col = NA, alpha = 0.5, fill = "red")

x11(); p1+p2
tb0 <- as.data.frame(sr0, xy = T)
ggplot() + 
  geom_tile(data = tb0, 
            aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = maps$GLP)

# Visualisations des projections
tb0 <- as.data.frame(sr0_rs_32620, xy = T)
p_sr0_32620 <- ggplot() + 
  geom_sf(data = maps_proj32620$GLP, col = NA, alpha = 0.5, fill = "red") +
  geom_raster(data = tb0, 
              aes(x = x, y = y, fill = layer))
p_sr0_32620 + p_sr0_326202


my_shiftx <- as.vector(ext(sr_bathy))[["xmin"]] - as.vector(ext(sr0))[["xmin"]]
my_shifty <- as.vector(ext(sr_bathy))[["ymin"]] - as.vector(ext(sr0))[["ymin"]]
sr0_shift <- shift(sr0, dx = my_shiftx)#, dy = my_shifty)

tbshift <- as.data.frame(sr0_shift, xy = T)
pshift <- ggplot() + 
  geom_raster(data = tbshift, 
              aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = maps$GLP, col = NA, alpha = 0.5, fill = "red")
p2 + pshift
