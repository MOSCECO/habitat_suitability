# valeurs de profondeurs originales
pgp <- ggplot() + 
  geom_tile(data = bathy$GLP, aes(x = x, y = y, fill = value)) +
  geom_sf(data = maps$GLP, col = NA, alpha = 0.5, fill = "red") +
  scale_fill_gradient(na.value = "white")
pmq <- ggplot() + 
  geom_tile(data = bathy$MTQ, aes(x = x, y = y, fill = value)) +
  geom_sf(data = maps$MTQ, col = NA, alpha = 0.5, fill = "red") + 
  scale_fill_gradient(na.value = "white")
pmq + p0 + p2 
# Traduite en spatRaster
sr_bathy <- rast(bathy$MTQ)
crs(sr_bathy) <- "EPSG:4326"
f <- sr_bathy %>% as.data.frame(xy = T)

pMQ <- ggplot() + 
  geom_tile(data = f, aes(x = x, y = y, fill = value)) +
  geom_sf(data = maps$MTQ, col = NA, alpha = 0.5, fill = "red")

sr_bathy <- rast(bathy$GLP)
crs(sr_bathy) <- "EPSG:4326"
f <- sr_bathy %>% as.data.frame(xy = T)

pGP <- ggplot() + 
  geom_tile(data = f, aes(x = x, y = y, fill = value)) +
  geom_sf(data = maps$GLP, col = NA, alpha = 0.5, fill = "red")

x11() ; (pMQ + pGP) / (pmq + pgp)
# effet du ré-échantillonnage
sr_bathy_rs <- resample(sr_bathy, sr02)
f_rs <- sr_bathy_rs %>% as.data.frame(xy = T)
pgp_rs <- ggplot() + 
  geom_tile(data = f_rs, aes(x = x, y = y, fill = value)) +
  geom_sf(data = maps$GLP, col = NA, alpha = 0.5, fill = "red")
pgp_rs + pgp
# léger décalage

# valeurs des netcdf
nc <- tidync(
  here(
    "data", "tidy", "nc", "GLP", "sextant", 
    "interpolation_complete_glp_sextant_chla.nc"
  )
)
srnc <- rast(
  here(
    "data", "tidy", "nc", "GLP", "sextant", 
    "interpolation_complete_glp_sextant_chla.nc"
  )
)
ooe <- lapply(srnc, ext)
oee2 <- lapply(ooe, as.vector)
oee3 <- do.call(rbind, oee2)
apply(oee3, 2, unique)
rnc <- nc %>% hyper_filter(time = time < 12054) %>% hyper_tibble()
ggplot() + 
  geom_tile(data = rnc, aes(x = longitude, y = latitude, fill = modis)) +
  geom_sf(data = maps$GLP, col = NA, alpha = 0.2, fill = "red")

nc <- tidync(
  here(
    "data", "tidy", "nc", "MTQ", "sextant", 
    "interpolation_complete_mtq_sextant_chla.nc"
  )
)
rnc <- nc %>% hyper_filter(time = time < 12054) %>% hyper_tibble()
ggplot() + 
  geom_tile(data = rnc, aes(x = longitude, y = latitude, fill = modis)) +
  geom_sf(data = maps$MTQ, col = NA, alpha = 0.2, fill = "red")
