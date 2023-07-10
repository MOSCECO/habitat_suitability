hg <- st_read(
  list.files(here("data", "raw", "hab", "GLP"), full.names = T, pattern = "shp")
)
hg <- hg %>% st_crop(st_bbox(maps$GLP))

ggplot() + 
  geom_sf(data = hg, aes(fill = peuplement), col = NA) +
  geom_sf(data = maps$GLP, col = "black", fill = "lightgreen", alpha = 0.2) +
  geom_sf(
    data = spp_sf %>%
      filter(grepl("G", spp_sf$method))
  )

hm <- do.call(
  rbind, 
  list.files(
    here("data", "raw", "hab", "MTQ", "Biocénoses benthiques", "Biocenoses"), 
    full.names = T, 
    pattern = "shp$"
  ) %>% 
  lapply(st_read)
)

ggplot() + 
  geom_sf(data = hm, aes(fill = Biocenoses), col = NA) + 
  geom_sf(data = maps$MTQ, col = "black", fill = "lightgreen", alpha = 0.2)

substm <- do.call(
  rbind, 
  list.files(
    here("data", "raw", "hab", "MTQ", "Substrats", "Substrat"), 
    full.names = T, 
    pattern = "shp$"
  ) %>% 
    lapply(st_read)
)
st_crs(spp_sf) <- st_crs("EPSG:4326")
ggplot() + 
  geom_sf(data = substm, aes(fill = Substrat), col = NA) + 
  geom_sf(data = maps$MTQ, col = "black", fill = "lightgreen", alpha = 0.2) + 
  geom_sf(
    data = spp_sf %>%
      filter(!grepl("G", spp_sf$method))
  )
