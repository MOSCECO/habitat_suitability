# cartes pour le datapaper
length(unique(stations$MTQ$collectEvent)) # 503
# Majoidea ----
stna <- stations$MTQ %>% 
  filter(
    collectEvent %in% (
      species$majo %>%
        filter(country == "MQ") %>% 
        select(collectStation) %>% 
        unique() %>% 
        unlist(use.names = F)
    )
  )
dpth <- bathy$MTQ 
names(dpth) <- c("Longitude", "Latitude", "Depth")

pa <- ggplot() + 
  geom_tile(data = dpth, aes(x = Longitude, y = Latitude, fill = Depth)) + 
  new_scale_fill() +
  geom_sf(data = mq_collec, aes(fill = DESCR)) +
  geom_sf(data = stations$MTQ, shape = 16, size = 3, col = "grey", alpha = 0.3) +
  geom_sf(data = stna, shape = "+", size = 4, col = colors_taxa[[1]]) +
  guides(fill = "none")
nrow(stna) # 203 sampling events
ggexport(
  pa, 
  filename = here("figures", "carte_occurrences_majoidea.png"),
  width  = 3000, 
  height = 2800, 
  res = 400
)

# Muricidae ----
stnu <- stations$MTQ %>% 
  filter(
    collectEvent %in% (
      species$muri %>%
        filter(country == "MQ") %>% 
        select(collectStation) %>% 
        unique() %>% 
        unlist(use.names = F)
    )
  )

pu <- ggplot() + 
  geom_tile(data = dpth, aes(x = Longitude, y = Latitude, fill = Depth)) + 
  new_scale_fill() +
  geom_sf(data = mq_collec, aes(fill = DESCR)) +
  geom_sf(data = stations$MTQ, shape = 16, size = 4, col = "grey", alpha = 0.3) +
  geom_sf(data = stnu, shape = "+", size = 4, col = colors_taxa[[2]]) +
  guides(fill = "none")
nrow(stnu) # 215 sampling events
ggexport(
  pu, 
  filename = here("figures", "carte_occurrences_muricidae.png"),
  width  = 3000, 
  height = 2800, 
  res = 400
)

# number of specimens
nrow(species$muri %>% filter(country == "MQ"))
# number of species
length(species$muri %>% filter(country == "MQ") %>% 
         select(aphiaID) %>% unique() %>% as.vector())