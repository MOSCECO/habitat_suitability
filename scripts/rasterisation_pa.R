# rasterisation des présences-absences pour éviter les superposition des 
# absences et présences

pa_all_species <- pa

# sélection des espèces avec assez d'occurrences
pan <- lapply(
  pa, 
  \(tb) {
    nrow(tb %>% filter(individualCount > 0))
  }
) %>% unlist(use.names = F)

pa <- pa_all_species[pan >= 30]

# rasterisation
pa <- lapply(
  pa, 
  \(tb) {
    bn <- tb$scientificName %>% unique()
    print(bn)
    isl <- tb$ISL %>% unique()
    
    # ajout des missions scientifiques
    tb$expedition[is.na(tb$expedition)] <- ifelse(
      grepl("CP|DW", tb$method[is.na(tb$expedition)]),
      "KARUBENTHOS 2",
      NA
    )
    tb$expedition[is.na(tb$expedition)] <- ifelse(
      grepl("G", tb$method[is.na(tb$expedition)]),
      "KARUBENTHOS 2012",
      "MADIBENTHOS"
    )
    
    # On ignore la mission Karubenthos 2 qui présente un biote très différent 
    # de celui de la côte... mais que pour les absences ? Sûrement un biais là.
    tb <- tb %>% 
      filter(expedition != "KARUBENTHOS 2")
    
    r <- terra::rasterize(
      tb %>% dplyr::select(decimalLongitude, decimalLatitude) %>% as.matrix(),
      climosaic, 
      values = ifelse(
        tb %>% dplyr::select(individualCount) > 0, 1, 0
      ) %>% as.vector(), 
      fun = max
    )
    names(r) <- "individualCount"
    # x11() ; plot(r)
    return(r)
  }
)

# Nombre d'occurrences perdues
pa_df <- lapply(pa, as.data.frame, xy = T)
a <- lapply(pa_df, \(tb) nrow(tb %>% filter(individualCount > 0))) %>% unlist()
b <- lapply(
  pa_all_species[pan >= 30], 
  \(tb) nrow(tb %>% filter(individualCount > 0))
) %>% 
  unlist()
a-b

# nouveau filtre pour ne retenir que les espèces à 30+ occurrences
lapply(pa_df, \(tb) nrow(tb %>% filter(individualCount > 0))) %>% 
  unlist() > 30
pa_df <- pa_df[lapply(pa_df, \(tb) nrow(tb %>% filter(individualCount > 0))) %>% 
  unlist() > 30]