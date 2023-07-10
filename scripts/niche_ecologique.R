# Niche écologique des espèces communes aux deux îles

path_fig_niches <- here("figures", "niche_obs")
makeMyDir(path_fig_niches)

species_sf <- species
species_sf <- lapply(
  species_sf,
  function(tb) {
    stn <- do.call(rbind, stations)
    out <- tb %>% 
      left_join(
        stn %>% 
          st_drop_geometry() %>% 
          select(collectStation = collectEvent, depth_mnhn = depth)
      )
  }
)
species_sf <- lapply(
  species_sf, 
  st_as_sf, 
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = wgs
)

# environement / occurrences
eo <- lapply(
  islands, 
  function(isl) {
    isl2 <- ifelse(isl == "GLP", "GP", "MQ")
    lapply(
      species_sf, 
      function(sp) {
        out <- terra::extract(
          env_vars_raster[[isl]],
          sp %>% filter(country == isl2)
        ) %>% as_tibble()
        out$depth[out$depth > 0] <- 0 # Mise à zéro des valeurs positives
        # remplacement des valeurs nulles de salinité à faible profondeur
        out$so[is.na(out$so)] <- out$so9[is.na(out$so)]
        out <- out %>% select(-so9)
        return(out)
      }
    )
  }
)

# ajouter la profondeur du Muséum aux profondeurs du gebco
species_sf <- sapply(
  islands, 
  function(isl) {
    isl2 <- ifelse(isl == "GLP", "GP", "MQ")
    out <- lapply(
      taxa, 
      function(taxon) {
        tb1 <- species_sf[[taxon]] %>% filter(country == isl2)
        tb2 <- eo[[isl]][[taxon]]
        tb3 <- tb1 %>% cbind(tb2)
        tb3$depth[is.na(tb3$depth)] <- -tb3$depth_mnhn[is.na(tb3$depth)]
        return(tb3)
      }
    )
    names(out) <- taxa
    return(out)
  },
  USE.NAMES = T, 
  simplify = F
)

# retournement de l'objet
sp_common <- list(
  muri = rbind(species_sf$GLP$muri, species_sf$MTQ$muri),
  majo = rbind(species_sf$GLP$majo, species_sf$MTQ$majo)
) %>% 
  lapply(st_drop_geometry)

sp_common <- lapply(
  sp_common, 
  \(tb) {
    tb %>% 
      add_column(
        genus = str_split(tb$scientificName, " ") %>% 
          lapply(pluck, 1) %>% 
          unlist()
      ) %>% 
      select(occurrenceID:family, genus, everything())
  }
)

vec_varenv <- c(
  "depth", "sst", "sbt", 
  "hm0",   "ww",  "sw1", 
  "chla",  "ssm", "tur", 
  "so"
)

lapply(
  taxa, 
  \(taxon) {
    
    superFamily <- switch(
      taxon, majo = superFamilies[[1]], muri = superFamilies[[2]]
    )
    
    path_fn_superFam <- here(path_fig_niches, superFamily)
    makeMyDir(path_fn_superFam)
    
    tbs_fam <- split(sp_common[[taxon]], sp_common[[taxon]]$family)
    
    lapply(
      names(tbs_fam),
      \(fam) {
        path_fnsf_fam <- here(path_fn_superFam, fam)
        makeMyDir(path_fnsf_fam)
        
        tb_fam <- tbs_fam[[fam]]
        file_name <- here(
          path_fnsf_fam,
          paste("observed", "ecological", "niche", fam, sep = "_") %>% 
            paste0(".png")
        )
        ecologicalNicheObserved(tb_fam, vec_varenv, file_name, "family")
        
        tbs <- split(sp_common[[taxon]], sp_common[[taxon]]$scientificName)
        
        lapply(
          names(tbs),
          \(sn) {
            tb <- tbs[[sn]] 
            file_name <- here(
              path_fnsf_fam,
              paste(
                "observed", "ecological", "niche", 
                tolower(gsub(" ", "_", sn)), sep = "_") %>% 
                paste0(".png")
            )
            ecologicalNicheObserved(
              tb, vec_varenv, file_name, "scientificName"
              )
            
          }
        )
        
        
      }
    )
  }
)
