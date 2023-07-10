# Détermination des variables environnementales non-corrélées

# Extraction des variables pour chaque évènement de collecte à partir des 
# climatologies

mtd <- "mean"

stations_gebco <- lapply(
  islands, 
  \(isl) {
    stn <- stations[[isl]]
    path_clim <- here("data", "tidy", "climatology", isl)
    mat_env <- Reduce(
      left_join, 
      lapply(
        list.files(path_clim),
        \(ev) { # environnemental variable
          r <- list.files(
            here(path_clim, ev), pattern = mtd, full.names = T
          ) %>% rast()
          out <- terra::extract(r, stn)
          names(out)[2] <- ev
          return(out)
        }
      )
    )
    out <- mat_env %>% 
      select(-ID) %>% 
      cbind(depth_mnhn = stn[["depth"]])
    out$depth[out$depth > 0] <- 0
    out$depth[is.na(out$depth)] <- -out$depth_mnhn[is.na(out$depth)]
    names(out)[which(names(out) == "depth")] <- "gebco"
    return(
      stn %>% 
        cbind(out %>% select(-depth_mnhn))
    )
  }
)
stations_env <- lapply(stations_gebco, select, -gebco)
# on enlève Karubenthos 2 pour ne considérer que les missions côtières
stations_env <- lapply(stations_env, filter, survey != "KARUBENTHOS 2")

# Identification des variables colinéaires
data_colin <- stations_env %>% append(list(ANT = do.call(rbind, stations_env)))
x11()
par(mfrow = c(1,3))
cplots <- lapply(
  data_colin,
  \(tb) {
    p <- cor(
      tb %>% 
        st_drop_geometry() %>% 
        select(depth, 14:25),
      method = "spearman"
    ) %>% corrplot::corrplot()
  }
)
# dev.off()
# pca
pcas <- lapply(
  data_colin,
  \(tb) {
    d <- tb %>% 
      st_drop_geometry() %>% 
      select(depth, 14:25)
    row.names(d) <- tb$collectEvent
    
    pca <- PCA(d, scale.unit = T, graph = F)
    
    fviz_pca_var(pca, col.var = "cos2") + 
      scale_color_viridis_c()
    
  }
)
x11()
Reduce(`+`, pcas)

# variance inflation factor
vifs <- lapply(
  data_colin,
  \(tb) {
    p <- tb %>% 
      st_drop_geometry() %>% 
      select(depth, 14:25)
    return(usdm::vifstep(p)@excluded)
  }
)

# Qualitativement : 
# - il faut aggréger les vagues entre elles : on ne retient que hm0
# - il faut aggréger les particules entre elles : on ne retient que chla
# - Pas de relations de corrélation entre températures et profondeur, 
# très bizarre : parce qu'on ne considère que les stations côtières ?
# - différences de tendances selon les îles...
# Pour les deux îles : Chlorophylle a, SST, SO9 et Depth
# Identification des variables colinéaires
v <- c("depth", "chla", "sst")
x11()
par(mfrow = c(1,3))
cplots <- lapply(
  data_colin,
  \(tb) {
    p <- cor(
      tb %>% 
        st_drop_geometry() %>% 
        select(all_of(v)),
      method = "spearman"
    ) %>% corrplot::corrplot()
  }
)
# dev.off()
# pca
pcas <- lapply(
  data_colin,
  \(tb) {
    d <- tb %>% 
      st_drop_geometry() %>% 
      select(all_of(v))
    row.names(d) <- tb$collectEvent
    
    pca <- PCA(d, scale.unit = T, graph = F)
    
    fviz_pca_var(pca, col.var = "cos2") + 
      scale_color_viridis_c()
    
  }
)
x11()
Reduce(`+`, pcas)

