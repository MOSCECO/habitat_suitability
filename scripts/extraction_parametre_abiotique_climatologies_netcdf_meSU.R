# extraction des données environnementales depuis les netcdf complet générés
# par les interpolations pour chaque évènement de collecte dans le fichiers
# des stations. 


# doMPI initialization
cl <- startMPIcluster()
registerDoMPI(cl)

wdw <- 15

matrices_isl_varenv <- lapply(
  islands, 
  function(isl) {
    
    print(isl)
    writeLines(c(""), here("log.txt"))
    
    stn    <- stations[[isl]]
    ncs    <- copernicus[[isl]]
    
    # itération sur les variables
    varenv_valeurs <- lapply(
      names(ncs),
      function(varenv) {
        nc <- ncs[[varenv]]
        # min(nc$transforms$time$time) %>% 
        #   as.POSIXct(origin = "1970-01-01") %>% 
        #   as.Date()
        # max(nc$transforms$time$time) %>% 
        #   as.POSIXct(origin = "1970-01-01") %>% 
        #   as.Date()
        # itération sur chaque évènement de collectes
        # stn <- stn[sample(1:nrow(stn), 2), ]
        stn <- stn[c(1:2), ]
        print(varenv)
        
        stn_valeurs <- foreach(
          i = 1:nrow(stn), 
          .packages = c(
            "raster", 
            "reshape2",
            "sf",
            "tidync", 
            "tidyverse"
          ),
          .export = c("isl", "wdw", "stn")
        ) %dopar% {
          sink("log.txt", append = T)
          print(
            paste0(i, "/", nrow(stn), " of ", varenv, " from ", isl)
          )
          # filtre de la date
          my_origin <- "1970-01-01"
          
          # date de l'évènement de collecte au bon format
          # les sextant sont au format as.Date %>% as.numeric() 
          # (nombre de jours)
          # les copernicus sont au format as.POSIXct() %>% as.numeric()
          # (nombres de secondes)
          my_date <- stn$eventDate[i] %>% 
            as.POSIXct(format = "%d/%m/%Y", tz = "GMT") %>% 
            as.Date()
          
          # bornes de l'intervalle de temps à filtrer du netcdf
          my_date_up <- (my_date + wdw) %>% 
            as.POSIXct() %>% 
            as.numeric()
          my_date_dw <- (my_date - wdw) %>% 
            as.POSIXct() %>% 
            as.numeric()
          
          # filtre du netcdf puis conversion en tibble
          tb <- nc %>% 
            hyper_filter(
              time = between(time, my_date_dw, my_date_up)
            ) %>% 
            hyper_tibble() %>% 
            mutate(
              time = time %>% 
                as.POSIXct(origin = "1970-01-01") %>% 
                as.Date()
            ) %>% 
            split(f = .$time)
          
          # création d'un raster et extraction de la valeur pour 
          # la station
          r    <- lapply(
            tb, function(x) {
              raster::rasterFromXYZ(x[, c(2, 3, 1)])
            }
          )
          vals <- lapply(r, function(y) raster::extract(y, stn[i, ]))
          # climatologie ----
          # période considérée
          timespan <- names(tb) %>% paste(collapse = "\n")
          n_days   <- length(timespan)
          # moyenne de toutes les valeurs de la période considérée
          val <- vals %>% unlist() %>% mean()
          std <- vals %>% unlist() %>% sd()
          
          out <- tibble(
            collectEvent = stn$collectEvent[i],
            eventDate    = my_date,
            timespan     = timespan,
            mean_value   = val,
            stdv_value   = std
          )
        }
      }
    )
    names(varenv_valeurs) <- names(ncs)
    out_tibbles <- lapply(varenv_valeurs, function(x) do.call(rbind, x))
    return(out_tibbles)
  }
)

# sauvegarde
saveRDS(
  matrices_isl_varenv, 
  here(
    "data", 
    "tidy", 
    paste("collectEvent", "climatology", "values", sep = "_") %>% 
      paste0(".rds")
  )
)

# formatage et sauvegarde
matrices_isl_varenv_format <- lapply(
  islands,
  function(isl) {
    d <- matrices_isl_varenv[[isl]]
    min_tbs <- lapply(
      names(d),
      function(varenv) {
        v <- d[[varenv]]
        my_labels <- paste(c("mean", "stdv"), varenv, sep = "_")
        my_labels_pos <- which(grepl("value", names(v)))
        names(v)[my_labels_pos] <- my_labels
        return(v %>% dplyr::select(c(1, all_of(my_labels_pos))))
      }
    )
    tb_out <- Reduce(left_join, min_tbs)
    tb_out <- tb_out[, sort(names(tb_out))]
    return(tb_out)
  }
)

saveRDS(
  matrices_isl_varenv_format, 
  here(
    "data", 
    "tidy", 
    paste("collectEvent", "matrices", "climatology", "values", sep = "_") %>% 
      paste0(".rds")
  )
)

closeCluster(cl)
mpi.quit()