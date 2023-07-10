# extraction des données environnementales depuis les netcdf complet générés
# par les interpolations pour chaque évènement de collecte dans le fichiers
# des stations. 

test <- lapply(
  islands, 
  function(isl) {
    
    stn    <- stations[[isl]]
    ncs    <- copernicus[[isl]]
    
    # itération sur les variables
    varenv_valeurs <- lapply(
      names(ncs),
      function(varenv) {
        nc <- ncs[[varenv]]
        min(nc$transforms$time$time) %>% 
          as.POSIXct(origin = "1970-01-01") %>% 
          as.Date()
        max(nc$transforms$time$time) %>% 
          as.POSIXct(origin = "1970-01-01") %>% 
          as.Date()
        # itération sur chaque évènement de collectes
        stn2 <- stn[360:370, ]
        stn_valeurs <- lapply(
          1:nrow(stn2), 
          function(i) {
            return(
              tryCatch(
                {
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
                  my_date_up <- (my_date + 1) %>% as.POSIXct() %>% as.numeric()
                  my_date_dw <- (my_date - 1) %>% as.POSIXct() %>% as.numeric()
                  
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
                    )
                  
                  bool <- my_date %in% unique(tb$time) 
                  
                  tb <- if (bool) {
                    tb %>% filter(., time == my_date)
                  } else {
                    tb %>% split(f = tb$time)
                  }
                  
                  # création d'un raster et extraction de la valeur pour la station
                  r <- if (bool) {
                    rasterFromXYZ(tb[, c(2, 3, 1)])
                  } else {
                    lapply(
                      tb, 
                      function(x) rasterFromXYZ(x[, c(2, 3, 1)])
                    )
                  }
                  # plot(r)
                  val <- if (bool) {
                    raster::extract(r, stn[i, ])
                  } else {
                    vals <- lapply(
                      r, 
                      function(y) raster::extract(y, stn[i, ])
                    )
                    mean(vals)
                  }
                  
                  t <- if (!is.null(unique(tb$time))) {
                    paste(unique(tb$time), collapse = "\n")
                  } else {
                    NA
                  }
                  
                  out <- tibble(
                    date0 = my_date,
                    date1 = t,
                    value = val
                  )
                  
                  return(out)
                }, 
                error = function(e) {
                  return(
                    tibble(
                      date0 = my_date,
                      date1 = NA, 
                      value = NA
                    )
                  )
                }
              )
            )
          }
        )
        return(stn_valeurs)
      }
    )
    return(do.call(cbind, varenv_valeurs))
  }
)

# 2023-02-06
# Code pour tenter d'attribuer la donnée journalière à chaque évènement de 
# collecte. Problème rencontré jusqu'à présent : erreur d'attribution quand 
# le jour manque. Tentative d'implémenter TryCatch pour attribuer les 
# stations sans problème puis faire une deuxième boucle pour élargir
# la fenêtre de prise de la donnée et moyenner pour le jour considéré. 

# obsolète : on fait de la climatologie, c'est trop fin, pas besoind d'avoir
# le jour même, on fait donc des moyennages. 