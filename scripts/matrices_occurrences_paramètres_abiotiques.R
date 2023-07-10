# Matrices occurrences / paramètres abiotiques

values_varenvs_tax <- lapply(
  names(species), 
  function(tax) {
    print(tax)
    tb <- species[[tax]]
    values_varenvs <- lapply(
      names(env$GLP),       
      function(varenv) {
        print(varenv)
        values_varenv <- lapply(
          1:nrow(tb),
          function(i) {
            print(paste0(i, "/", nrow(tb), " from ", varenv, " for ", tax))
            r <- tb[i, ]
            date_occ <- r$eventDate %>% 
              as.POSIXct(format = "%d/%m/%Y", tz = "GMT") %>% 
              as.Date()
            sttn_occ <- r$collectStation
            coun_occ <- ifelse(r$country == "GP", "GLP", "MTQ")
            
            mini_env <- env[[coun_occ]][[varenv]] %>% 
              filter(collect == sttn_occ & as.Date(time) == date_occ) %>% 
              select(value)
            return(mini_env)
          }
        )
        return(values_varenv)
      }
    )
    return(values_varenvs)
  }
)

names(values_varenvs_tax) <- taxa
values_varenvs_tax2 <- values_varenvs_tax %>% 
  lapply(
    function(l) {
      names(l) <- names(env$GLP)
      return(l)
    }
  )
saveRDS(values_varenvs_tax2, 
        here("data", "analysis", "valeurs_varenv_comme_sp_threshold.rds"))
