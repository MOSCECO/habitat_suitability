# niche écologique observée au niveau mondial de Claremontiella nodulosa

env <- global_clim$Muricoidea$Claremontiella_nodulosa
# différences dans les jeux d'occurrences utilisés
stations_clim <- env %>% lapply(
  \(tb) {
    stn <- paste(tb$x, tb$y)
    return(stn)
  }
)
pmin <- which(stations_clim %>% lapply(length) %>% unlist() %in% 
                min(stations_clim %>% lapply(length) %>% unlist()))
pmax <- which(stations_clim %>% lapply(length) %>% unlist() %in% 
                max(stations_clim %>% lapply(length) %>% unlist()))[1]
# stations_clim[[pmax]] %in% stations_clim[[pmin]]
f <- stations_clim[[pmin]] %>% factor()
levels(f)  <- sprintf("%03d", 1:nlevels(f))
global_stn <- stations_clim[[pmin]] %>% 
  cbind(as.character(f)) %>% 
  as_tibble()
names(global_stn) <- c("stn_dec", "stn")
env2 <- lapply(
  env, 
  \(tb) {
    tb$stn_dec <- paste(tb$x, tb$y)
    tbout <- tb %>% 
      left_join(global_stn[!duplicated(global_stn), ], by = "stn_dec")
    return(tbout[!is.na(tbout$stn), ])
  }
)

stations_clim %>% lapply(
  \(stn) {
    stn %>% 
      unique() %>% 
      length()
  }
)

lapply(env2, dim)
envbind <- env2[[1]] %>% 
  dplyr::select(x, y, stn, bottomt = value) %>% 
  cbind(
    do.call(cbind, env2[-1]) %>% 
      dplyr::select(., which(!grepl("x|y|stn", names(.))))
  )
names(envbind)[grepl(".value", names(envbind))] <- names(envbind)[
  grepl(".value", names(envbind))
] %>% substr(1, nchar(.) - 6)

env_data <- na.omit(envbind)
dim(env_data) - dim(envbind)

# niche écologique observée
env_dens <- lapply(
  names(env_data)[4:length(names(env_data))], 
  \(varenv) {
    p <- ggplot(data = env_data, aes(x = get(varenv), col = 1, fill = 2)) + 
      geom_density(alpha = 0.6) +
      scale_fill_viridis_c() + 
      scale_color_viridis_c() +
      guides(fill = "none", col = "none") + 
      labs(x = varenv, y = "Densité")
  }
)
env_grph <- Reduce(`+`, env_dens)
x11() ; env_grph
# pas de gaussiennes claires