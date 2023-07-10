plotComparaisonOccurrences <- function(sr) {
  ps <- lapply(
    islands, 
    \(nisl) {
      # nisl <- "GLP"
      
      # chargement des éléments du graphe
      isl          <- maps[[nisl]]
      e            <- ext(climatologies[[nisl]])
      sr_crop      <- terra::crop(sr, e) 
      tb           <- as.data.frame(sr_crop, xy = T)
      names(tb)[3] <- "value"
      # tb           <- tb %>% filter(value != 0)
      occ <- spp_sf %>% st_crop(as.vector(e)[c(1,3,2,4)])
      
      # emprise rectangulaire
      o <- st_convex_hull(st_union(isl))
      oproj <- st_transform(o, crs = "EPSG:32620")
      cproj <- st_centroid(oproj)
      bproj <- bSquare(cproj, as.numeric(sqrt(st_area(o))*1.5)^2)
      b <- st_transform(bproj, crs = "EPSG:4326")
      # ggplot() + geom_sf(data = b) + geom_sf(data = isl)
      # (bbox <- st_bbox(b))
      bbox <- st_bbox(b)
      
      # figures ggplot2
      p <- ggplot() + 
        geom_tile(data = tb, aes(x = x, y = y, fill = value)) + 
        geom_sf(data = isl) + 
        scale_fill_gradient2(
          low = "transparent", mid = "orange", high = "#50d464", midpoint = 500
        ) +
        labs(x = "Longitude", y = "Latitude") + 
        guides(fill = guide_colorbar("Probabilité\nd'occurrence")) +
        xlim(bbox[c(1,3)]) + 
        ylim(bbox[c(2,4)]) + 
        theme_map()
      pocc <- p + 
        geom_sf(data = occ, col = "red", shape = "+", size = 5)
      
      # préparation de la seconde carte sans certains éléments graphiques
      p <- if(nisl == "MTQ") {
        p + 
          theme(
            axis.title.y = element_blank(), 
            axis.line.y  = element_blank(), 
            axis.text.y  = element_blank(), 
            axis.ticks.y = element_blank()
          )
      } else {
        p + theme(legend.position = "none")
      }
      
      pocc <- if(nisl == "MTQ") {
        pocc + 
          theme(
            axis.title.y = element_blank(), 
            axis.line.y  = element_blank(), 
            axis.text.y  = element_blank(), 
            axis.ticks.y = element_blank()
          )
      } else {
        pocc + theme(legend.position = "none")
      }
      return(list(nocc = p, pocc = pocc))
    }
  )
  
  P    <- Reduce(`+`, ps %>% lapply(pluck, 1))
  Pocc <- Reduce(`+`, ps %>% lapply(pluck, 2))
  
  return(list(nocc = P, pocc = Pocc))
}