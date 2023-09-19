plotComparaisonOccurrences_hs <- function(sr, spp_sf, title, subtitle) {
  ps <- sapply(
    islands,
    \(nisl) {
      # nisl <- "GLP"

      # chargement des éléments du graphe
      isl          <- maps[[nisl]]
      e            <- ext(climatologies[[nisl]])
      sr_crop      <- terra::crop(sr, e)
      tb           <- as.data.frame(sr_crop, xy = T)
      names(tb)[3] <- "value"
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
          low = "white",
          mid = "#ecb171",
          high = "#03a700",
          midpoint = 250,
          limits = c(0, 1000)
        ) +
        guides(fill = guide_colorbar(title = "Adéquation\nenvironnementale")) +
        labs(x = "Longitude", y = "Latitude") +
        xlim(bbox[c(1,3)]) +
        ylim(bbox[c(2,4)]) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(plot.margin = unit(rep(0.01, 4), "pt"))
      pocc <- p +
        geom_sf(data = occ, col = "red", shape = "+", size = 3)

      # préparation de la seconde carte sans certains éléments graphiques
      p0 <- p +
        labs(title = title, subtitle = subtitle)

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

      pocc0 <- pocc +
        labs(title = title, subtitle = subtitle)

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
      return(list(nocc = p, pocc = pocc, orig_p = p0, orig_pocc = pocc0))
    },
    simplify = F,
    USE.NAMES = T
  )

  P    <- Reduce(`+`, ps %>% lapply(pluck, 1)) +
    plot_layout(guides = "collect", ) +
    plot_annotation(title = title, subtitle = subtitle) &
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = unit(rep(0.01, 4), "pt")
    )
  Pocc <- Reduce(`+`, ps %>% lapply(pluck, 2))  +
    plot_layout(guides = "collect", ) +
    plot_annotation(title = title, subtitle = subtitle) &
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = unit(rep(0.01, 4), "pt")
    )

  return(
    list(
      ANT = list(nocc = P, pocc = Pocc),
      GLP = list(nocc = ps[["GLP"]]$orig_p, pocc = ps[["GLP"]]$orig_pocc),
      MTQ = list(nocc = ps[["MTQ"]]$orig_p, pocc = ps[["MTQ"]]$orig_pocc)
    )
  )
}
