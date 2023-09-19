plotComparaisonOccurrences_pa <- function(
    sr, spp_sf, pred, title, subtitle
) {
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
      prd <- pred %>% st_crop(as.vector(e)[c(1,3,2,4)])
      occ <- occ %>%
        add_column(
          right_pred = prd[[3]] %>% st_drop_geometry(),
          .after = "individualCount"
        )

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
      # Distribution de base ----
      p <- ggplot() +
        geom_tile(data = tb, aes(x = x, y = y, fill = factor(value))) +
        geom_sf(data = isl) +
        scale_fill_manual(
          values = c("white", "#03a700"),
          labels = c("Non-adéquat", "Adéquat")
        ) +
        labs(x = "Longitude", y = "Latitude") +
        guides(fill = guide_legend(title = "Environnement")) +
        xlim(bbox[c(1,3)]) +
        ylim(bbox[c(2,4)]) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(
          plot.margin = unit(rep(0.01, 4), "pt"),
          legend.key = element_rect(colour = "black")
        )

      # Graphe des présences correctement prédites ou non ----
      pocc <- p +
        geom_sf(
          data = occ %>% filter(individualCount > 0),
          aes(shape = right_pred, size = right_pred, col = right_pred),
          alpha = 0.7
        ) +
        scale_shape_manual(
          values = c(1, 3), labels = c("Non-prédite", "Prédite")
        ) +
        scale_size_manual(
          values = c(1, 3), labels = c("Non-prédite", "Prédite")
        ) +
        scale_color_manual(
          values = c("blue", "red"),
          labels = c("Non-prédite", "Prédite")
        ) +
        guides(
          shape = guide_legend(title = "Présence\nobservée"),
          size  = guide_legend(title = "Présence\nobservée"),
          col   = guide_legend(title = "Présence\nobservée")
        )
      # Graphe des absences correctement prédites ou non ----
      aocc <- p +
        geom_sf(
          data = occ %>% filter(individualCount == 0),
          aes(shape = right_pred, size = right_pred, col = right_pred),
          alpha = 0.7
        ) +
        scale_shape_manual(
          values = c(1, 3), labels = c("Non-prédite", "Prédite")
        ) +
        scale_size_manual(
          values = c(1, 3), labels = c("Non-prédite", "Prédite")
        ) +
        scale_color_manual(
          values = c("blue", "red"),
          labels = c("Non-prédite", "Prédite")
        ) +
        guides(
          shape = guide_legend(title = "Absence\nobservée"),
          size  = guide_legend(title = "Absence\nobservée"),
          col   = guide_legend(title = "Absence\nobservée")
        )

      # seconde carte ----
      # préparation de la seconde carte sans certains éléments graphiques
      # base ----
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

      # présences ----
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

      # absences ----
      aocc0 <- aocc +
        labs(title = title, subtitle = subtitle)
      aocc <- if(nisl == "MTQ") {
        aocc +
          theme(
            axis.title.y = element_blank(),
            axis.line.y  = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank()
          )
      } else {
        aocc + theme(legend.position = "none")
      }

      return(
        list(
          nocc = p,    # pas d'occurrences
          pocc = pocc, # présences
          aocc = aocc, # absences
          orig_p    = p0,
          orig_pocc = pocc0,
          orig_aocc = aocc0
        )
      )
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
  Aocc <- Reduce(`+`, ps %>% lapply(pluck, 3))  +
    plot_layout(guides = "collect", ) +
    plot_annotation(title = title, subtitle = subtitle) &
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = unit(rep(0.01, 4), "pt")
    )

  return(
    list(
      ANT = list(nocc = P, pocc = Pocc, aocc = Aocc),
      GLP = list(
        nocc = ps[["GLP"]]$orig_p,
        pocc = ps[["GLP"]]$orig_pocc,
        aocc = ps[["GLP"]]$orig_aocc
      ),
      MTQ = list(
        nocc = ps[["MTQ"]]$orig_p,
        pocc = ps[["MTQ"]]$orig_pocc,
        aocc = ps[["MTQ"]]$orig_aocc
      )
    )
  )
}
