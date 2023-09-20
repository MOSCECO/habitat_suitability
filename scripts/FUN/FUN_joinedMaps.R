joinedMaps <- function(
    list_of_maps,
    collect_guides = F,
    keep_title     = T,
    plot_title     = "A new title"
) {

  # on enlève tous les titres des axes des ordonnés après le premier graphe
  list_of_maps_modified <- list_of_maps[1] %>%
    append(
      lapply(
        list_of_maps[-1],
        \(m) {
          # m <- list_of_maps[[2]]

          if ("patchwork" %in% class(m)) {
            Reduce(
              `+`,
              lapply(
                seq_along(m$facet),
                \(i) {
                  m[[i]] +
                    labs(subtitle = m$patches$annotation$subtitle) +
                    theme(
                      axis.text.y  = element_blank(),
                      axis.title.y = element_blank(),
                      axis.ticks.y = element_blank()
                    )
                }
              )
            ) +
              {
                if(keep_title) plot_annotation(
                  theme = theme(
                    plot.title = m$patches$annotation$title
                  )
                )
              } +
              theme()
          } else {

            m +
              theme(
                axis.text.y  = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank()
              ) +
              {
                if(!keep_title) theme(plot.title = element_blank())
              } +
              theme()

          }
        }
      )
    )

  list_of_maps_modified[[1]] <- if(
    "patchwork" %in% class(list_of_maps_modified[[1]])
  ) {
    Reduce(
      `+`,
      lapply(
        seq_along(list_of_maps_modified[[1]]$facet),
        \(i) {
          list_of_maps_modified[[1]][[i]] +
            labs(
              subtitle = list_of_maps_modified[[1]]$patches$annotation$subtitle
            )
        }
      )
    )
  } else { list_of_maps_modified[[1]] }

  # On ne retient que le titre de l'axe des abscisses
  middle <- ceiling(length(list_of_maps_modified)/2)
  list_of_maps_middlefied <- lapply(
    seq_along(list_of_maps_modified),
    \(i) {

      m <- list_of_maps_modified[[i]]

      if("patchwork" %in% class(m)) {
        if (i != middle) {
          m1 <- m[[1]] +
            theme(axis.title.x = element_blank())
          m2 <- m[[2]] +
            theme(axis.title.x = element_blank())
          m1 + m2
        } else {
          m
        }
      } else {
        if (i != middle) {
          m +
            theme(axis.title.x = element_blank())
        } else { m }
      }

    }
  )

  list_of_maps_middlefied[[1]] <- if("patchwork" %in% class(list_of_maps_middlefied[[1]])) {
    list_of_maps_middlefied[[1]] +
      {
        if(keep_title) plot_annotation(title = plot_title)
      }
  } else { list_of_maps_middlefied[[1]] }

  # concaténation des cartes
  pout <- Reduce(`|`, list_of_maps_middlefied) +
    {
      if(collect_guides) plot_layout(guides = "collect")
    } +
    theme()

  return(pout)
}
