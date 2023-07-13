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
      )
    )

  # On ne retient que le titre de l'axe des abscisses
  middle <- ceiling(length(list_of_maps_modified)/2)
  list_of_maps_middlefied <- lapply(
    seq_along(list_of_maps_modified),
    \(i) {
      m <- list_of_maps_modified[[i]]

      if (i != middle) {
        m +
          theme(axis.title.x = element_blank())
      } else { m }
    }
  )

  # concaténation des cartes
  pout <- Reduce(`+`, list_of_maps_middlefied) +
    {
      if(collect_guides) plot_layout(guides = "collect")
    } +
    {
      if(!keep_title) plot_annotation(
        title = plot_title, theme = theme(title = element_text(hjust = 0.5))
      )
    } +
    theme()

  return(pout)
}
