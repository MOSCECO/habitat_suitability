ecologicalNicheObserved <- function(
    tb, vec_varenv, file_name, taxRank
) {
  plots <- lapply(
    vec_varenv,
    \(varenv) {
      title_varenv <- switch(
        varenv, 
        hm0 = "Hauteur significative des vagues - VHM0",
        ww = "Hauteur significative des vagues de vent - VHM0_WW",
        sw1 = "Hauteur significative des vagues de houle - VHM0_SW1",
        chla = "Chlorophylle a",
        ssm = "Matières suspendues",
        sst = "Température de surface",
        sbt = "Température du fond estimée",
        tur = "Turbidité", 
        so = "Salinité selon quatre tranches bathymétriques",
        # so25 = "Salinité à 25 m de profondeur",
        # so77 = "Salinité à 77 m de profondeur",
        # so130 = "Salinité à 130 m de profondeur",
        depth = "Profondeur"
      )
      
      colFill_varenv <- switch(
        varenv, 
        hm0   = c("blue", "lightblue1"),
        ww    = c("blue", "lightblue3"), 
        sw1   = c("blue", "lightblue4"), 
        chla  = c("green", "lightgreen"),
        ssm   = c("coral", "coral4"),
        sst   = c("red", "red3"),
        sbt   = c("orange", "orange3"),
        tur   = c("salmon", "salmon3"),
        so   = c("purple", "purple3"),
        # so25  = c("purple", "purple3"),
        # so77  = c("purple", "purple3"),
        # so130 = c("purple", "purple3"),
        depth = c("grey", "seashell2")
      )
      
      p <- ggplot(
        data = tb, aes(
          x = get(varenv), fill = get(taxRank), col = get(taxRank)
        )
      ) + 
        geom_density(alpha = 0.6) +
        # scale_fill_viridis_d() + 
        # scale_color_viridis_d() + 
        scale_color_manual(values = colFill_varenv[[1]]) + 
        scale_fill_manual( values = colFill_varenv[[2]]) + 
        guides(fill = "none", col = "none") + 
        labs(
          # title = unique(tb$scientificName) %>% 
          #   paste("-", nrow(tb), "occurrences"),
          x = title_varenv, 
          y = "Densité"
        )
    }
  )
  names(plots) <- vec_varenv
  
  ecoNiche <- Reduce(`+`, plots) + plot_annotation(
    title = unique(tb[[taxRank]]),
    subtitle = paste(nrow(tb), "occurrences")
  ) + plot_layout(ncol = 3, nrow = 4)
  
  ggexport(
    ecoNiche, 
    filename = file_name, 
    height = 3800,
    width = 4000,
    res = 300
  )
}