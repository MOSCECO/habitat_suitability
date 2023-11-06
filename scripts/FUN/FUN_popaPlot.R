popaPlot <- function(
    projRasters,
    type,                        # Type de données
    superfamily = "all",         # "all", "Majoidea", "Muricoidea"
    ensemble_algorithm,          # "wmean", "ca"
    projection_time = "current", # "current", "ssp126", "ssp585"
    threshold_algorithm = "TSS", # "KAPPA", "ROC", "TSS"
    do_plot = TRUE,
    do_plot_combine = TRUE
) {
  # projRasters <- dis
  # type <- "adequation_environnementale"
  # ensemble_algorithm <- "ca"
  # type <- "presence_absence"
  # projection_time <- "current"
  o <- projRasters[[type]]
  o <- if (superfamily != "all") list(o[[superfamily]]) else o
  out <- Sapply(
    o,
    \(o_sf) {
      # o_sf <- o$Majoidea
      Sapply(
        o_sf,
        \(o_bn) {
          # o_bn <- o_sf$`Amphithrax hemphilli`
          o_bn[[ensemble_algorithm]][[projection_time]]
        })
    })
  out <- Reduce(c, out)
  out <- Mapply(
    \(r, nm) { names(r) <- nm ; return(r) },
    out, names(out)
  )
  out <- Reduce(c, out)
  if (do_plot) x11(); par(mfrow = c(4, 5)); plot(out)
  if (do_plot_combine) x11(); plot(app(out, sum)/nlyr(out))
  return(out)
}
