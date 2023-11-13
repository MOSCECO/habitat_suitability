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

  out <- if (type == "presence_absence") {
    t <- names(out[[1]][[1]]) %>% substr(nchar(.) - 2, nchar(.))
    cat(paste(threshold_algorithm, "=", t, "\n"))
    Sapply(
      out,
      \(x) subset(
        x,
        names(x)[
          which(grepl(tolower(threshold_algorithm), names(x)))
        ]
      )
    )
  } else out

  out <- Mapply(
    \(r, nm) { names(r) <- nm ; return(r) },
    out, names(out)
  )
  out <- Reduce(c, out)
  if (do_plot) x11(); plot(subset(out, 1:16))
  if (do_plot) x11(); plot(subset(out, 17:18))
  dvd <- if (type == "adequation_environnementale") nlyr(out) else 1
  if (do_plot_combine) x11(); plot(app(out, sum)/dvd)

  out <- list(
    species = out,
    combine = app(out, sum)/dvd
  )

  return(out)
}
