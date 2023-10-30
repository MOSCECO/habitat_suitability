sdmRasterCompilation <- function(
    sr, # List of rasters
    ens_alg, # Ensemble modelling algorithm
    met_evl, # Evaluation metric
    vec_weight # Vector of weights for scales
) {
  # id
  code_alg <- paste0("EM", ens_alg, "By", met_evl)

  # Aggrégation
  sr_subset <- lapply(
    sr,
    \(r) {
      subset(r, names(r)[grepl(code_alg, names(r))])
    }
  )

  # stacking
  srs <- Reduce(c, sr_subset)

  # calcul de la moyenne pondérée
  srs_wmean <- terra::weighted.mean(srs, vec_weight)

  return(srs_wmean)
}
