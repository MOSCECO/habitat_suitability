pathClusterBiomod <- function(x) {
  gsub(
    "/scratchbeta/manielg",
    "/home/borea/Documents/mosceco/r_projects",
    x
)
}
mod_ensemble@models.out@link <- mod_ensemble@models.out@link %>%
  pathClusterBiomod()
mod_ensemble@models.evaluation@link <- mod_ensemble@models.evaluation@link %>%
  pathClusterBiomod()
mod_ensemble@variables.importance@link <- mod_ensemble@variables.importance@link %>%
  pathClusterBiomod()
mod_ensemble@models.prediction@link <- mod_ensemble@models.prediction@link %>%
  pathClusterBiomod()
mod_ensemble@models.prediction.eval@link <- mod_ensemble@models.prediction.eval@link %>%
  pathClusterBiomod()
mod_ensemble@link <- mod_ensemble@link %>% pathClusterBiomod()

spec_ensemble_models@models.out@link
spec_ensemble_models@models.evaluation@link
spec_ensemble_models@variables.importance@link
spec_ensemble_models@models.prediction@link
spec_ensemble_models@models.prediction.eval@link
spec_ensemble_models@link
