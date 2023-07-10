maskForModels <- function(r, qt, do.plot = FALSE) {
  # Filtre selon un seuil de probabilité de présence arbitraire fixé
  vec_r <- unlist(as.data.frame(r))
  thresh <- quantile(vec_r, qt)
  r_mask <- ifel(r < thresh, 0, 1)
  
  # Filtre bathymétrique
  # Utilisation de la profondeur maximale de l'espèce pour filtrer les cellules
  b <- min(terra::extract(climosaic$depth, spp[, c("x", "y")], ID = F))
  bathy_mask <- ifel(climosaic$depth >= b, 1, 0)
  
  # mask_out <- r_mask
  mask_out <- r_mask * bathy_mask
  
  if (do.plot) {
    x11(); plot(mask_out) 
  }
  return(mask_out)
}