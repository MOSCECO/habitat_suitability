# Multivariate environment similarity surface

# spatial raster de prédicteurs
predictors_spatrast_cpc <- clim_proj_sub # copernicus
predictors_spatrast_sxt <- sxt_sub       # sextant
predictors_spatrast_hab <- hab_sub       # habitats
predictors_cpc <- do.call(stack, lapply(as.list(predictors_spatrast_cpc), raster))
predictors_sxt <- do.call(stack, lapply(as.list(predictors_spatrast_sxt), raster))
predictors_hab <- do.call(stack, lapply(as.list(predictors_spatrast_hab), raster))

# données biologiques
reference_points_hab <- terra::extract(hab_sub, spp_local_sf, ID = F) # habitats
reference_points_hab[is.na(reference_points_hub)] <- 0
reference_points_sxt <- terra::extract(sxt_sub, spp_local_sf, ID = F) # sextant
reference_points_sxt[is.na(reference_points_sxt)] <- 0
reference_points_cpc <- bio %>% 
  st_drop_geometry() %>% 
  select(7:14)

# MESS
res_mess_cpc <- dismo::mess(x = predictors_cpc, v = reference_points_cpc, full = T)
res_mess_sxt <- dismo::mess(x = predictors_sxt, v = reference_points_sxt, full = T)
res_mess_hab <- dismo::mess(x = predictors_hab, v = reference_points_hab, full = T)

# Visualisation du MESS
x11(); plot(res_mess_cpc)
x11(); plot(res_mess_sxt)
x11(); plot(res_mess_hab)