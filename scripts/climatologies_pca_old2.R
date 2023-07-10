# Climatologies à partir des premiers axes d'une ACP qui synthétise les
# informations des climatologies des variables environnementales
climosaicrope <- lapply(
  extents, \(e) terra::crop(climosaic, terra::ext(c(e$x, e$y)))
) %>% 
  Reduce(terra::mosaic, .)
tb <- as.data.frame(climosaicrope, xy = T) %>%
  na.omit() %>% # je comprends pas pourquoi il y a des NA...
  as_tibble()
res_pca <- prcomp(tb[, -c(1,2)], center = T, scale. = T)
# data_evplot <- res_pca$sdev^2  
# x11()
# evplot(data_evplot)
fviz_contrib(res_pca, "var", 1)
fviz_contrib(res_pca, "var", 2)
fviz_contrib(res_pca, "var", c(1,2))
data_pca_rast <- tb[, 1:2] %>% cbind(res_pca$x[, 1:3]) %>% as_tibble()
climosaic_pca <- rast(
  x      = data_pca_rast, 
  extent = ext(climosaicrope),
  crs    = crs(climosaic)
) # résolution différente de climosaic, sûrement à cause des na.omit...
x11()
plot(climosaic_pca)

climosaic_pca_resample <- resample(
  climosaic_pca, climosaic
)