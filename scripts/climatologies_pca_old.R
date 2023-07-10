# Climatologies à partir des premiers axes d'une ACP qui synthétise les
# informations des climatologies des variables environnementales
climosaicrope <- lapply(
  extents, \(e) terra::crop(climosaic, terra::ext(c(e$x, e$y)))
) %>% 
  Reduce(terra::mosaic, .)
climosaic2 <- as.data.frame(
  climosaic, cells = T, xy = T
)
o <- apply(climosaic2, 2, \(x) which(is.na(x)))
oo <- Reduce(union, o)
cells_to_delete <- climosaic2$cell[oo]
climosaic5 <- climosaic
climosaic5[cells_to_delete] <- NA
as.data.frame(climosaic5)
climosaic3 <- climosaic2 %>% 
  filter(
    !(1:nrow(climosaic2) %in% oo)
  )
rast(climosaic3, type = "xyz")

terra::writeRaster(
  x = climosaicrope, 
  filename = here("data", "c.tif"),
  filetype = "GTiff", 
  overwrite = T
)
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