# Climatologies à partir des premiers axes d'une ACP qui synthétise les
# informations des climatologies des variables environnementales

# matrice des données
tb <- as.data.frame(climosaic) %>%
  as_tibble()
# table(is.na(tb))

# Réduction de la colinéarité
# variance inflation factor
(vif_excluded <- usdm::vifstep(tb %>% as.matrix())@excluded)
varenv_subset <- names(tb)[-which(names(tb) %in% vif_excluded)]
tb <- tb %>% dplyr::select(all_of(varenv_subset))
climosaic_sub <- terra::subset(climosaic, varenv_subset)

# ACP sur matrices de corrélation
m <- as.data.frame(climosaic_sub)
cd <- as.data.frame(climosaic_sub, xy = T) %>%
  as_tibble() %>% 
  dplyr::select(x, y)
res_pca <- prcomp(m, center = T, scale. = T)

# méthode des bâtons brisés et contribution des axes
data_evplot <- res_pca$sdev^2
# png(
#   here("figures", "evplot_pca_clim.png"), 
#   height = 1000, 
#   width = 1600, 
#   res = 200
# )
x11()
evplot(data_evplot)
# dev.off()
cc12 <- fviz_pca_var(res_pca, c(1,2), repel = T, col.var = "contrib") + 
  labs(title = NULL) + 
  scale_color_gradient(low = "red", high = "darkgreen")
cc13 <- fviz_pca_var(res_pca, c(1,3), repel = T) + labs(title = NULL)
ggexport(
  cc12 + cc13, 
  filename = here("data", "analysis", "correlation_circles_123_pca.png"),
  height = 800,
  width = 1600, 
  res = 150
)
m_loadings <- res_pca$rotation[, 1:3] %>% 
  cbind((res_pca$rotation[, 1:3])^2)
colnames(m_loadings)[4:6] <- colnames(m_loadings)[1:3] %>% paste0("sq")
write.csv(
  m_loadings,
  here("data", "analysis", "rotation_pca.csv")
)
fviz_contrib(res_pca, "var", 1)
fviz_contrib(res_pca, "var", 2)
fviz_contrib(res_pca, "var", 3)
fviz_contrib(res_pca, "var", c(1,2))
# indiv12 <- fviz_pca_biplot(
#   res_pca, label = "var", col.var = "contrib"
# )
# ggexport(
#   plot = indiv12,
#   filename = here("figures", "pca_env_indivCos2_12.png"), 
#   width = 1200, 
#   height = 1000, 
#   res = 150,
#   units = "px",
#   device = "png", 
#   limitsize = F
# )
# indiv13 <- fviz_pca_biplot(
#   res_pca, c(1, 3), label = "var", col.var = "contrib", 
#   palette = palette(c("blue", "red"))
# )
# ggexport(
#   plot = indiv13,
#   filename = here("figures", "pca_env_indivCos2_13.png"), 
#   width = 1200, 
#   height = 1000, 
#   res = 150,
#   units = "px",
#   device = "png", 
#   limitsize = F
# )

# on conserve "naxis" axes
naxis <- 3
data_pca_rast <- cd %>%
  cbind(res_pca$x[, 1:naxis]) %>% 
  as_tibble()

# projection des données dans un nouveau spatial raster à plusieurs couches
climosaic_pca <- rast(
  x      = data_pca_rast, 
  crs    = crs(climosaic)
)
x11() ; plot(climosaic_pca)

pmosaic <- here("data", "tidy", "climatologies_mosaic")
makeMyDir(pmosaic)

# sauvegarde
writeRaster(
  climosaic_pca, 
  here(
    pmosaic, 
    paste("climatologies", "mosaic", "pca", sep = "_") %>% 
      paste0(".tif")
  ), 
  filetype = "GTiff",
  overwrite = TRUE
)