# Compilation des résultats des différents modèles

# Problème avec les données : variations de la chlorophylle a aberrantes. 
# Modèle d'habitat à seulement deux paramètres 
# (pente et profondeur avec des NA dans la pente)

# 2023-06-02 Proposition de SFR
# Fabriquer un masque à partir du modèle d'habitats avec les cellules ayant une 
# probabilité de présence supérieur au quantile 75% des valeurs. 
# Mettre à 0 toutes les cellules des deux autres modèles qui ne rentrent pas
# dans les cellules de présence du masque
# étudier les distributions des valeurs de probabilités de présence pour les 
# deux modèles. 
# Décider de la fonction à utiliser pour compiler les deux résultats


# À partir des cartes de probabilités de toutes les espèces, faire du clustering
# puis des distributions de probabilités de présences par cluster pour repérer
# des communautés d'espèces.

# importation des résultats de modèles ----
proj_currents <- lapply(
  list.files(
    here("data", "analysis", "models"), 
    pattern = "Cla.nod"
  ),
  \(f) {
    list.files(
      here("data", "analysis", "models", f, "proj_current"),
      pattern = "\\.tif", 
      full.names = T
    ) %>% rast()
  }
)
names(proj_currents) <- c(
  "copernicus", "hab", "sextant", "means_sextant", "bsc", "pca"
)

# masque ----
# threshold ----
proj <- proj_currents$hab
proj <- subset(proj, names(proj)[grepl("wmeanBy|caBy", names(proj))])

proj_masks <- lapply(proj, maskForModels, qt = 0.937, do.plot = T)
names(proj_masks) <- c("wmean", "ca")

# Isolement des cellules du masque dans les modèles sextant et copernicus
p_masque <- lapply(
  c("copernicus", "sextant", "means_sextant"), 
  \(lvl) {
    # lvl <- "copernicus"
    rs <- proj_currents[[lvl]]
    mapply(
      \(r, vn) {
        # r <- subset(rs, names(rs)[grepl("wmeanBy|caBy", names(rs))])[[1]]
        # vn <- "wmean"
        
        # Projection dans le masque
        v <- proj_masks[[vn]]
        rv <- r*v
        drf <- as.data.frame(rv, xy = T)
        names(drf)[3] <- "value"
        p_out_1 <- ggplot(data = drf, aes(x, y, fill = value)) + 
          geom_tile() + 
          coord_equal() + 
          scale_fill_gradient2(
            low = "lightblue", mid = "orange", high = "red", midpoint = 500
          ) + 
          labs(title = paste(lvl, vn))
        
        # Distribution des valeurs de présences - densités
        # seulement pour les zones du masque = 1
        q <- tibble(value = as.data.frame(rv)[as.data.frame(v) == 1])
        
        p_out_2 <- ggplot(
          data = q, aes(x = value, fill = "red", col = "red", alpha = 0.7)
        ) + 
          theme(legend.position = "none") +
          geom_density() +
          scale_color_manual(values = "red") +
          scale_fill_manual(values = "#F09C9D") +
          labs(title = paste(lvl, vn))
        
        # Distribution des valeurs de présences - boxplot
        p_out_3 <- ggplot(data = q, aes(x = value)) + 
          geom_boxplot() +
          theme_void()
        
        return(list(ras = rv, map = p_out_1, den = p_out_2, box = p_out_3))
      }, 
      subset(rs, names(rs)[grepl("wmeanBy|caBy", names(rs))]),
      names(proj_masks), 
      SIMPLIFY = F, 
      USE.NAMES = T
    )
  }
)
names(p_masque) <- c("copernicus", "sextant", "means_sextant")
p_masque <- lapply(p_masque, \(p) {names(p) <- c("ca", "wmean"); return(p) })

require(patchwork)

p1 <- p_masque$copernicus$ca$den / p_masque$copernicus$ca$box  + plot_layout(heights = c(0.9, 0.1))
p2 <- p_masque$copernicus$wmean$den / p_masque$copernicus$wmean$box  + plot_layout(heights = c(0.9, 0.1))
p3 <- p_masque$sextant$ca$den / p_masque$sextant$ca$box  + plot_layout(heights = c(0.9, 0.1))
p4 <- p_masque$sextant$wmean$den / p_masque$sextant$wmean$box  + plot_layout(heights = c(0.9, 0.1))

p5 <- (p1 + p2) + plot_layout(ncol = 2, nrow = 2, byrow = F, widths = c(0.5, 0.5))
p6 <- (p3 + p4) + plot_layout(ncol = 2, nrow = 2, byrow = F, widths = c(0.5, 0.5))
(p5 / p6)

# Aggrégation des modèles ----
p_aggreg <- aggregationModels(p_masque[c(1, 3)], c("ca", "wmean"), c("mean", "max"), do_plot = T)
names(p_aggreg) <- c("ca", "wmean")

# Distribution des probabilités de présences associées aux occurrences ----
distrib_proba_pres <- lapply(
  names(p_aggreg), 
  \(alg) {
    ralg <- p_aggreg[[alg]]
    buf <- 100
    dens <- lapply(
      names(ralg), 
      \(f) {
        ragg <- ralg[[f]]
        vec <- unlist(
          terra::extract(ragg, spp[, c("x", "y")], ID = F), use.names = F
        )
        while(dim(table(is.na(vec))) > 1) {
          zragg <- raster(ragg)
          vec <- unlist(raster::extract(zragg, spp[, c("x", "y")], buffer = buf, fun = "mean" ), use.names = F)
          buf <- buf + 10
        }
        print(paste(alg, buf))
        return(vec)
      }
    )
  }
)
names(distrib_proba_pres) <- c("ca", "wmean")
distrib_proba_pres <- lapply(distrib_proba_pres, \(l) {
  names(l) <- c("max", "mean")
  return(l)
})
tb <- Reduce(cbind, distrib_proba_pres %>% lapply(\(l) Reduce(cbind, l))) %>% 
  as_tibble()
names(tb) <- c("ca_mean", "ca_max", "wmean_mean", "wmean_max")
p_proba_pres_occ <- ggplot(data = tb) + 
  geom_density(aes(x = ca_mean),    fill = "red", col = "red", alpha = 0.3) +
  geom_density(aes(x = ca_max),     fill = "blue", col = "blue", alpha = 0.3) + 
  geom_density(aes(x = wmean_mean), fill = "red4", col = "red4", alpha = 0.3) +
  geom_density(aes(x = wmean_max),  fill = "lightblue", col = "lightblue", alpha = 0.3)
pa <- ggplot(data = tb) + 
  geom_histogram(aes(x = ca_mean),    fill = "red", col = "red", alpha = 0.3)
pb <- ggplot(data = tb) +  
  geom_histogram(aes(x = ca_max),     fill = "blue", col = "blue", alpha = 0.3)
pc <- ggplot(data = tb) +
  geom_histogram(aes(x = wmean_mean), fill = "red4", col = "red4", alpha = 0.3)
pd <- ggplot(data = tb) +
  geom_histogram(aes(x = wmean_max),  fill = "lightblue", col = "lightblue", alpha = 0.3)


# Visualisation
p_visu <- lapply(
  names(p_aggreg), 
  \(alg) {
    psout <- lapply(
      names(p_aggreg[[alg]]),
      \(f) {
        r <- p_aggreg[[alg]][[f]]
        drf <- as.data.frame(r, xy = T) 
        names(drf)[3] <- "value"
        p <- ggplot(data = drf, aes(x, y, fill = value)) + 
          geom_tile() + 
          coord_equal() + 
          scale_fill_gradient2(
            low = "white", mid = "orange", high = "green", midpoint = 500
          ) + 
          labs(title = paste(alg, f))
        return(p)
      }
    )
    names(psout) <- names(p_aggreg[[alg]])
    return(psout)
  }
)
names(p_visu) <- names(p_aggreg)


ras_non_aggreg <- list(
  cop_ca = p_masque$copernicus$ca$ras,
  sxt_ca = p_masque$sextant$ca$ras, 
  cop_wmean = p_masque$copernicus$wmean$ras,
  sxt_wmean = p_masque$sextant$wmean$ras
)
ras_aggreg <-list(
  ca_mean    = p_aggreg$ca$mean,
  ca_max     = p_aggreg$ca$max,
  wmean_mean = p_aggreg$wmean$mean,
  wmean_max = p_aggreg$wmean$max
)
p_comp_rasNon <- sapply(
  ras_non_aggreg, plotComparaisonOccurrences, simplify = F, USE.NAMES = T
)
p_comp_ra <- sapply(
  ras_aggreg, plotComparaisonOccurrences, simplify = F, USE.NAMES = T
)

sr <- ifel(
  p_aggreg$wmean$max > 750, 
  1, 
  0
)
plotComparaisonOccurrences(sr)
plotComparaisonOccurrences2(sr)