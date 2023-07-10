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
aggregationModels <- function(models, algos, fs, do_plot = F) {
  lapply(
    algos, 
    \(alg) {
      # alg <- "ca"
      # models <- p_masque
      l <- lapply(names(models), \(m) models[[m]][[alg]]$ras)
      # print(l)
      out_fs <- sapply(
        # fs, \(f) {print(f); Reduce(eval(parse(text = f)), l)}, simplify = F, USE.NAMES = T
        fs, \(f) {app(Reduce(c, l), f)}, simplify = F, USE.NAMES = T
      )
      
      if (do_plot) {
        lapply(names(out_fs), \(n) plot(out_fs[[n]], main = paste(alg, n)))
      }
      
      return(out_fs)
    }
  )
}

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

require(ggthemes)
plotComparaisonOccurrences <- function(sr) {
  ps <- lapply(
    islands, 
    \(nisl) {
      # nisl <- "GLP"
      
      # chargement des éléments du graphe
      isl          <- maps[[nisl]]
      e            <- ext(climatologies[[nisl]])
      sr_crop      <- terra::crop(sr, e) 
      tb           <- as.data.frame(sr_crop, xy = T)
      names(tb)[3] <- "value"
      # tb           <- tb %>% filter(value != 0)
      occ <- spp_sf %>% st_crop(as.vector(e)[c(1,3,2,4)])
      
      # emprise rectangulaire
      o <- st_convex_hull(st_union(isl))
      oproj <- st_transform(o, crs = "EPSG:32620")
      cproj <- st_centroid(oproj)
      bproj <- bSquare(cproj, as.numeric(sqrt(st_area(o))*1.5)^2)
      b <- st_transform(bproj, crs = "EPSG:4326")
      # ggplot() + geom_sf(data = b) + geom_sf(data = isl)
      # (bbox <- st_bbox(b))
      bbox <- st_bbox(b)
      
      # figures ggplot2
      p <- ggplot() + 
        geom_tile(data = tb, aes(x = x, y = y, fill = value)) + 
        geom_sf(data = isl) + 
        scale_fill_gradient2(
          low = "transparent", mid = "orange", high = "#50d464", midpoint = 500
        ) +
        labs(x = "Longitude", y = "Latitude") + 
        guides(fill = guide_colorbar("Probabilité\nd'occurrence")) +
        xlim(bbox[c(1,3)]) + 
        ylim(bbox[c(2,4)]) + 
        theme_map()
      pocc <- p + 
        geom_sf(data = occ, col = "red", shape = "+", size = 5)
      
      # préparation de la seconde carte sans certains éléments graphiques
      p <- if(nisl == "MTQ") {
        p + 
          theme(
            axis.title.y = element_blank(), 
            axis.line.y  = element_blank(), 
            axis.text.y  = element_blank(), 
            axis.ticks.y = element_blank()
          )
      } else {
        p + theme(legend.position = "none")
      }
      
      pocc <- if(nisl == "MTQ") {
        pocc + 
          theme(
            axis.title.y = element_blank(), 
            axis.line.y  = element_blank(), 
            axis.text.y  = element_blank(), 
            axis.ticks.y = element_blank()
          )
      } else {
        pocc + theme(legend.position = "none")
      }
      return(list(nocc = p, pocc = pocc))
    }
  )
  
  P    <- Reduce(`+`, ps %>% lapply(pluck, 1))
  Pocc <- Reduce(`+`, ps %>% lapply(pluck, 2))
  
  return(list(nocc = P, pocc = Pocc))
}

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





plotComparaisonOccurrences2 <- function(sr) {
  ps <- lapply(
    islands, 
    \(nisl) {
      # nisl <- "GLP"
      
      # chargement des éléments du graphe
      isl          <- maps[[nisl]]
      e            <- ext(climatologies[[nisl]])
      sr_crop      <- terra::crop(sr, e) 
      tb           <- as.data.frame(sr_crop, xy = T)
      names(tb)[3] <- "value"
      # tb           <- tb %>% filter(value != 0)
      occ <- spp_sf %>% st_crop(as.vector(e)[c(1,3,2,4)])
      vecr <- raster::extract(
        raster(sr_crop), occ, buffer = 350, fun = \(x) ifelse(sum(x) > 0, 1, 0)
      )
      cat(paste(nisl, "\n"))
      print(table(vecr, useNA = "always"))
      
      # emprise rectangulaire
      o <- st_convex_hull(st_union(isl))
      oproj <- st_transform(o, crs = "EPSG:32620")
      cproj <- st_centroid(oproj)
      bproj <- bSquare(cproj, as.numeric(sqrt(st_area(o))*1.5)^2)
      b <- st_transform(bproj, crs = "EPSG:4326")
      # ggplot() + geom_sf(data = b) + geom_sf(data = isl)
      # (bbox <- st_bbox(b))
      bbox <- st_bbox(b)
      
      # figures ggplot2
      p <- ggplot() + 
        geom_tile(data = tb, aes(x = x, y = y, fill = factor(value))) + 
        geom_sf(data = isl) + 
        scale_fill_manual(values = c("white", "green")) +
        labs(x = "Longitude", y = "Latitude") + 
        xlim(bbox[c(1,3)]) + 
        ylim(bbox[c(2,4)]) + 
        theme_map()
      pocc <- p + 
        geom_sf(data = occ, col = "red", shape = "+", size = 5)
      
      # préparation de la seconde carte sans certains éléments graphiques
      p <- if(nisl == "MTQ") {
        p + 
          theme(
            axis.title.y = element_blank(), 
            axis.line.y  = element_blank(), 
            axis.text.y  = element_blank(), 
            axis.ticks.y = element_blank()
          )
      } else {
        p + theme(legend.position = "none")
      }
      
      pocc <- if(nisl == "MTQ") {
        pocc + 
          theme(
            axis.title.y = element_blank(), 
            axis.line.y  = element_blank(), 
            axis.text.y  = element_blank(), 
            axis.ticks.y = element_blank()
          )
      } else {
        pocc + theme(legend.position = "none")
      }
      return(list(nocc = p, pocc = pocc))
    }
  )
  
  P    <- Reduce(`+`, ps %>% lapply(pluck, 1))
  Pocc <- Reduce(`+`, ps %>% lapply(pluck, 2))
  
  return(list(nocc = P, pocc = Pocc))
}
plotComparaisonOccurrences2(sr)
# Faire des seuils de présence en prenant différents cut de probabiltiés
# de présenc epour passer en présence absence

# EG : problème : si on fait un premier seuil pour le masque
# des habitats en décidant d'un seuil arbitraire, on perd l'idée
# de filtre
# décider d'un seuil identique pour chaque modèle et aggréger
# les modèles ensemble selon l'idée qu'une absence est rédhibitoire
# pour la cellule. 

# Répétition de filtres
seuil <- 500
masks <- lapply(
  # proj_currents[c("copernicus", "sextant", "hab")],
  proj_currents[c("copernicus", "sextant")],
  # proj_currents[c("copernicus", "means_sextant")],
  \(rs) {
    rs_sub <- subset(rs, names(rs)[grepl("wmeanBy|caBy", names(rs))])
    rs_fil <- ifel(rs_sub < seuil, 0 , 1)
    return(rs_fil)
  }
)
# lapply(masks, \(r) {x11(); plot(r)})
m <- transpose(
  lapply(masks, \(r) {
    lr <- as.list(r)
    names(lr) <- c("ca", "wmean")
    return(lr)
  })
)
m2 <- lapply(
  m, 
  \(vec_r) {
    vr <- app(Reduce(c, vec_r), sum)
    rout <- ifel(vr < length(m[[1]]), 0, 1)
    return(rout)
  }
)

x11(); par(mfrow = c(1,2)); plot(m2$ca, main = "ca"); plot(m2$wmean, main = "wmean")
aa <- plotComparaisonOccurrences2(m2$ca)
ww <- plotComparaisonOccurrences2(m2$wmean)
aa$pocc / ww$pocc

# La profondeur et la pente semblent à l'origine d'une meilleure performance du 
# modèle, on obtient un peu plus de la moitié des occurrences observées
# dans zones de présence modélisées avec l'habitat. 

# LOrsque l'on enlève le modèle habitat, les performances du modèle chutent, 
# notamment en Guadeloupe où les zones au large, où de faibles variations de 
# chlorophylle a prédominent, sont remplies de cellules de présence. 

# Je propose d'essayer de refaire un autre modèle pour la voir la différence 
# avec l'apport de l'habitat : 
  # En n'utilisant que des moyennes pour Sextant => Pas concluant...

# Essayer d'aggréger les résultats avant une conversion en présence/absence
# Répétition de filtres

sr_for_aggreg <- proj_currents[c("copernicus", "means_sextant", "hab")] %>% 
  lapply(
    \(sr) {
      sr2 <- subset(sr, names(sr)[grepl("wmeanBy|caBy", names(sr))])
      # names(sr2) <- c("ca", "wmean")
      l <- as.list(sr2)
      l <- lapply(l, \(ll) as.list(ll))
      names(l) <- c("ca", "wmean")
      l <- lapply(l, \(ll) {names(ll) <- "ras"; return(ll)})
      return(l)
    }
  )

p_aggreg <- aggregationModels(
  sr_for_aggreg, 
  c("ca", "wmean"), 
  c("mean", "max"), 
  do_plot = T
)
names(p_aggreg) <- c("ca", "wmean")

seuil <- 500
masks <- Reduce(
  c, 
  lapply(
    names(p_aggreg), 
    \(alg) {
      l1 <- lapply(
        names(p_aggreg[[alg]]), 
        \(f) {
          rs <- p_aggreg[[alg]][[f]]
          ifel(rs < seuil, 0 , 1)
        }
      )
      Rl1 <- Reduce(c, l1)
      names(Rl1) <- paste(alg, names(p_aggreg[[alg]]), sep = "_")
      return(Rl1)
    }
  )
)
plot(masks)

aa <- plotComparaisonOccurrences2(masks$ca_mean)
ww <- plotComparaisonOccurrences2(masks$wmean_mean)
# lapply(masks, \(r) {x11(); plot(r)})
m <- transpose(
  lapply(masks, \(r) {
    lr <- as.list(r)
    names(lr) <- c("ca", "wmean")
    return(lr)
  })
)
m2 <- lapply(
  m, 
  \(vec_r) {
    vr <- app(Reduce(c, vec_r), sum)
    rout <- ifel(vr < length(m[[1]]), 0, 1)
    return(rout)
  }
)

x11(); par(mfrow = c(1,2)); plot(m2$ca, main = "ca"); plot(m2$wmean, main = "wmean")
aa <- plotComparaisonOccurrences2(m2$ca)
ww <- plotComparaisonOccurrences2(m2$wmean)
aa$pocc / ww$pocc
