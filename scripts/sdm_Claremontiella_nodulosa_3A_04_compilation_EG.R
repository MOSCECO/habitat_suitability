# Compilation des résultats des différents modèles

# Problème avec les données : variations de la chlorophylle a aberrantes. 
# Modèle d'habitat à seulement deux paramètres 
# (pente et profondeur avec des NA dans la pente)

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

# EG : problème : si on fait un premier seuil pour le masque
# des habitats en décidant d'un seuil arbitraire, on perd l'idée
# de filtre
# décider d'un seuil identique pour chaque modèle et aggréger
# les modèles ensemble selon l'idée qu'une absence est rédhibitoire
# pour la cellule. 

# Seuil de présence/absence
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

# Inversion de l'object en algorithme 
# puis modèle
m <- transpose(
  lapply(masks, \(r) {
    lr <- as.list(r)
    names(lr) <- c("ca", "wmean")
    return(lr)
  })
)
# Aggrégation des modèles selon 
# 1 + 1 = 1
# tout le rest = 0
m2 <- lapply(
  m, 
  \(vec_r) {
    vr <- app(Reduce(c, vec_r), sum)
    rout <- ifel(vr < length(m[[1]]), 0, 1)
    return(rout)
  }
)

# visualisations
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

# On moyenne et on prend le maximum des probabilités de présences pour les 
# différents modèles sélectionnés. 
p_aggreg <- aggregationModels(
  sr_for_aggreg, 
  c("ca", "wmean"), 
  c("mean", "max"), 
  do_plot = T
)
names(p_aggreg) <- c("ca", "wmean")

# On adapte la variable masks pour ces modèles en passant directement en 
# présence absence
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

# visualisations
x11(); par(mfrow = c(1,4))
plot(masks$ca_max, main = "ca_max")
plot(masks$wmean_max, main = "wmean_max")
plot(masks$ca_mean, main = "ca_mean")
plot(masks$wmean_mean, main = "wmean_mean")

aa_max <- plotComparaisonOccurrences2(masks$ca_max)
ww_max <- plotComparaisonOccurrences2(masks$wmean_max)
x11(); aa_max$pocc / ww_max$pocc

aa_mean <- plotComparaisonOccurrences2(masks$ca_mean)
ww_mean <- plotComparaisonOccurrences2(masks$wmean_mean)
x11(); aa_mean$pocc / ww_mean$pocc
