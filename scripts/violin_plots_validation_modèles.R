# Comparaison des environnements occupées par les occurrences observées
# et prédites par les modèles de distribution d'espèces

# Espèce considérée ----
binomial_name <- "Claremontiella nodulosa"

# importation des modèles compilées ----
fs <- list.files(
  here("data", "analysis", "compilation"),
  full.names = T,
  pattern = "presence-absence"
)
ns <- list.files(
  here("data", "analysis", "compilation"), pattern = "presence-absence"
)
mods <- lapply(fs, rast)
names(mods) <- gsub("\\.tif", "", ns)

# constitution en liste organisée en ----
# algorithme de compilation > algorithme de modélisation
mods <- list(
  wmean = Reduce(c, mods[names(mods)[grepl("wmean", names(mods))]]),
  ca    = Reduce(c, mods[names(mods)[grepl("ca", names(mods))]])
)
mods <- lapply(mods, \(m) {
  names(m) <- ns[grepl("wmean", ns)] %>%
    lapply(str_split, "_") %>%
    lapply(pluck, 1, 2) %>%
    unlist(use.names = F) %>%
    gsub("[0-9]", "", .)
  return(m)
})

# Extraction des données environnementales correspondant aux présences
# modélisées
ae <- c(subset(climosaic, names(cgc_sub)), sxt_sub, hab_sub)
# masque des variables environnementales
mods_masks <- sapply(
  names(mods),
  \(alg_mod) {
    sapply(
      names(mods[[alg_mod]]),
      \(alg_cmp) {
        x <- mods[[alg_mod]][[alg_cmp]]
        x <- ifel(x > 0, 1, NA)
        z <- x*ae
        names(z) <- names(ae)
        return(z)
      },
      simplify = F,
      USE.NAMES = T
    )
  },
  simplify = F,
  USE.NAMES = T
)

# par(mfrow = c(1, 3))
# plot(mods_masks$wmean$rf$mean.sbt)
# plot(mods_masks$wmean$maxent$mean.sbt)
# plot(mods_masks$wmean$ensemble$mean.sbt)
# dev.off()

# valeurs des niches modélisées
mods_vals <- sapply(
  names(mods),
  \(alg_mod) {
    sapply(
      names(mods[[alg_mod]]),
      \(alg_cmp) {
        x <- mods_masks[[alg_mod]][[alg_cmp]]
        as.data.frame(x)
      },
      simplify = F,
      USE.NAMES = T
    )
  },
  simplify = F,
  USE.NAMES = T
)

bios <- do.call(
  cbind,
  lapply(
    list.files(here("data", "analysis"), pattern = "bio_data", full.names = T),
    \(p) {
      d <- readRDS(p)
      d %>%
        st_drop_geometry() %>%
        filter(scale == "local") %>%
        filter(type == "pr") %>%
        select(-c(type, id, scale, x, y, individualCount))
    }
  )
)
bios <- bios[, names(mods_vals$wmean$ensemble)]
mods_vals$wmean <- append(list(observed = bios), mods_vals$wmean)
mods_vals$ca <- append(list(observed = bios), mods_vals$ca)

# comparaison des valeurs des différences niches modélisées et observées
# localement
comp_niches <- lapply(
  names(mods_vals),
  \(alg_cmp) {

    # alg_cmp <- "wmean"

    m <- mods_vals[[alg_cmp]]

    # Centrage réduction
    m <- lapply(
      names(m),
      \(alg_mod) {
        md <- m[[alg_mod]] %>%
          apply(2, \(x) {(x - mean(x, na.rm = T))/sd(x, na.rm = T)}) %>%
          as.data.frame()
        e <- cbind(alg_cmp, alg_mod, md)
        return(e)
      }
    )

    # pivot
    mm <- do.call(rbind, m) %>%
      select(-1) %>%
      melt(id.vars = "alg_mod")
    mm$alg_mod <- factor(
      mm$alg_mod, levels = c("observed", "rf", "maxent", "ensemble")
    )
    levels(mm$alg_mod) <- c("Observations", "Forêt Aléatoire", "MAXENT", "Ensemble")

    # différences significatives ou non entre niches modélisées et observée ?
    mm_split <- split(mm, f = mm$variable)
    res_wxtest <- lapply(
      mm_split,
      \(tb) {
        tb <- na.omit(tb)
        pairwise.wilcox.test(
          tb$value, tb$alg_mod, p.adjust.method = "bonferroni"
        )
      }
    )
    seuils_test <- res_wxtest %>%
      lapply(pluck, "p.value") %>%
      lapply(`<`, 0.05) # Seuil normal parce que la correction de
    # Bonferroni incluse dans "p.adjust" multiplie les p.values
    # et ne divise pas le seuil de risque

    # représentation graphique
    p <- ggplot(
      data = mm,
      aes(
        x = variable,
        y = value,
        col = alg_mod,
        group = interaction(alg_mod, variable)
      )
    ) +
      # geom_violin(position = "dodge") +
      geom_boxplot(position = "dodge") +
      # geom_jitter(
      #   aes(group = interaction(alg_mod, variable)),
      #   position = position_jitterdodge(dodge.width = 0.9),
      #   size = 0.1,
      #   alpha = 0.5
      # ) +
      guides(
        col  = guide_legend(title = "Algorithme de\nmodélisation")
      ) +
      xlab("Variable environnementale (climatologies)") +
      ylab("Valeurs centrées réduites") +
      labs(title = alg_cmp)

    return(list(res = seuils_test, p = p))
  }
)

# wmean
comp_niches[[1]]$res
comp_niches[[1]]$p

# ca
comp_niches[[2]]$res
comp_niches[[2]]$p


# Comparaison des niches globales et locales pour les données de copernicus
# normal
o <- ocr <-  bio %>%
  st_drop_geometry() %>%
  filter(type == "pr") %>%
  select(-c(type, id, x, y, individualCount))
om <-  o %>%
  melt(id.vars = "scale")
# Centrage réduction
ocr[, -1] <- ocr[, -1] %>%
  apply(2, \(x) {(x - mean(x))/sd(x)}) %>%
  as.data.frame()
ocrm <- ocr %>%
  melt(id.vars = "scale")
ocrm$scale <- factor(ocrm$scale)
levels(ocrm$scale) <- c("Globale", "Locale")

# différences significatives ou non
l <- om %>%
  split(f = om$scale) %>%
  lapply(\(x){split(x, f = x$variable)})
ll <- lapply(l, \(x) lapply(x, \(y) y %>% select(value)))

wx_res <- mapply(
  \(x, y) {
    res <- wilcox.test(x[["value"]], y[["value"]])
    return(res$p.value < 0.05)
  },
  ll$global,
  ll$local,
  SIMPLIFY = F,
  USE.NAMES = T
)
# Seulement deux paramètres ne diffèrent pas significativement
# dans leurs distribution.

# représentation graphique
ggplot(
  data = ocrm,
  aes(
    x = variable,
    y = value,
    col = scale,
    group = interaction(scale, variable)
  )
) +
  geom_violin(position = "dodge") +
  geom_jitter(
    aes(group = interaction(scale, variable)),
    position = position_jitterdodge(dodge.width = 0.9),
    size = 0.1,
    alpha = 0.5
  ) +
  guides(
    col  = guide_legend(title = "Échelle")
  ) +
  xlab("Variable environnementale (climatologies Copernicus)") +
  ylab("Valeur centrée réduite")
