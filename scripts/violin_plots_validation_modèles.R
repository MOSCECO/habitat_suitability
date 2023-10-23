# Comparaison des environnements occupées par les occurrences observées
# et prédites par les modèles de distribution d'espèces

path_validation <- here("data", "analysis", "validation")
makeMyDir(path_validation)

lapply(
  species$species,
  \(bn) {
    # bn <- "Mithraculus forceps"
    supfam <- species$superFamily[species$species == bn]

    path_valsf <- here(path_validation, supfam)
    makeMyDir(path_valsf)
    path_valsf_species <- here(path_valsf, bn)
    makeMyDir(path_valsf_species)

    # importation des modèles compilées ----
    fs <- list.files(
      here("data", "analysis", "compilation", "presence_absence", supfam, bn),
      full.names = T
    )
    ns <- list.files(
      here("data", "analysis", "compilation", "presence_absence", supfam, bn)
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
    ae <- c(subset(climosaic, names(cgc_sub[[supfam]][[bn]])), sxt_sub, hab_sub)
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
        list.files(
          here("data", "tidy", "bio", supfam, bn),
          pattern = "local",
          full.names = T
        ),
        \(p) {
          d <- readRDS(p)
          d %>%
            st_drop_geometry() %>%
            # filter(scale == "local") %>%
            filter(type == "pr") %>%
            select(-c(type, id, scale, x, y, individualCount))
        }
      )
    )
    bios            <- bios[, names(mods_vals$wmean$ens)]
    # names(mods_vals$wmean$ens)[
    #   which(!(names(mods_vals$wmean$ens) %in% names(bios)))
    # ]
    mods_vals$wmean <- append(list(observed = bios), mods_vals$wmean)
    mods_vals$ca    <- append(list(observed = bios), mods_vals$ca)

    # comparaison des valeurs des différences niches modélisées et observées
    # localement
    comp_niches <- sapply(
      names(mods_vals),
      \(alg_cmp) {

        # alg_cmp <- "wmean"

        m <- mods_vals[[alg_cmp]]

        # Centrage réduction
        m <- lapply(
          names(m),
          \(alg_mod) {
            # alg_mod <- "observed"
            # alg_mod <- "ens"
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
          mm$alg_mod, levels = c("observed", "rf", "maxent", "ens")
        )
        levels(mm$alg_mod) <- c(
          "Observations", "Forêt Aléatoire", "MAXENT", "Ensemble"
        )

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
        seuils_test_pval <- res_wxtest %>%
          lapply(pluck, "p.value") %>%
          lapply(round, 2)
        seuils_test_code <- seuils_test_pval %>%
          lapply(
            .,
            symnum,
            na        = FALSE,
            corr      = FALSE,
            cutpoints = c(    0, .001, .01, .05, 1),
            symbols   = c("***", "**", "*", "n.s.")
          ) # Seuil normal parce que la correction de
        # Bonferroni incluse dans "p.adjust" multiplie les p.values
        # et ne divise pas le seuil de risque

        # représentation graphique
        p <- ggplot(
          data = mm,
          aes(
            x     = variable,
            y     = value,
            col   = alg_mod,
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
          labs(title = alg_cmp) +
          coord_flip()

        return(
          list(
            algo_pval = seuils_test_pval,
            algo_code = seuils_test_code,
            algo_plot = p
          )
        )
      },
      simplify = F,
      USE.NAMES = T
    )

    # wmean/ca
    lapply(
      names(comp_niches),
      \(algo_cmp) {
        # algo_cmp <- "wmean"
        lapply(
          names(comp_niches[[algo_cmp]]$algo_pval),
          \(varenv) {
            write.csv(
              # varenv <- "mean.sbt"
              comp_niches[[algo_cmp]]$algo_pval[[varenv]],
              here(
                path_valsf_species,
                paste(
                  "pvaleurs", "algos",
                  algo_cmp, varenv, sep = "_"
                ) %>% paste0(".csv")
              )
            )
          }
        )

        lapply(
          names(comp_niches[[algo_cmp]]$algo_code),
          \(varenv) {
            write.csv(
              # varenv <- "mean.sbt"
              comp_niches[[algo_cmp]]$algo_pval[[varenv]],
              here(
                path_valsf_species,
                paste(
                  "significativite", "algos",
                  algo_cmp, varenv, sep = "_"
                ) %>% paste0(".csv")
              )
            )
          }
        )

        ggexport(
          comp_niches[[algo_cmp]]$algo_plot,
          filename = here(
            path_valsf_species,
            paste(
              "comparaison", "algos", algo_cmp, sep = "_"
            ) %>% paste0(".png")
          ),
          height = 2400,
          width  = 2000,
          res = 300
        )

      }
    )

    # Comparaison des niches globales et locales pour les données de copernicus
    # occurrences complètes
    bio <- list.files(
      here("data", "tidy", "bio", supfam, bn),
      pattern = "global",
      full.names = T
    ) %>%
      readRDS()
    # normal
    o <- ocr <- bio %>%
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
        return(res$p.value %>% round(2))
      },
      ll$global,
      ll$local,
      SIMPLIFY = F,
      USE.NAMES = T
    ) %>%
      unlist() %>%
      rbind(
        symnum(
          .,
          corr      = FALSE,
          cutpoints = c(    0, .001, .01, .05, 1),
          symbols   = c("***", "**", "*", "n.s.")
        )
      )
    row.names(wx_res) <- c("p.value", "significance")

    # Aucun paramètre ne différe pas significativement entre valeurs des occurrences
    # locales et valeurs des occurrences globales.

    # représentation graphique
    p_echelle <- ggplot(
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
        alpha = 0.2
      ) +
      guides(
        col  = guide_legend(title = "Échelle")
      ) +
      xlab("Variable environnementale (climatologies Copernicus)") +
      ylab("Valeur centrée réduite") +
      coord_flip()

    write.csv(
      wx_res,
      here(
        path_valsf_species,
        paste(
          "significativite",
          "distribution",
          "occurrences",
          sep = "_"
        ) %>% paste0(".csv")
      ),
      row.names = F
    )

    ggexport(
      comp_niches[[algo_cmp]]$algo_plot,
      filename = here(
        path_valsf_species,
        paste(
          "comparaison",
          "distribution",
          "occurrences",
          sep = "_"
        ) %>% paste0(".png")
      ),
      height = 2400,
      width  = 2000,
      res = 300
    )


    return(
      list(
        algo_pval_wmean = comp_niches$wmean$algo_pval,
        algo_pval_ca    = comp_niches$ca$algo_pval,
        algo_code_wmean = comp_niches$wmean$algo_code,
        algo_code_ca    = comp_niches$ca$algo_code,
        algo_plot_wmean = comp_niches$wmean$algo_plot,
        algo_plot_ca    = comp_niches$ca$algo_plot,
        echelle_pval    = wx_res,
        echelle_plot    = p_echelle
      )
    )
  }
)
