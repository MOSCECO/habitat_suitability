# Réduction par temps de projection
dis_reduc <- Sapply(
  names(mae),
  \(px) {
    # px <- "adequation_environnementale"
    Sapply(
      names(mae[[px]]),
      \(supfam) {
        # supfam <- "Majoidea"
        Sapply(
          names(mae[[px]][[supfam]]),
          \(bn) {
            # bn <- "Amphithrax hemphilli"
            Sapply(
              names(mae[[px]][[supfam]][[bn]]),
              \(ens_alg) {
                # ens_alg <- "ca"
                Reduce(c, mae[[px]][[supfam]][[bn]][[ens_alg]])
              })
          })
      })
  })

pa_wmean_roc_current <- popaPlot(
  projRasters         = mae,
  type                = "presence_absence",
  superfamily         = "all",
  ensemble_algorithm  = "wmean",
  threshold_algorithm = "ROC",
  projection_time     = "current"
)
dev.off()

ae_wmean_current <- popaPlot(
  projRasters         = mae,
  type                = "adequation_environnementale",
  superfamily         = "all",
  ensemble_algorithm  = "wmean",
  projection_time     = "current"
)
dev.off()

