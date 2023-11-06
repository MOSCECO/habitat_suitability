# Réduction par temps de projection
dis_reduc <- Sapply(
  names(dis),
  \(px) {
    # px <- "adequation_environnementale"
    Sapply(
      names(dis[[px]]),
      \(supfam) {
        # supfam <- "Majoidea"
        Sapply(
          names(dis[[px]][[supfam]]),
          \(bn) {
            # bn <- "Amphithrax hemphilli"
            Sapply(
              names(dis[[px]][[supfam]][[bn]]),
              \(ens_alg) {
                # ens_alg <- "ca"
                Reduce(c, dis[[px]][[supfam]][[bn]][[ens_alg]])
              })
          })
      })
  })

# presence-absence | committee averaging
popaPlot(
  projRasters        = dis,
  type               = "adequation_environnementale",
  superfamily        = "all",
  ensemble_algorithm = "ca",
  projection_time    = "ssp585"
)

# presence-absence | weighted mean
pa <- dis$pa
pawm <- Reduce(
  c,
  lapply(
    pa,
    \(y) {
      z <- y %>%
        lapply(pluck, 2) %>%
        unlist()
      Reduce(c, z)
    }
  )
)
x11(); plot(app(pawm, sum))

# probability of occurrences | committee averaging
po <- dis$po
poca <- Reduce(
  c,
  lapply(
    po,
    \(y) {
      z <- y %>%
        lapply(pluck, 1) %>%
        unlist()
      Reduce(c, z)
    }
  )
)
x11(); plot(app(poca, sum))

# probability of occurrences | committee averaging
po <- dis$po
powm <- Reduce(
  c,
  lapply(
    po,
    \(y) {
      z <- y %>%
        lapply(pluck, 2) %>%
        unlist()
      Reduce(c, z)
    }
  )
)
x11(); plot(app(powm, sum))

# all graphs
x11(); par(mfrow = c(1,4)); plot(app(powm, sum)); plot(app(pawm, sum)); plot(app(poca, sum)); plot(app(paca, sum))
