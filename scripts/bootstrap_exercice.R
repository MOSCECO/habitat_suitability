# Tentative de bootstrap

require(tibble)

# Données
d <- read.csv("data/raw/body_fat.csv")
hist(d$X.Fat, breaks = 20)

n_resamp <- 10000
b <- lapply(
  X   = seq(1, n_resamp),
  FUN = \(x) {
    sample(
      x       = d$X.Fat,
      size    = length(d$X.Fat),
      replace = TRUE
    )
  }
)
tb <- Reduce(cbind, b) %>%
  as_tibble()
m <- apply(tb, 2, mean)
hist(m, breaks = 100)

quantile(m, probs = c(0.25, 0.975))

# Test avec noms d'espèces
d <- species$species
n_resamp <- 10000
b <- lapply(
  X   = seq(1, n_resamp),
  FUN = \(x) {
    sample(
      x       = d,
      size    = length(d),
      replace = TRUE
    )
  }
)
tb <- Reduce(cbind, b) %>%
  as_tibble()
m <- apply(tb, 2, \(x) length(unique(x)))
hist(m)

quantile(m, probs = c(0.25, 0.975))
