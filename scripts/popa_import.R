dis <- sapply(
  list.files(
    here("data", "analysis", "compilation")
  ),
  \(px) {
    # px <- "adequation_environnementale"
    out <- sapply(
      list.files(
        here("data", "analysis", "compilation", px)
      ),
      \(supfam) {
        # supfam <- "Majoidea"
        out <- sapply(
          list.files(
            here("data", "analysis", "compilation", px, supfam)
          ),
          \(spe) {
            # spe <- "Mithraculus forceps"
            print(spe)
            out <- sapply(
              list.files(
                here("data", "analysis", "compilation", px, supfam, spe),
              ),
              \(ens_alg) {
                # ens_alg <- "ca"
                print(ens_alg)
                out <- sapply(
                  list.files(
                    here(
                      "data", "analysis", "compilation",
                      px, supfam, spe, ens_alg
                    ),
                    full.names = T
                  ),
                  \(f) {
                    out <- if (px == "adequation_environnementale") {
                      rast(f)
                    } else {
                      sapply(
                        list.files(f, full.names = T), rast,
                        simplify = F, USE.NAMES = T
                      )
                    }
                    return(out)
                  },
                  simplify = F,
                  USE.NAMES = T
                )
                out <- Reduce(c, out)
                t <- list.files(here(
                  "data", "analysis", "compilation",
                  px, supfam, spe, ens_alg
                )) %>% str_split("_") %>% unlist() %>% table()
                names(out) <- names(t)[t == 1]
                out <- as.list(out)
                names(out) <- names(t)[t == 1]
                return(out)
              },
              simplify = F, USE.NAMES = T
            )
          },
          simplify = F, USE.NAMES = T
        )
      },
      simplify = F, USE.NAMES = T
    )
  },
  simplify = F, USE.NAMES = T
)
