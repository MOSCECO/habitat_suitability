mae <- Sapply(
  list.files(popa_path),
  \(px) {
    # px <- "adequation_environnementale"
    # px <- "presence_absence"
    out <- Sapply(
      list.files(
        here(popa_path, px)
      ),
      \(supfam) {
        # supfam <- "Majoidea"
        out <- Sapply(
          list.files(
            here(popa_path, px, supfam)
          ),
          \(spe) {
            # spe <- "Mithraculus forceps"
            print(spe)
            out <- Sapply(
              list.files(
                here(popa_path, px, supfam, spe),
              ),
              \(ens_alg) {
                # ens_alg <- "ca"
                print(ens_alg)
                out <- Sapply(
                  list.files(
                    here(
                      popa_path,
                      px, supfam, spe, ens_alg
                    ),
                    full.names = T
                  ),
                  \(f) {
                    out <- if (px == "adequation_environnementale") {
                      rast(f)
                    } else {
                      out_pa <- Sapply(
                        list.files(f, full.names = T), rast,
                        simplify = F, USE.NAMES = T
                      )
                      out_pa <- Reduce(c, out_pa)
                      t <- list.files(f) %>%
                        str_split("_") %>%
                        unlist() %>%
                        table()
                      names(out_pa) <- names(t)[t == 1] %>%
                        paste(names(out_pa), sep = ".")
                      out_pa <- as.list(out_pa)
                      names(out_pa) <- names(t)[t == 1]
                      out_pa
                    }
                    return(out)
                  },
                  simplify = F,
                  USE.NAMES = T
                )
                out <- if(px == "adequation_environnementale") {
                  Reduce(c, out)
                  t <- list.files(here(
                    popa_path,
                    px, supfam, spe, ens_alg
                  )) %>% str_split("_") %>% unlist() %>% table()
                  names(out) <- names(t)[t == 1]
                  out <- as.list(out)
                  names(out) <- names(t)[t == 1]
                  out
                } else {
                  names(out) <- list.files(
                    here(popa_path,
                         px, supfam, spe, ens_alg))
                  out <- purrr::transpose(out)
                  Sapply(out, \(x) Reduce(c, x))
                }
                return(out)
              })
          })
      })
  })
