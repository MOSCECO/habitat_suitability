dis <- lapply(
  list.files(
    here("data", "analysis", "compilation")
  ),
  \(px) {
    # px <- "probabilite_occurrence"
    out <- lapply(
      list.files(
        here("data", "analysis", "compilation", px)
      ),
      \(supfam) {
        # supfam <- "Majoidea"
        out <- lapply(
          list.files(
            here("data", "analysis", "compilation", px, supfam)
          ),
          \(spe) {
            # spe <- "Mithraculus forceps"
            out <- lapply(
              list.files(
                here("data", "analysis", "compilation", px, supfam, spe),
                full.names = T
              ),
              \(f) {
                rast(f)
              }
            )
            x1 <- list.files(
              here("data", "analysis", "compilation", px, supfam, spe)
            ) %>% str_split("_") %>% lapply(pluck, 2) %>% unlist()
            x2 <- list.files(
              here("data", "analysis", "compilation", px, supfam, spe)
            ) %>% str_split("_") %>% lapply(pluck, 3) %>% unlist()
            names(out) <- paste(x1, x2, sep = "_")
            return(out)
          }
        )
        names(out) <- gsub(" ", "_", list.files(
          here("data", "analysis", "compilation", px, supfam)
        ))
        return(out)
      }
    )
    names(out) <- list.files(
      here("data", "analysis", "compilation", px)
    )
    return(out)
  }
)
names(dis) <- c("pa", "po")

dis$pa
