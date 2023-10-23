lapply(
  list.files(here(pth, "models", "Ste.set.ens5.01.global.cpc"), full.names = T),
  \(x) {
    # x <- list.files(here(pth, "models", "Ste.set.ens5.01.global.cpc"), full.names = T)[[1]]
    m <- get(load(x))
    m@dir_name <- pathClusterBiomod(m@dir_name)
    save(m)
  }
)

