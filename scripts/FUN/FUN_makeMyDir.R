# fonction création fichier
makeMyDir <- function(path, del = FALSE) {
  if(!dir.exists(path)) {
    dir.create(path)
  } else {
    if (del) {
      unlink(path, recursive = TRUE)
      dir.create(path)
    }
  }
} 