aggregationModels <- function(models, algos, fs, do_plot = F) {
  lapply(
    algos, 
    \(alg) {
      # alg <- "ca"
      # models <- p_masque
      l <- lapply(names(models), \(m) models[[m]][[alg]]$ras)
      # print(l)
      out_fs <- sapply(
        # fs, \(f) {print(f); Reduce(eval(parse(text = f)), l)}, simplify = F, USE.NAMES = T
        fs, \(f) {app(Reduce(c, l), f)}, simplify = F, USE.NAMES = T
      )
      
      if (do_plot) {
        lapply(names(out_fs), \(n) plot(out_fs[[n]], main = paste(alg, n)))
      }
      
      return(out_fs)
    }
  )
}