bm_PlotEvalMean_gm <- function (bm.out, metric.eval = NULL, dataset = "calibration", 
                                group.by = "algo", do.plot = TRUE, main, xlim, ylim) 
{
  # args <- .bm_PlotEvalMean.check.args(bm.out, metric.eval, 
  #                                     dataset, group.by, ...)
  # for (argi in names(args)) {
  #   assign(x = argi, value = args[[argi]])
  # }
  # rm(args)
  scores <- get_evaluations(bm.out)
  models_mean = tapply(
    X = scores[, dataset], 
    INDEX = list(scores$metric.eval, scores[, group.by]), 
    FUN = mean, 
    na.rm = TRUE
  )
  models_sd = tapply(
    X = scores[, dataset], 
    INDEX = list(scores$metric.eval, scores[, group.by]), 
    FUN = sd, 
    na.rm = TRUE
  )
  ggdat <- merge(data.frame(name = colnames(models_mean), t(models_mean)), 
                 data.frame(name = colnames(models_sd), t(models_sd)), 
                 by = "name")
  colnames(ggdat) <- gsub("\\.x", "_mean", colnames(ggdat))
  colnames(ggdat) <- gsub("\\.y", "_sd", colnames(ggdat))
  limits1 <- aes(
    xmax = get(paste(metric.eval[[1]], "mean", sep = "_")) + 
      get(paste(metric.eval[[1]], "sd", sep = "_")), 
    xmin = get(paste(metric.eval[[1]], "mean", sep = "_")) -
      get(paste(metric.eval[[1]], "sd", sep = "_")), 
    fill = NULL
  )
  limits2 <- aes(
    ymax = get(paste(metric.eval[[2]], "mean", sep = "_")) + 
      get(paste(metric.eval[[2]], "sd", sep = "_")), 
    ymin = get(paste(metric.eval[[2]], "mean", sep = "_")) -
      get(paste(metric.eval[[2]], "sd", sep = "_")), 
    fill = NULL
  )
  gg <- ggplot(
    ggdat, 
    aes(
      x = get(paste(metric.eval[[1]], "mean", sep = "_")), 
      y = get(paste(metric.eval[[2]], "mean", sep = "_")), 
      colour = get("name"), 
      fill = NULL
    )
  ) + 
    geom_point() +
    geom_errorbarh(limits1, height = 0) +
    geom_errorbar( limits2, width = 0) +
    xlab(metric.eval[1]) +
    ylab(metric.eval[2]) +
    theme(
      legend.title = element_blank(), legend.key = element_rect(fill = "white")
    )
  if (length(ylim) > 0 | length(xlim) > 0) {
    gg <- gg + coord_cartesian(ylim = ylim, xlim = xlim)
  }
  if (length(main) > 0) {
    gg <- gg + labs(title = main)
  }
  if (do.plot) {
    print(gg)
  }
  return(list(tab = ggdat, plot = invisible(gg)))
}