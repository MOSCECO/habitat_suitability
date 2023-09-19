p <- ggplot(data, aes(x=WS, y=DV, group=count))
p <- p + geom_boxplot(aes(fill=factor(count), group=interaction(WS, count)))
p <- p + stat_summary(fun.y=median, geom="smooth", aes(group=factor(count), color =factor(count)))
p <- p + scale_x_continuous(breaks = c(2,6,14))

p.values <- sapply(
  split(data, data$WS), function(x){wilcox.test(DV~count, x)$p.value}
)
labels   <- lapply(
  res_wxtest %>% lapply(pluck, "p.value"),
  symnum,
  corr      = FALSE,
  cutpoints = c(    0, .001, .01, .05, 1),
  symbols   = c("***", "**", "*", "n.s."),
  lower.triangular = T
)
y.values <- sapply(
  mm_split,
  function(x) {
    # x <- mm_split[[1]]
    max(
      sapply(
        split(x, x$alg_mod),
        function(xx){
          boxplot(x$value, plot = F)$stats[5, ]
        }
      )
    )
  }
) + 2

p <- ggplot(
  data = mm %>% filter(variable != "slope"),
  aes(
    x = variable,
    y = value,
    col = alg_mod
  )
) +
  geom_boxplot() +
  geom_signif(
    y_position  = y.values[-13],
    xmin        = unique(mm$variable)[-13],
    xmax        = unique(mm$variable)[-13],
    annotations = labels[-13]
  )
p
