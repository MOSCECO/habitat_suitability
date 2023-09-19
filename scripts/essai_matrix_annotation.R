p <- ggplot(
  data = mm %>% filter(variable != "slope"),
  aes(
    x     = alg_mod,
    y     = value,
    col   = alg_mod
  )
) +
  geom_boxplot() +
  # geom_violin() +
  # geom_jitter() +
  facet_wrap(vars(variable))

at <-   c(
  c("\t", "\t", colnames(d)), "\n",
  c(row.names(d)[1], "\t", d[1, 1],"\t", "\t",  d[1, 2], "\t", d[1, 3]), "\n",
  c(row.names(d)[2],"\t",  "\t", d[2, 1], "\t", "\t", d[2, 2],"\t",  d[2, 3]), "\n",
  c(row.names(d)[3],"\t",  "\t", d[3, 1],"\t", "\t",  d[3, 2], "\t",  d[3, 3])
) %>% paste0(collapse = " ")


char_string <- capture.output(
  write.table(d, sep = "\t  ", quote = FALSE)
)
char_string[1] <- paste("\t", char_string[1], sep = "\t")
char_string[2] <- gsub("\t", "\t\t", char_string[2])
char_string[3] <- sub("\t", "\t\t\t", char_string[3])
char_string[4] <- gsub("\t", "\t\t", char_string[4])
char_string <- paste(char_string, collapse = "\n")
cat(char_string)

ann_text <- data.frame(
  algo_mod = "Observations",
  value = -10,
  label = toString(seuils_test_code$stdv.chla),
  variable = factor(
    "stdv.chla",
    levels = levels(
      factor(
        mm %>% filter(variable != "slope") %>% select(variable) %>% unlist()
      )
    )
  )
)
p + geom_text(data = ann_text, aes(label = label))
