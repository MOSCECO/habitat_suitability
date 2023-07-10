replaceColumnInTable <- function(tb1, tb2, col_rcv, col_cmn, col_prv) {
  tb3 <- left_join(tb1, tb2, by = col_cmn)
  tb1[is.na(tb1[[col_rcv]]), col_rcv] <- as.data.frame(tb3)[is.na(tb1[[col_rcv]]), col_prv]
  return(tb1)
}