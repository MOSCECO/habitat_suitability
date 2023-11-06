Mapply <- function(
    FUN, ..., MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE
  ) {
  mapply(
    FUN, ..., MoreArgs = MoreArgs,
    SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES
  )
}
