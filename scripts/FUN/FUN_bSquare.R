bSquare <- function(x, a) {
  a <- sqrt(a)/2
  return(
    sf::st_buffer(
      x, 
      dist = a, 
      # nQuadSegs=1, 
      endCapStyle = "SQUARE"
    ) 
  )
}