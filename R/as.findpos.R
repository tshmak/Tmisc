as.findpos <- function(positions, vector.length) {
  #' Turn an integer vector into a findpos object
  attr(positions, "vector.length") <- vector.length
  class(positions) <- "findpos"
  return(positions)
}