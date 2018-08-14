offdiags <- function(matrix, upper=T, both=F) {
  ### Function to extract off-diagonal elements of a square matrix
  if(both) return(c(matrix[upper.tri(matrix)], matrix[lower.tri(matrix)]))
  else if(upper) return(matrix[upper.tri(matrix)])
  else return(matrix[lower.tri(matrix)])
}