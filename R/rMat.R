rMat <- function(nrow=1, ncol=1) {
  #' A random matrix of rnorm() 
  return(matrix(rnorm(nrow*ncol), nrow=nrow, ncol=ncol))
}