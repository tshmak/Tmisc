head.matrix <- function(Matrix, nrow=10, ncol=5) {
  ### A head() function for matrices.
  nrows <- min(nrow(Matrix),nrow)
  ncols <- min(ncol(Matrix),ncol)
  return(Matrix[1:nrows, 1:ncols])
}