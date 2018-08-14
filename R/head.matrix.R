head.matrix <- function(Matrix, nrow=10, ncol=6, strip.names=TRUE) {
  ### A head() function for matrices.
  nrows <- min(nrow(Matrix),nrow)
  ncols <- min(ncol(Matrix),ncol)
  out <- Matrix[1:nrows, 1:ncols]
  if(strip.names) {
    colnames(out) <- NULL
    rownames(out) <- NULL
  }
  return(out)
}