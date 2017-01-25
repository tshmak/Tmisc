rcov <- function(Sigma, n) {
  #' Random covariance matrices
  #' @param Sigma True covariance matrix
  
  stopifnot(isSymmetric(Sigma))
  dim <- dim(Sigma)[1]
  stopifnot(nrow(Sigma) == ncol(Sigma))
  E <- eigen(Sigma) 
  thres <- .Machine$double.eps * dim
  stopifnot(all(-E$values < thres))
  E.values <- E$values
  E.values[E.values < thres] <- 0
  
  if(n < dim) {
    dat <- matrix(rnorm(n*dim), n, dim)  
    cov1 <- cov(dat)
  } else {
    cov1 <- rWishart(1, n, diag(dim))[,,1, drop=T] / n
  }
  sqrt <- sqrt(E.values)
  return(E$vectors %*% Matrix::Diagonal(x=sqrt) %*% cov1 %*% 
           Matrix::Diagonal(x=sqrt) %*% t(E$vectors))
}