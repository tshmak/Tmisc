svdpcomppred <- function(X, n.comps=3) {
  ## Obtain X %*% b, where b (column vector) is 
  ## i-th principal component of XX'
  ## n.comps gives number of components
  n.comps <- min(nrow(X), n.comps)
  svd <- svd(X, nu=n.comps, nv=0)
  return(svd$u %*% diag(svd$d[1:n.comps]))
}