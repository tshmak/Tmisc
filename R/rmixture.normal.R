rmixture.normal <- function(n, p.causal, ...) {
  #' Generate random mixure Normal variables
  #' Mixture of 0 and rnorm(n.causal, ...)
  
  stopifnot(p.causal > 0 && p.causal <= 1)
  beta <- rep(0, n) 
  n.causal <- ceiling(n * p.causal)
  
  beta[sample(n, n.causal)] <- rnorm(n.causal, ...)
  return(beta)
  
}