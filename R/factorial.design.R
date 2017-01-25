factorial.design <- function(nfactors, levels=c(0,1)) {
  stopifnot(nfactors >= 1)
  nlevels <- length(levels)
  X <- matrix(0, nlevels^nfactors, nfactors)
  Levels <- levels
  for(i in 1:nfactors) {
    X[,i] <- Levels %x% rep(1, nlevels^(nfactors - i))
    Levels <- rep(1, nlevels) %x% Levels
  }
  return(X)
}
