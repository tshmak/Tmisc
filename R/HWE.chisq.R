HWE.chisq <- function(counts) {
  ### Hardy Weinberg Equilibrium tests
  ### counts: Table of counts of aa, Aa, AA
  freq <- c(0,1,2)
  n <- sum(counts)
  p <- sum(counts * freq) / (n*2)

  q <- 1 - p
  pvec <- c(q^2, 2*p*q, p^2)
  E <- n * pvec

  chisq <- sum((counts-E)^2 / E)
  p <- pchisq(chisq, 1, lower.tail=F)
  return(p)
}