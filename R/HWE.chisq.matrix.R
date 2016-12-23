HWE.chisq.matrix <- function(X) {
  ### Hardy Weinberg Equilibrium tests (for matrix)
  ### counts: Table of counts of aa, Aa, AA
  colSum <- colSums(X)
  nrow <- nrow(X)
  p <- colSum / (nrow*2)
  q <- 1 - p
  count0 <- colSums(X == 0)
  count1 <- colSums(X == 1)
  count2 <- colSums(X == 2)
  p2 <- p^2
  pq2 <- 2 * p * q
  q2 <- q^2
  
  E <- rbind(q2, pq2,p2) * nrow
  O <- rbind(count0, count1, count2)
# print(rbind(p,q))
# print(rbind(q2, pq2,p2))
# print(E)
# print(O)
  chisq <- (O - E)^2/E
  chisq <- colSums(chisq)

  p <- pchisq(chisq, 1, lower.tail=F)
  return(p)
}