z2p <- function(z, two.tailed=T) {
  p <- pnorm(abs(z), lower.tail = F)
  if(two.tailed) p <- p * 2
  return(p)
}