p2z <- function(p, two.tailed=T) {
  if(two.tailed) p <- p/2
  return(qnorm(p, lower.tail = F))
}