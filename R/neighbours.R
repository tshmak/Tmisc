neighbours <- function(i, plusminus=3, min=1, max=Inf) {
  #' Returns a sequence of number centred at a number i
  x <- seq(max(c(min, i - plusminus)), min(min(c(max, i + plusminus))))
  return(x)
}