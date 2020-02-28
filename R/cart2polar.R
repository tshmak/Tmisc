cart2polar <- function(x) {
  stopifnot(x >= 0 & length(x) > 1)
  n <- length(x)
  x2 <- rev(x^2)
  x3 <- rev(sqrt(cumsum(x2)))
  radius <- x3[1]
  theta <- acos(x / x3)
  theta[is.nan(theta)] <- 0 # if x3 == 0
  theta <- theta / (pi/2)
  return(list(theta=theta[1:(n-1)], radius=radius))
}

