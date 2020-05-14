polar2cart <- function(theta, radius=1) {
  #' c.f. https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates
  #' for x >= 0
  stopifnot(theta >= 0 & theta <= 1) # theta is a scaled angle measure from 0-1
  stopifnot(radius >= 0)
  cos <- cos(theta * pi * 0.5)
  sin <- sin(theta * pi * 0.5)
  n <- length(theta)
  x <- rep(radius, n+1)
  if(n > 1) {
    for(i in 1:(n-1)) {
      m <- rep(1, n+1)
      m[(1:(n+1)) > i] <- sin[i]
      x <- x * m
    }
  }
  x <- x * c(cos, sin[n])
  return(x)
}

