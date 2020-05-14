box2tri.aux <- function(n_minus_1) {
  #' Auxiliary function of box2tri
  n <- n_minus_1 + 1
  M <- matrix(0, nrow=n, ncol=ceiling(log2(n)))
  i <- 1:n
  k <- 1
  for(j in 1:ncol(M)) {
    # j <- 1
    blocksize <- 2^(j-1)
    l <- 1
    while(n - l + 1 > blocksize) {
      tofill <- rep(c(k,-k), c(blocksize,blocksize))
      remaining0 <- n - l + 1
      filllength <- min(remaining0, 2*blocksize)
      M[l:(l+filllength-1), j] <- tofill[1:filllength]
      l <- l + 2*blocksize
      k <- k + 1
    }
  }
  result <- lapply(1:nrow(M), function(i) {
    l <- as.vector(M[i,])
    return(l[l != 0])
  })
  return(result)
}

box2tri <- function(x, L=box2tri.aux(length(x))) {
  #' Gives a one-to-one transformation of a hypercube to the hyperplane where 
  #' sum(x) = 1
  #' Pre-compute L to increase speed in optimization
  stopifnot(x >= 0 & x <= 1)
  xc <- 1 - x
  y <- rep(NA, length(x)+1)
  for(i in 1:length(y)) {
    ii <- L[[i]]
    y[i] <- prod(c(x[ii[ii>0]], xc[-ii[ii<0]])) 
  }
  return(y)
}
