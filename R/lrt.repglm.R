lrt.repglm <- function(x, ...) {
  #' Function to do per-variant likelihood-ratio test
  if(is.vector(x)) {
    ncols <- 1
    x <- matrix(x, ncol=1)
  }
  ncols <- ncol(x)
  if(ncols == 1) return(repglm.wrapper(x=x, ..., type="lrt.p"))
  
  d <- as.matrix(data.frame(lapply(1:ncols, function(i) (1:ncols)[-i])))
  L <- lapply(1:nrow(d), function(i) x[,d[i,], drop=F])
  dev <- repglm.wrapper(x=L, ..., type="deviance")
  full <- repglm.wrapper(x=x[,1,drop=F], covar=x[,-1, drop=F], ..., 
                         type="deviance")
  p <- sapply(dev, 
              function(X) pchisq(q=X[1] - full[[1]][1],
                                    df=X[2] - full[[1]][2], 
                                    lower.tail = F))
  return(p)
}
