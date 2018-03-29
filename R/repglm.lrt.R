repglm.lrt <- function(dev1, dev0) {
  #' Function to perform Likelihood Ratio test, given two lists of deviances and their
  #' df as output from repglm.wrapper(..., type="deviance")
  stopifnot(length(dev1) == length(dev0))
  p <- sapply(1:length(dev1), 
              function(i) {
                df <- dev0[[i]][2] - dev1[[i]][2]
                if(df > 0) {
                  return(pchisq(dev0[[i]][1] - dev1[[i]][1], 
                                         df=df, lower.tail = FALSE))
                } else {
                  return(NA)
                }
              })
}
