parallel <- function(nclusters, test.nclusters=2) {
  library(parallel)
  if(interactive()) nclusters <- test.nclusters
  return(makeCluster(nclusters, type="FORK", outfile=""))
} 