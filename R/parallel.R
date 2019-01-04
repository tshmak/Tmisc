parallel <- function(nclusters) {
  library(parallel)
  if(is.null(nclusters)) return(NULL) 
  return(makeCluster(nclusters, type="FORK", outfile=""))
}
