RandomImputeByColumn <- function(data) {
  ## Function to impute a matrix with missing data with a random pick 
  ## from the same column
  for(i in 1:ncol(data)) {
    selected <- data[,i]
    replacevec <- is.na(selected)
    n.replace <- sum(replacevec)
    if(n.replace > 0) data[,i][replacevec] <- 
      sample(selected[!replacevec],n.replace, replace=T) 
  }
  return(data)
}
