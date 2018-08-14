Normal.elicit <- function(lower, upper, length.CI=0.95) {
  ### Function to elicit mean and variance of Normal distribution
  ### given lower and upper quantile value
  ### length.CI: Length of credible interval 
  
  mean <- (lower + upper)/2

  adjusted.q <- 1 - (1-length.CI)/2
  sd <- (upper - mean) / qnorm(adjusted.q)
  
  return(list(mean=mean, sd=sd))

  
}