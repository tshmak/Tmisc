r2t <- function(r, n) {
  #### Function to convert Pearsons' correlation to t statistic
  return(r*sqrt((n - 2)/(1-r^2)))
}