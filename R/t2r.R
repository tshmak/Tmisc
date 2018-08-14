t2r <- function(t, n) {
  #### Function to convert t statistic to Pearsons' correlation 
  return(t/sqrt(n - 2 + t^2))
}