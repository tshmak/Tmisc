within.window <- function(x, center, window, inclusive=TRUE) {
  #' Returns a logical vector indicating if x falls within a 
  #' window of center.
  if(inclusive) {
    return(x >= center - window & x <= center + window)
  } else {
    return(x > center - window & x < center + window)
  }
}