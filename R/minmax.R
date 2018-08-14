minmax <- function(..., na.rm=TRUE) {
  #' Function to get min and max for a bunch of objects (all coerced to vectors)
  #' to set xlim and ylim in plot
  
  a <- numeric(0)
  things <- list(...)
  for(i in 1:length(things)) {
    a <- c(a, as.vector(things[[i]]))
  }
  return(c(min(a, na.rm=na.rm), max(a, na.rm = na.rm)))
}