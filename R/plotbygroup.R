plotbygroup <- function(x,y, by, ...) {
  ## plot with a "by" stratification
  stopifnot(length(x)==length(y) && length(x)==length(by))
  
  plot(x,y,type="n")
  levels <- unique(by)
  
  for(i in levels) {
    points(x,y,)
  }
  
}