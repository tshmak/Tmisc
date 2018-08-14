nameoption <- function(filename, pos, split=".", fixed=T, ...) {
  #' Function to extract option from a filename
  #' We assume the filename will contain information about
  #' options used in a simiulation, e.g. sim.10.DOM.R
  #' nameoption("sim.10.DOM.R", 2) --> 10
  #' nameoption("sim.10.DOM.R", 3) --> "DOM"

  a <- strsplit(filename, split=split, fixed=fixed, ...)[[1]]
  b <- a[pos]
  if(is.numeric(b)) return(as.numeric(b)) else return(b)

}
