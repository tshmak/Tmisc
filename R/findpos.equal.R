findpos.equal <- function(input, within, firstonly=FALSE, lastonly=FALSE) {
  #' Basically findpos(input == within) but maintain the order of input
  
  res <- lapply(input, function(x) {
    f <- findpos(within == x, firstonly=firstonly, lastonly=lastonly)
    if(length(f) == 0) f <- NA
    return(f)
  })
  lens <- sapply(res, length)
  if(any(lens > 1)) stop("Multiple matches found in some inputs.")
  
  return(as.findpos(unlist(res), length(within)))
  
}