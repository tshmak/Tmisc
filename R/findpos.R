findpos <- function(logical.vector) {
  ## Function to find position of TRUEs in a logical vector 
  res <- (1:length(logical.vector))[logical.vector]
  return(res[!is.na(res)])
}