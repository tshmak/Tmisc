findpos <- function(vec, within=NULL, firstonly=FALSE, lastonly=FALSE) {
  #' Function to find position of TRUEs in a logical vector. 
  #' You can optionally put in a "within" vector to find the positions
  #' within the "within" vector. 
  #' @param vec A logical vector or a findpos object
  #' @param within A logical vector or a findpos object
  stopifnot(!firstonly || !lastonly)
  if(class(vec) == "findpos") {
    vec.len <- attr(vec, "vector.length")
  } else if(is.logical(vec)) {
    vec.len <- length(vec)
  } else {
    stop("vec must be either a findpos object or a logical vector")
  }

  if(!is.null(within)) {
    within.pos <- findpos(within)
    stopifnot(vec.len == length(within.pos))
    vec.len <- attr(within.pos, "vector.length")
    res <- within.pos[vec]
  } else if(class(vec) == "findpos") {
    res <- vec
  } else if(is.logical(vec)) {
    res <- which(vec)
  }
  
  if(firstonly) res <- res[1] else if(lastonly) res <- res[length(res)]
  attr(res, "vector.length") <- length(vec)
  class(res) <- "findpos"
  return(res)
  
}
