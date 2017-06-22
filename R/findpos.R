findpos <- function(vec, within=NULL) {
  #' Function to find position of TRUEs in a logical vector. 
  #' You can optionally put in a "within" vector to find the positions
  #' within the "within" vector. 
  #' @param vec A logical vector or a findpos object
  #' @param within A logical vector or a findpos object
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
    return(as.findpos(within.pos[vec], attr(within.pos, "vector.length")))
  } else if(class(vec) == "findpos") {
    return(vec)
  } else if(is.logical(vec)) {
    res <- which(vec)
    attr(res, "vector.length") <- length(vec)
    class(res) <- "findpos"
    return(res)
  }
}
