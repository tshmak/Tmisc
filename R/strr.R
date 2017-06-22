strr <- function(object, nmax=10, tail=F) {
  #' Function to examine the object like str
  #' but will try to avoid giving displays that's too long
  
  if(!tail) {
    cols <- 1:nmax
    description <- "first"
  } else {
    if(is.list(object)) {
      len <- length(object)
      cols <- max(c(len-nmax+1,1)):len
      description <- "last"
    }
  }
  
  if(is.data.frame(object)) {
    ncol <- ncol(object)
    if(ncol > nmax) {
      str(object[,cols])
      cat(paste("data.frame with", ncol, 
                "columns. Only", description, nmax, "columns shown\n"))
    } else {
      str(object, 1)
    }
  } else if(is.list(object)) {
    len <- length(object)
    if(len > nmax) {
      str(object[cols], 1)
      cat(paste("list with", len, "elements. Only", description, nmax, "shown.\n"))
    } else {
      str(object, 1)
    }
  } else {
    str(object, 1)
  }
}