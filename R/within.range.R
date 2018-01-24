within.range <- function(x, x2=NULL, lower, upper, 
                         inclusive=TRUE, 
                         OR=TRUE, reference=FALSE, 
                         no.warning=FALSE) {
  #' Function to determine if x falls within a range
  
  stopifnot(length(lower) == length(upper))
  stopifnot(all(lower <= upper))
  if(inclusive) {
    wr <- function(i) return(i >= lower & i <= upper)
  } else {
    wr <- function(i) return(i > lower & i < upper)
  }
  
  if(!is.null(x2)) {
    input.range <- TRUE
    stopifnot(length(x2) == length(x))
  } else {
    input.range <- FALSE
  }
  
  if(length(x) == 1) {
    if(!input.range) {
      result <- wr(x)
    } else {
      r1 <- wr(x)
      r2 <- wr(x2)
      if(OR) {
        result <- r1 | r2
      } else {
        result <- r1 & r2
      }
    }
    
    if(any(is.na(result)) && !no.warning) warning("NAs encountered")
    
    if(!reference) {
      return(any(x))
    } else {
      return(result)
    }
      
  } else {
    if(length(lower) == 1) {
      if(!input.range) {
        result <- wr(x)
      } else {
        r1 <- wr(x)
        r2 <- wr(x2)
        if(OR) {
          result <- r1 | r2
        } else {
          result <- r1 & r2
        }
      }
      
      if(any(is.na(result)) && !no.warning) warning("NAs encountered")
      
      if(!reference) {
        return(result)
      } else {
        return(any(result))
      }
    } else {
      if(length(lower) < length(x)) {
        res <- lapply(1:length(lower), function(i) {
          within.range(x, x2, lower[i], upper[i], 
                       inclusive=inclusive, 
                       OR=OR, reference=reference, 
                       no.warning=TRUE)
        })
        if(any(is.na(unlist(res))) & !no.warning) {
          warning("NAs encountered")
        }
        
        if(!reference) {
          res$na.rm <- TRUE
          result <- as.logical(do.call("pmax", res))
        } else {
          result <- sapply(res, any)
        }
        return(result)

      } else {
        res <- lapply(1:length(x), function(i) {
          within.range(x[i], x2[i], lower, upper, 
                       inclusive=inclusive, 
                       OR=OR, reference=reference, 
                       no.warning=TRUE)
        })
        if(any(is.na(unlist(res))) & !no.warning) {
          warning("NAs encountered")
        }
        
        if(!reference) {
          result <- sapply(res, any)
        } else {
          res$na.rm <- TRUE
          result <- as.logical(do.call("pmax", res))
        }
        return(result)
      }
      
    }
  }
  
}

