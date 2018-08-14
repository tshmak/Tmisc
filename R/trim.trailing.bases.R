trim.trailing.bases <- function(A1, A2, keep.identical=TRUE) {
  #' If A1 == "GA", and A2 == "GAA", then this is equivalent to 
  #' A1 == "G", A2 == "GA".
  lastchar <- function(str) {
    nch <- nchar(str)
    return(substr(str, start = nch, stop=nch))
  }
  rm.lastchar <- function(str) {
    nch <- nchar(str)
    return(substr(str, start = 1, stop=nch-1))
  }
  
  if(!keep.identical) {
    PROBS <- lastchar(A1) == lastchar(A2)
  } else {
    PROBS <- lastchar(A1) == lastchar(A2) & A1 != A2
  }
  a1 <- A1[PROBS]
  a2 <- A2[PROBS]
  probs <- lastchar(a1) != lastchar(a2)

  while(any(probs)) {
    a1[probs] <- rm.lastchar(a1[probs])
    a2[probs] <- rm.lastchar(a2[probs])
    probs <- lastchar(a1) == lastchar(a2)
  }
  A1[PROBS] <- a1
  A2[PROBS] <- a2
  return(data.frame(A1=A1, A2=A2))
  
}