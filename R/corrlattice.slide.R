corrlattice.slide <- function(data, skip=1, size=100, Steps=10, 
                        start.col=NULL, random=F, ...) {

  startexists <- with(.GlobalEnv, exists(".corrlattice.start", inherits=F))
  if(!is.null(start.col)) {
    .corrlattice.start <- start.col
  }
  else if(random) {
    if(is.null(corr)) ncol <- ncol(data)
    else ncol <- ncol(corr)
    max.start <- ncol - skip*(size-1)
    .corrlattice.start <- ceiling(max.start * runif(1))
  }
  else if(startexists) {
    .corrlattice.start <- with(.GlobalEnv, .corrlattice.start)
  }
  else {
    .corrlattice.start <- 1
  }
  .corrlattice.start <<- .corrlattice.start + skip * Steps
  
  selecao <- seq(from=.corrlattice.start, by=skip, length.out=size)
  if(!isSymmetric(data)) {
    return(corrlattice(data[,selecao], ...))
  }
  else {
    return(corrlattice(data[selecao,selecao], ...))
  }

}
