corrlattice <- function(data, skip=1, size=100, Steps=10, 
                        start.col=NULL, nodraw=F, r2=F, 
                        abs=F, corr=NULL, random=F) {

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
  if(is.null(corr)) {
    selected <- data[,selecao]
    corr <- cor(selected, use="pairwise.complete.obs")
  }
  else {
    corr <- as.matrix(corr[selecao, selecao])
  }

  col.regions <- colorRampPalette(c("blue", "white", "red"))
  colnames(corr) <- rownames(corr) <- NULL
  at <- seq(from=-1.05,to=1.05,by=0.1)
  if(r2) corr <- corr^2
  else if(abs) {
    corr <- abs(corr)
    col.regions <- colorRampPalette(c("white", "red"))
    at <- seq(from=-0.05,to=1.05,by=0.1)
  }
  
  if(!nodraw) {
    toplot <- lattice::levelplot(corr, main="", xlab="", ylab="", 
            col.regions=col.regions, at=at)
    lattice::plot.levelplot(toplot)
  }
  return(invisible(corr))
  
}
