corrlattice <- function(data, size=Inf, 
                        nodraw=F, r2=F, abs=F, 
                        maxsize=300, use="pairwise.complete.obs", 
                        tick.number=10) {

  if(size > ncol(data)) {
    size <- ncol(data)
  } 
  
  if(size > maxsize) stop(paste(
    "The size of", size, "may be too big. Set it to less than", maxsize
  ))

  selected <- round(seq(1, ncol(data), length=size))
  
  if(!isSymmetric(data)) {
    corr <- cor(data[,selected], use=use) 
  } else {
    corr <- data[selected,selected]
  }
  
  col.regions <- grDevices::colorRampPalette(c("blue", "white", "red"))
  # colnames <- colnames(corr)
  # if(is.null(colnames)) {
  #   colnames <- (1:ncol(data))[selected]
  # }
  
  if(tick.number > size) tick.number <- size
  ticks <- round(seq(1, size, length=tick.number))

  at <- seq(from=-1.05,to=1.05,by=0.1)
  if(r2 || abs) {
    if(r2) {
      corr <- corr^2
    } else if(abs) {
      corr <- abs(corr)
    }
    col.regions <- grDevices::colorRampPalette(c("white", "red"))
    at <- seq(from=-0.05,to=1.05,by=0.1)
  } 
  
  if(!nodraw) {
    toplot <- lattice::levelplot(corr, main="", 
                                 xlab="", ylab="", 
                                 scales=list(at=ticks), 
                                 col.regions=col.regions, at=at)
    plot(toplot)
  }
  return(invisible(corr))
  
}
