plot.qqplot_pvals <- function(obj,
                              asp=1, 
                              xlab=expression("Expected -log"[10]*"(p)"),
                              ylab=expression("Observed -log"[10]*"(p)"),
                              add=FALSE, 
                              ..., abline.options=list(), cilines.options=list()) {
  
  x <- obj$x[obj$selectvector]
  y <- obj$y[obj$selectvector]
  
  inf <- is.infinite(y)
  if(any(inf)) {
    x <- x[!inf]
    y <- y[!inf]  
    warning("Some p values are zeros and not plotted")

  }
  
  if(!add) plot(x,y, 
                asp=asp,xlab=xlab,ylab=ylab,...) else
          points(x,y, 
               asp=asp,xlab=xlab,ylab=ylab,...)
  abline.options$a <- 0
  abline.options$b <- 1
  do.call("abline", abline.options)
  cilines.options$x <-obj$x[obj$selectvector]
  cilines.options$y <- obj$l
  do.call("lines", cilines.options)
  cilines.options$y <- obj$u
  do.call("lines", cilines.options)

}