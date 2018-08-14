plot.qqplot_pvals <- function(obj,
                              square=c(NA, T, F),
                              xlab=expression("-log"[10]*"(Expected p)"),
                              ylab=expression("-log"[10]*"(Observed p)"),
                              add=FALSE,
                              abline=TRUE, cilines=TRUE,
                              ..., abline.options=list(), cilines.options=list()) {

  x <- obj$x[obj$selectvector]
  y <- obj$y[obj$selectvector]


  inf <- is.infinite(y)
  if(any(inf)) {
    x <- x[!inf]
    y <- y[!inf]
    warning("Some p values are zeros and not plotted")

  }
  maxx <- max(x[!inf])
  maxy <- max(y[!inf])

  square <- square[1]
  if(is.na(square)) {
    square <- maxy / maxx < 2
  }

  if(square) {
    ylim <- xlim <- c(0, max(maxx, maxy))
  } else {
    xlim <- c(0, maxx)
    ylim <- c(0, maxy)
  }

  if(!add) {
    if(square) {
      save.pty <- par("pty")
      par(pty="s")
    }
    plot(x,y,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim, ...)
    if(abline) {
      abline.options$a <- 0
      abline.options$b <- 1
      do.call("abline", abline.options)
    }
    if(cilines) {
      cilines.options$x <-obj$x[obj$selectvector]
      cilines.options$y <- obj$l
      do.call("lines", cilines.options)
      cilines.options$y <- obj$u
      do.call("lines", cilines.options)
    }
    if(square) {
      par(pty=save.pty)
    }

  } else {
    points(x,y,...)
  }


}
