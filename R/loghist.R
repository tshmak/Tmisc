loghist <- function(..., plot=TRUE) {
  h <- hist(..., plot=F)
  class(h) <- "loghist"
  if(plot){
    plot(h)
  }
  invisible(h)
}
