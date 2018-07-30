startParallel <- function(nthreads) {
  if(missing(nthreads)) {
    if(interactive()) {
      input <- readline("number of threads? ")
    } else {
      input <- parseargs()$nthreads
    }

    if(length(input) == 0) {
      nthreads <- 1
    } else {
      if(!grepl("[0-9]*", input)) {
        stop("It seems that nthreads is not an integer.")
      }
      nthreads <- as.integer(input)
    }
  }

  max.threads <- parallel::detectCores()
  if(nthreads > max.threads) {
    stop(paste("nthreads exceed the maximum number of cores available, which is:",
               max.threads))
  }

  return(parallel(nthreads))

}
