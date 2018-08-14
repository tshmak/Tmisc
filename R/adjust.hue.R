adjust.hue <- function(colours, degree) {
  #' Adjust colours. degree < 1 means lighter. > 1 means darker
  if(degree < 0) stop("degree should be > 0; < 1 means lighter; > 1 means darker")
  lighter <- TRUE
  if(degree > 1) {
    degree <- 1/degree
    lighter <- FALSE
  }
  cols <- colours
  RGB <- col2rgb(cols)
  new.cols <- rep(NA, length(cols))
  for(i in 1:length(cols)) {
    args <- RGB[,i]
    if(!lighter) {
      args <- args * degree
    } else {
      args <-  255 - (255 - args) * degree
    }
    args <- as.list(args)
    args$max <- 255
    new.cols[i] <- do.call(rgb, args)
  }
  return(new.cols)
}
