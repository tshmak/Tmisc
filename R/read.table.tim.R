read.table.tim <- function(...)  {
  options <- list(...)
  if(is.null(options$header)) {
    options2 <- options
    options2$stringsAsFactors <- NULL
    options2$nrows <- NULL
    options2$skip <- NULL
    options$header <- do.call("check.header", options2)
  }
  if(is.null(options$stringsAsFactors)) options$stringsAsFactors <- F
  return(do.call("read.table",options))
}