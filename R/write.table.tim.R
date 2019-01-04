write.table.tim <- function(..., header=T)  {
  options <- list(...)
  if(is.null(options$quote)) options$quote <- F
  if(is.null(options$col.names)) options$col.names <- header
  if(is.null(options$row.names)) options$row.names <- F
  if(is.null(options$sep)) options$sep <- "\t"
  do.call("write.table", options)
}