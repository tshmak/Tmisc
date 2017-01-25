write.table.tim <- function(...)  {
  options <- list(...)
  if(is.null(options$quote)) options$quote <- F
  if(is.null(options$row.names)) options$row.names <- F
  do.call("write.table", options)
}