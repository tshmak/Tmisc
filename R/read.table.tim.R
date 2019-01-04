read.table.tim <- function(input, ..., comment=NULL)  {
  options <- list(...)
  if(is.null(options$data.table)) options$data.table <- F
  if(is.null(options$check.names)) options$check.names <- T
  if(is.null(comment)) options$input <- input else {
    options$input <- paste("sed", paste0("'/^", comment, "/d'"), input)
  }
  return(do.call("fread",options))
}