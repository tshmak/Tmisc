extract.from.RData <- function(file, name) {
  #' Function for extracting an object from a .RData file created by R's save() command
  E <- new.env()
  load(file=file, envir=E)
  return(get(name, envir=E, inherits=F))
}