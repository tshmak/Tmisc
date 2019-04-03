Interactive <- function() {
  if(isTRUE(getOption("running.jupyternb"))) return(TRUE) else return(interactive())
}