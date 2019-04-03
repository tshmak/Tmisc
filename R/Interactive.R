Interactive <- function() {
  if(any(c(FALSE, getOption("running.jupyternb")))) return(TRUE) else return(interactive())
}