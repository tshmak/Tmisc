Tmisc <- function(clear=TRUE, ...) {
  if(isTRUE(getOption("running.jupyternb"))) stop("It seems R running on jupyter doesn't like Tmisc(). Type 'Tim.load(Tmisc)' instead.")
  if(clear) clear(...)
  Tim.load("Tmisc")
}
