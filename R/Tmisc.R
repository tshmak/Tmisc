Tmisc <- function(clear=TRUE, ...) {
  if(any(c(FALSE, getOption("running.jupyternb")))) stop("It seems R running on jupyter doesn't like Tmisc(). Type 'Tim.load(Tmisc)' instead.")
  if(clear) clear(...)
  Tim.load("Tmisc")
}
