PDF <- function(file, ..., width=7, height=7) {
  #' start pdf if non-Interactive
  #' saves some global variable to be used by dev.copy2pdf()
  if(!Interactive()) pdf(file=file, ..., width=width, height=height)
  assign(".pdf.file", file, envir=.GlobalEnv)
  assign(".pdf.height", height, envir=.GlobalEnv)
  assign(".pdf.width", width, envir=.GlobalEnv)
}
