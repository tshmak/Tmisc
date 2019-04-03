dev.copy2PDF <- function(file, ..., width=NULL, height=NULL) {
  #' overrides default to enable similar behaviour from Interactive and
  #' non-Interactive use
  #' used in conjunction with PDF()

  if(exists(".pdf.file", envir=.GlobalEnv, inherits = F)) {
    .file <- get(".pdf.file", envir=.GlobalEnv)
    rm(list=".pdf.file", envir=.GlobalEnv)
    if(missing(file)) file <- .file
  }
  if(exists(".pdf.height", envir=.GlobalEnv, inherits = F)) {
    .height <- get(".pdf.height", envir=.GlobalEnv)
    rm(list=".pdf.height", envir=.GlobalEnv)
    if(is.null(height)) height <- .height
  }
  if(exists(".pdf.width", envir=.GlobalEnv, inherits = F)) {
    .width <- get(".pdf.width", envir=.GlobalEnv)
    rm(list=".pdf.width", envir=.GlobalEnv)
    if(is.null(width)) width <- .width
  }

  dev <- names(dev.cur())
  if(dev == "pdf") {
    dev.off()
  } else if(grepl("^X11", dev) || dev == "RStudioGD") {
    grDevices::dev.copy2pdf(file=file, ...,
                            width=width, height=height)
  } else {
    dev.off()
    # file.rename("Rplots.pdf", file)
  }
}
