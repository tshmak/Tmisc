source.some <- function(file, start="", end="", run=FALSE, ...) {
  #' Function to source part of an R script. 
  #' Helps avoid duplication of codes! 
  
  Rscript <- file(file, open="r")
  contents <- readLines(Rscript)
  close(Rscript)
  
  if(start=="") Start <- 1 else Start <- 0
  if(end=="") End <- length(contents) else End <- 0
  
  i <- 1
  if(Start==0) {
    for(i in 1:length(contents)) {
      if(contents[i] == start) {
        Start <- i
        break
      }
    }
  }
  if(End == 0) {
    for(i in i:length(contents)) {
      if(contents[i] == end) {
        End <- i
        break
      }
    }
  }
  
  if(Start == 0) stop("start line cannot be found")
  if(End == 0) stop("end line cannot be found")
  
  if(run) {
    new.file.name <- tempfile()
    new.file <- file(new.file.name, open="w")
    writeLines(contents[Start:End], con = new.file)
    close(new.file)
    source(new.file.name, ...)
  } else {
    cat(paste(contents[Start:End], collapse="\n"))
    cat("\n")
  }

}