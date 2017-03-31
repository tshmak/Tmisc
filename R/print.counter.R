print.counter <- function(i, skip=1, text="Iteration:", max=NA) {
  if(!is.na(max)) outof <- paste("out of", max) else outof <- ""
  if(skip > 1) if(i %% skip != 0) return(invisible(NULL))
  cat(paste(text, i, outof, "\n"))
  
}