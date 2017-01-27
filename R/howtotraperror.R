howtotraperror <- function() {
  cat("withCallingHandlers({\n.\n.\n.\n")
  cat("}, error=function(e) save.image(paste0(filename, '.error.dump.RData')))")
}



