load.results <- function(pattern, path=".", ext=".RData", ...,
                         files=NULL, prompt=T, append=F) {
  #' Load a bunch of .RData files and have them append.results() together
  # path <- "."; ext <- ".RData"
  if(is.null(files)) {
    ext <- ifelse(substr(ext, 1,1) == ".", ext, paste0(".", ext))
    ext <- paste0(ext, "$")
    ext <- gsub("\\.","\\\\\\.",x = ext)
    files <- dir(pattern=ext, path = path, ignore.case = T)
    matchingfiles <- files[grep(pattern, files, value=F)]
  } else {
    matchingfiles <- files
  }
  if(length(matchingfiles) == 0) stop(paste("No files matching", pattern))
  first <- !append
  for(i in 1:length(matchingfiles)) {
    toload <- paste(path, matchingfiles[i], sep="/")
    load <- T
    if(prompt) {
      yesno <- readline(paste0("Load '", toload, "'?"))
      if(!(yesno %in% c("Y", "y"))) load <- F
      if(yesno %in% c("a", "A")) prompt <- F
    }
    if(load) {
      # if(first) {
      #   load(toload, envir=.GlobalEnv)
      #   first <- FALSE
      # } else append.results(toload)
      append.results(toload, ...)
    }
  }

}
