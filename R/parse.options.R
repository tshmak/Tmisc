parse.options <- function(options, pre="--", sep=" ") {
  l <- length(options)
  if(l == 0) return("")
  names <- names(options)
  Names <- gsub("\\.", "-", names)
  cmd <- ""
  for(i in 1:l) {
    if(is.null(options[[i]])) options[[i]] <- ""
    if(is.logical(options[[i]])) {
      if(options[[i]]) options[[i]] <- "" else next
    }
    cmd <- paste(cmd, paste(paste0(pre, Names[i]), options[[i]], sep=sep))
  }
  return(cmd)
}
