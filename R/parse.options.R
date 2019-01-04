parse.options <- function(options, pre="--", sep=" ", dot2dash=TRUE) {
  l <- length(options)
  if(l == 0) return("")
  names <- names(options)
  if(dot2dash) names <- gsub("\\.", "-", names)
  cmd <- ""
  for(i in 1:l) {
    if(is.null(options[[i]])) options[[i]] <- ""
    if(is.logical(options[[i]])) {
      if(options[[i]]) options[[i]] <- "" else next
    }
    cmd <- paste(cmd, paste(paste0(pre, names[i]), options[[i]], sep=sep))
  }
  return(cmd)
}
