attachroot <- function(...) {
  dir <- paste0(...)
  dir <- sub("^~", "", dir) # This is to faciliate auto-completion, so you can precede
                            # directories by ~ just to enable auto-directory-lookup.  
  return(paste(getOption(".rootdir"), dir, sep=""))
}
