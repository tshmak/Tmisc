check.header <- function(file, ...) {
  ### Function to check if there is a header in a  text file
  test1 <- read.table(file, header=F, nrows=1, stringsAsFactors=F, ...)
  classes1 <- unlist(lapply(test1, class)) %in% c("character", "logical")
  test2 <- read.table(file, header=F, nrows=1, stringsAsFactors=F, skip=1, ...)
  classes2 <- unlist(lapply(test2, class)) %in% c("character", "logical")
  if(all(classes2 == classes1)) return(F)
  else return(T)
}