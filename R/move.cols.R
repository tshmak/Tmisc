move.cols <- function(df, from, to, before=FALSE) {
  #' Rearrange columns in a data.frame
  
  colnames <- colnames(df)
  if(is.character(from)) from <- findpos.equal(from, colnames)
  if(is.character(to)) to <- findpos.equal(to, colnames)
  allcols <- 1:ncol(df)
  notchosen <- allcols[!(allcols %in% from)]
  stopifnot(!any(is.na(to)))
  stopifnot(!any(is.na(from)))
  
  if(length(to) == 1) {
    stopifnot(!(to %in% from))
    if(before) {
      beforecols <- notchosen[notchosen < to]
      aftercols <- notchosen[notchosen >= to]
    } else {
      beforecols <- notchosen[notchosen <= to]
      aftercols <- notchosen[notchosen > to]
    }
    new.df <- cbind(df[,beforecols, drop=F], df[,from, drop=F], df[,aftercols, drop=F])
  } else {
    stop("Not sure what to do if length(to) > 1. Maybe loop?")
  }
  
  return(new.df)
}