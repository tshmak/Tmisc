insert.columns <- function(df, at=ncol(df)+1, ...) {
  #' Insert columns to a data.frame
  to.insert <- as.data.frame(list(...))
  before <- 1:ncol(df) < at
  after <- 1:ncol(df) >= at
  return(cbind(df[,before, drop=F], to.insert, df[,after, drop=F]))

}