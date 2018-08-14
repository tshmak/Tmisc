move.last.cols <- function(df, n, to) {
  #' Move the last n columns to the middle of the data.frame/matrix
  ncols <- ncol(df)
  tomove <- (ncols - n + 1):ncols
  before <- (1:ncols) < to
  after <- (1:ncols) < (ncols - n + 1) & !before
  new.df <- cbind(df[,before, drop=FALSE], df[,tomove], df[,after])
  return(new.df)
}