factor2char <- function(df) {
  #' Function to turn factor variables in df (data.frame) to characters

  stopifnot(is.data.frame(df))
  if(ncol(df) >= 1) {
    for(i in 1:ncol(df)) {
      if(is.factor(df[,i])) df[,i] <- as.character(df[,i])
    }
  }
  return(df)
}
