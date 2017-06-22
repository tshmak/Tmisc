which.matrix <- function(mat) {
  #' A function to do "which" on a matrix 
  nrow <- nrow(mat)
  ncol <- ncol(mat)
  w <- which(mat)
  col <- w %/% nrow + 1
  row <- w %% nrow
  row[row==0] <- nrow
  
  return(data.frame(row=row, col=col))

    
}