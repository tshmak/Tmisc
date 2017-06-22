running.corr <- function(X,depth=10, covariance=F) {
  ### Function for generating the correlation matrix of a SNP frequency matrix
  ### Matrix stored as sparseMatrix object
  ### depth: Maximum lag considered
  
  ncol <- ncol(X)
  nrow <- nrow(X)
  depth <- min(ncol-1, depth)
  if(!covariance) {
    # Calculate required length
    length <- (ncol - depth)*depth + sum(0:(depth-1))
    ijx <- matrix(NA, length,3)
    
    start <- 1
    for(i in 1:(ncol-1)) {
      start.col <- i + 1
      end.col <- min(i + depth, ncol)
      corr <- cor(X[,start.col:end.col], X[,i])
      len <- length(corr)
      end <- start + len - 1 
      ijx[start:end, ] <- cbind(i,start.col:end.col, corr)
      start <- start + len
    }
    
    to.return <- Matrix::sparseMatrix(i=ijx[,1], j=ijx[,2], x=ijx[,3],
                              dims=c(ncol, ncol), sym=T)
    diag(to.return) <- 1
    
  }
  else {
    # Calculate required length
    length <- (ncol - depth)*(depth+1) + sum(1:depth)
    ijx <- matrix(NA, length,3)
    
    start <- 1
    for(i in 1:ncol) {
      start.col <- i 
      end.col <- min(i + depth, ncol)
      cov <- cov(X[,start.col:end.col], X[,i])
      len <- length(cov)
      end <- start + len - 1 
      ijx[start:end, ] <- cbind(i,start.col:end.col, cov)
      start <- start + len
    }
    
    to.return <- Matrix::sparseMatrix(i=ijx[,1], j=ijx[,2], x=ijx[,3],
                              dims=c(ncol, ncol), sym=T)
    
  }
  return(to.return)
  
}