corr.lag <- function(X, lags, covariance=F) {
  ### Function for estimating the correlation of SNPs separated by lags
  ### lags: vector
  
  ncol <- ncol(X)
  nrow <- nrow(X)
  length.lags <- length(lags)
  
  Result <- NULL
  for(i in 1:length.lags) {
    for(j in 1:lags[i]) {
      select <- seq(j, ncol, by=lags[i])
      selected <- X[,select,drop=F]
      selected <- scale(selected, center=T, scale=F)
      A <- selected[,-1,drop=F]
      B <- selected[,-ncol(selected),drop=F]
      numerator <- colSums(A*B)
      if(covariance==F) {
        var <- colSums(selected * selected)
        denom <- sqrt(var[-1] * var[-ncol(selected)])
      }
      else denom <- nrow - 1
      result <- numerator / denom
      result2 <- data.frame(i=select[-ncol(selected)], j=select[-1], x=result)
      SNP.names <- colnames(selected)
      if(!is.null(SNP.names)) {
        rownames(result2) <- paste(SNP.names[-ncol(selected)], SNP.names[-1], sep=",")
      }
      Result <- rbind(Result, result2)
      
    }
  }
  Result$lag <- Result$j - Result$i
  return(Result)
  
}