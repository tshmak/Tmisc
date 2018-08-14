QC <- function(X, MAF.min=0.05, HWE.min=1e-6, batch=F, ...) {
  
  ### Function to do QC
  ### X should be a matrix of SNP genotypes
  ### batch: make use of ffcolapply in ff package for batch processing 
  ###         to save memory
  ### ...: Other arguments to pass to ffcolapply
  
  if(!batch) {
    colMeans <- colMeans(X)
    select <- (1 - abs(1-colMeans)) > MAF.min
    
    HWE <- HWE.chisq.matrix(X)
    select <- select & HWE > HWE.min
    
  }
  else {
    colMeans <- ff::ffcolapply(colMeans(X[,i1:i2,drop=F]), X=X, RETURN=T, 
                           CFUN="c", USE.NAMES=F, ...)
    select <- (1 - abs(1-colMeans)) > MAF.min
    
    HWE <- ff::ffcolapply(HWE.chisq.matrix(X[,i1:i2,drop=F]), X=X, RETURN=T, 
                      CFUN="c",  USE.NAMES=F, ...)
    select <- select & HWE > HWE.min
  }
  
}
