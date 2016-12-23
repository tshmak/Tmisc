BHfdr <- function(pvals, FDR=0.05) {
  ## Function to calculate the critical p-value
  ## using Bejamini and Hochberg's FDR procedure
  ## Remember the significants are defined by <= not <
  
  lp <- length(pvals)
  p.threshold <- FDR * (1:lp) / lp
  p.sorted <- sort(pvals)
  pass.threshold <- p.sorted <= p.threshold
  pass.threshold.index <- max((1:lp)[pass.threshold])
  p.sorted[pass.threshold.index]

}