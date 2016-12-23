qqplot_pvals <- function(vector, nodraw=F,
                         resolution=0.01, 
                         na.rm=T, 
                         ...) {
  ## tail: Insist the top x SNPs be plotted.
  ## ...: Other commands to pass to plot

  na <- is.na(vector)
  if(any(na)) if(!na.rm) stop("vector contains N/A") else vector <- vector[!na]

  stopifnot(vector <= 1 & vector >= 0)

  N <- length(vector)
  ref <- (1:N - 0.5)/ N
  x <- -log10(ref)
  y <- -log10(sort(vector))

  stopifnot(resolution > 0)  
  xplot <- seq(0, max(x),by=resolution)
  index <- ceiling(10^(log10(N) - xplot))
  selectvector <- sort(unique(index))
  selectvector <- selectvector[selectvector <= N]

  ## CIs...
  u <- l <- selectvector * NA
  for(i in 1:length(selectvector)) {
    u[i] <- qbeta(0.975,selectvector[i],N-selectvector[i]+1)
    l[i] <- qbeta(0.025,selectvector[i],N-selectvector[i]+1)
  }
  u <- -log10(u)
  l <- -log10(l)
  
  result <- list(x=x,y=y,u=u,l=l, selectvector=selectvector)
  class(result) <- "qqplot_pvals"
  
  if(!nodraw) plot(result, ...)
  
  return(invisible(result))

}