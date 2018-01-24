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
  # ref <- (1:N - 0.5)/ N # I've used this formula prior to 19/10/2017, 
  # but it's wrong
  ref <- (1:N)/ (N+1) # This is Expected(p). However, it doesn't mean 
  # E(-log10(p)) = -log10(E(p))!
  # 
  # Nonetheless, this seems to be more useful than E(-log10(p)), not only 
  # because it is easier to compute. It means that the BH criteria for
  # fdr control is simply a diagonal line on the graph with gradient 1 
  # and intercept -log10(alpha) - log10(n+1/n)
  
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