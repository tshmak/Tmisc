qqplot_pvals2 <- function(vector, nodraw=F,tail=NULL, 
                         resolution=0.05, 
                         memory=1e9, 
                         ...) {
  ## ...: Other commands to pass to plot
  N <- length(vector)
  ref <- (1:N - 0.5)/ N
  x <- -log10(ref)
  y <- -log10(sort(vector))
  
  selectvector <- rep(T, N)
    
  if(!is.null(tail)) {
    selectvector <- rep(F, N)
    selectvector[1:tail] <- T
  }
  
  if(resolution > 0) {
    
    ### Nipped from my general collection ###
    nearest.neighbour <- function (vec, ref.vec, selectvector = F) 
    {
      mat <- matrix(1, length(ref.vec), 1) %*% vec
      ref.mat <- ref.vec %*% matrix(1, 1, length(vec))
      abs.diff <- abs(mat - ref.mat)
      colmin <- apply(abs.diff, 2, min)
      selected <- abs.diff == (matrix(1, length(ref.vec), 1) %*% 
                                 colmin)
      selected.col <- rowSums(selected) > 0
      if (!selectvector) 
        return(ref.vec[as.vector(selected.col)])
      else return(as.vector(selected.col))
    }
    
    xplot <- seq(0, max(x),by=resolution)
    blocksize <- ceiling(memory/ 8 / length(xplot))
    for(i in 1:ceiling(N/blocksize)) {
      start <- (i-1) * blocksize + 1
      end <- min(c(i * blocksize,N))
      select <- nearest.neighbour(xplot,ref.vec=x[start:end], selectvector=T)
      selectvector[(start:end)[!select]] <- F
    }

  }

  ## CIs...
  plotted.points <- (1:N)[selectvector]
  u <- l <- plotted.points * NA
  for(i in 1:length(plotted.points)) {
    u[i] <- qbeta(0.975,plotted.points[i],N-plotted.points[i]+1)
    l[i] <- qbeta(0.025,plotted.points[i],N-plotted.points[i]+1)
  }
  u <- -log10(u)
  l <- -log10(l)
  
  result <- list(x=x,y=y,u=u,l=l, selectvector=selectvector)
  class(result) <- "qqplot_pvals"
  
  if(!nodraw) plot(result, ...)
  
  return(invisible(result))

}