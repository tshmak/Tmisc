plotcor <- function(corr, nlevels=31, shape1=1, shape2=1, remove.names=F) {
  
  col.regions <- colorRampPalette(c("blue", "white", "red"))
  if(remove.names) colnames(corr) <- rownames(corr) <- NULL
  
  at <- pbeta(seq(0,1, length=nlevels), shape1, shape2)
  at <- (at[-1] + at[-length(at)])/2
  at <- c(at, 1 + abs(1 - max(at)))
  at <- sort(unique(c(-at, at)))
  
  toplot <- lattice::levelplot(corr, main="", xlab="", ylab="", 
                      col.regions=col.regions, at=at)
  plot(toplot)
  return(invisible(corr))
  
}
