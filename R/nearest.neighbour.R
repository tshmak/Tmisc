nearest.neighbour <- function(vec, ref.vec, selectvector=F) {
  ## Simple function to find nearest neighbour for two vectors
  ## I haven't matched the nearest neighbour to the vector yet!
  ## Only returns the subset of ref.vec that are nearest neighbours... 
  ## mainly for use in surface.explore()
  
  mat <- matrix(1, length(ref.vec),1) %*% vec
  ref.mat <- ref.vec %*% matrix(1, 1, length(vec))
  
  abs.diff <- abs(mat - ref.mat)
  colmin <- apply(abs.diff, 2, min)
  selected <- abs.diff == (matrix(1,length(ref.vec),1) %*% colmin)
  selected.col <- rowSums(selected) > 0
  
  if(!selectvector) return(ref.vec[as.vector(selected.col)])
  else return(as.vector(selected.col))
  
}