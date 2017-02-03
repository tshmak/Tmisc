sparseSummary2sparseMatrix <- function(i,j,x,square=F, symmetric=F, 
                                       diag=NULL, sort=T, 
                                       levels=NULL, 
                                       levels.i=NULL, levels.j=NULL, ...) {
  stopifnot(is.numeric(x))
  
  if(square) {
    if(is.null(levels)) {
      levels <- unique(c(i,j))
    } else stopifnot(length(levels) == length(unique(levels)))
    if(!(is.null(levels.i) && is.null(levels.j))) 
      stop("When 'square==T', don't specify levels.i and levels.j. Specify levels instead.")
    levels.i <- levels.j <- levels
  } else {
    if(is.null(levels.i)) levels.i <- unique(i) else 
      stopifnot(length(levels.i) == length(unique(levels.i)))
    if(is.null(levels.j)) levels.j <- unique(j) else 
      stopifnot(length(levels.j) == length(unique(levels.j)))
  }
  
  if(is.numeric(i) && !is.numeric(levels.i)) {
    stop("i is numeric but levels.i is not")
  }
  if(!is.numeric(i) && is.numeric(levels.i)) {
    stop("i is not numeric but levels.i is")
  }
  if(is.numeric(j) && !is.numeric(levels.j)) {
    stop("j is numeric but levels.j is not")
  }
  if(!is.numeric(j) && is.numeric(levels.j)) {
    stop("j is not numeric but levels.j is")
  }
  
  if(sort) {
    levels.i <- sort(levels.i)
    levels.j <- sort(levels.j)
  }

  ii <- (factor(i, levels = levels.i))
  jj <- (factor(j, levels = levels.j))
  if(sum(is.na(ii)) + sum(is.na(jj) > 0)) 
    stop("The levels (or levels.i or levels.j) given don't correspond to actual levels.")
  nlevels <- length(levels)
  if(square) {
    mat <- Matrix::sparseMatrix(i=as.integer(ii),j=as.integer(jj),x=x, 
                        symmetric = symmetric, dims=c(nlevels, nlevels), ...)
  } else {
    mat <- Matrix::sparseMatrix(i=as.integer(ii),j=as.integer(jj),x=x, 
                        symmetric = symmetric, ...)
  }
  if(!square) {
    rownames <- unique(i)
    colnames <- unique(j)
  } else {
    rownames <- levels
    colnames <- levels
  }
  
  rownames <- as.character(rownames)
  colnames <- as.character(colnames)
  
  if(sort) {
    rownames <- sort(rownames)
    colnames <- sort(colnames)
  }
  
  rownames(mat) <- rownames
  colnames(mat) <- colnames
  
  if(!is.null(diag)) diag(mat) <- diag
  return(mat)
  
}