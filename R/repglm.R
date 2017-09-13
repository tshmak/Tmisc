repglm <- function(x, y, covariates=NULL, return=NULL, constant=T,
                   intermediate.instructions=NULL, glm.function="glm.fit",
                   cluster=NULL, trace=0, rm.na=F, ...) {
  ## Function for repeatedly calling glm
  ## y: Vector of response
  ## x: Matrix of SNPs
  ## return: The object to return from each glm
  ## constant: If an intercept should be added to x
  ## intermediate.instructions: Extra commands to perform after glm
  ## ...: arguments to be passed to glm

  if(!is.list(x)) {
    ncol <- ncol(x)
    x <- list(x)
  } else {
    ncols <- sapply(x, ncol)
    if(!all(ncols == ncols[1])) stop("Number of columns not equals in x.")
    ncol <- ncols[1]
  }

  ymatrix <- FALSE
  if(is.matrix(y)) {
    stopifnot(ncol(y) == ncol || ncol(y) == 1)
    if(ncol(y) > 1) ymatrix <- TRUE
  }

  if(!is.null(cluster)) {
    stopifnot(inherits(cluster, "cluster"))
    nclusters <- length(cluster)
    seq <- round(seq(0.500001, nclusters+0.499999, length=ncol))
    X <- lapply(1:nclusters, function(i) lapply(1:length(x), function(j)
      x[[j]][, seq==i,drop=F]))
    l <- parallel::parLapply(cluster, X, repglm, y=y, covariates=covariates,
                             return=return, constant=constant,
                             intermediate.instructions=intermediate.instructions,
                             ...)
    return(do.call("c",l))
  }

  res <- list()
  glm.args <- list(...)


  if(!is.null(intermediate.instructions)) {
    toexecute <- parse(text=intermediate.instructions)
    add.something <- T
  }
  else add.something <- F

  f<- function(X) X[,i]

  for(j in 1:length(x)) {
    if(is.vector(x[[j]])) x[[j]] <- matrix(x[[j]], ncol=1)
  }

  ytouse <- y
  for(i in 1:ncol) {
    if(trace > 0) mysimtools::print.counter(i, skip=100,ncol)
    X <- do.call("cbind", lapply(x, f))
    if (constant) touse <- cbind(1, X, covariates) else
      touse <- cbind(X, covariates)

    if(rm.na) {
      ok <- rowSums(is.na(touse)) == 0
      if(ymatrix) ok <- ok | !is.na(y[,i])
      if(!all(ok)) {
        touse <- touse[ok,]
        if(ymatrix) ytouse <- y[ok,i] else ytouse <- y[ok]
      }
    } else if(ymatrix) {
      ytouse <- y[,i]
    }

    arguments <- c(list(x=touse, y=ytouse), glm.args)
    glm.res <- do.call(glm.function, arguments)
    if(add.something) eval(toexecute)
    Res <- list(glm.res[[return]])
    if(i == 1) res <- rep(Res, ncol) else {
      res[i] <- Res
    }
  }

  return(res)
}
