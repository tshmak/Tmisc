repglm <- function(y, x, covariates=NULL, return, constant=T, 
                   intermediate.instructions=NULL, ...) {
  ## Function for repeatedly calling glm
  ## y: Vector of response
  ## x: Matrix of SNPs
  ## return: The object to return from each glm
  ## constant: If an intercept should be added to x
  ## intermediate.instructions: Extra commands to perform after glm
  ## ...: arguments to be passed to glm

  res <- list()
  glm.args <- list(...)
  if(!is.null(intermediate.instructions)) {
    toexecute <- parse(text=intermediate.instructions)
    add.something <- T
  }
  else add.something <- F

  for(i in 1:ncol(x)) {
    if (constant) touse <- cbind(1, x[,i], covariates)
    else touse <- cbind(x[,i], covariates)
    arguments <- c(list(x=touse,y=y), glm.args)
    glm.res <- do.call("glm.fit", arguments)
    if(add.something) eval(toexecute)
    res <- c(res, list(glm.res[[return]]))
  }
  
  res
}
