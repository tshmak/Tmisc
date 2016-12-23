pandcoefs <- function(y,x,constant=T, coefs.pos=1+constant,...) {
  ## Function to get p-values and coefficients using repglm
  ## ...: arguments to pass to repglm/glm.fit
  
  results <- repglm(y=y, x=x, return="toreturn",
    intermediate=paste(
      "glm.res$toreturn <- c(glm.res$coefficients[", 
      coefs.pos, 
      "], glm.res$deviance)", sep=""), ...)
  results2 <- unlist(results)
  deviance <- results2[c(F,T)]
  coefs <- results2[c(T,F)]
  null.model <- glm.fit(y=y, x=rep(1, length(y)), ...)
  null.deviance <- null.model$deviance
  pvals <- pfromdev(deviance, df.model=1, null.deviance=null.deviance,
      Gauss=null.model$family$family=="gaussian", n=length(y))
  return(list(coefs=coefs, pvals=pvals))
  
}