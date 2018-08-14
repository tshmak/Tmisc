pfromdev <- function(deviance, df.model, null.deviance, pval=T, 
                     Gaussian=F, n=NULL, log.p=F) {
  ## Function to convert deviance from a glm 
  ## to p-values/Chi-squares
  ## null.deviance: outcome used in model
  ## deviance: deviance (can be a vector)
  ## pval: Output p-values instead of F-values
  ## Gaussian: If we were running a Gaussian glm, 
  ##           settings need to be different
  ## n: Number of observations - only needed in Gaussian glm
  
  if(!Gaussian) {
    chisq <- null.deviance - deviance 
    if(pval) return(pchisq(chisq, df.model, lower.tail=F, log.p))
    else return(chisq)
  }
  else {
    null.deviance <- log(null.deviance) * n
    deviance <- log(deviance) * n
    pfromdev(deviance, df.model, null.deviance, pval, log.p)
  }    
}