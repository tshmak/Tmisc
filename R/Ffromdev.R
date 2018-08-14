Ffromdev <- function(deviance, df.model, df.residuals, null.deviance,
                     pval=T, log.p=F) {
  ## Function to convert deviance from a Gaussian glm 
  ## to F-values/p-values
  ## null.deviance: outcome used in model
  ## deviance: deviance (can be a vector)
  ## pval: Output p-values instead of F-values
  
  SStot <- null.deviance
  SSmod <- SStot - deviance
  MSSmod <- SSmod / df.model
  MSSres <- deviance / df.residuals
  F <- MSSmod/MSSres
  if(pval) return(pf(F,df1=df.model, df2=df.residuals, 
                     lower.tail=F, log.p))
  else return(F)
  
}