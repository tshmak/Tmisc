'%==%' <- function(a,b, digits=10) {
  #' Test for approximate equality
  #' 
  return(signif(a, digits) == signif(b, digits))
}