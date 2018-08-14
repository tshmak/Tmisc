peek <- function(RData, details=F) {
  #' Examine a .RData file 
  env <- attach(RData)
  print(ls(envir=env, all.names = TRUE))
  if(details) print(ls.str(envir = env))
  detach(paste("file", RData, sep=":"), character.only = TRUE)
}