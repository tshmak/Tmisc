pkg.version <- function(pkg.vec) {
  #' Find out the package version of specified packages
  #' Not to be confused with base::package_version()
  #' but similar to utils::packageVersion()
  
  j <- installed.packages()
  results <- data.frame(package=pkg.vec, 
                        version=rep(NA, length(pkg.vec)), 
                        installed=rep(NA, length(pkg.vec)))
  for(i in 1:length(pkg.vec)) {
    if(pkg.vec[i] %in% rownames(j)) {
      results$version[i] <- j[pkg.vec[i], "Version"]
      results$installed[i] <- j[pkg.vec[i], "LibPath"]
    }
  }
  return(results)
  
}