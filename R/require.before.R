require.before <- function(package, before.package) {

  if(try(class(package),silent=T)!="character")
    package <- deparse(substitute(package))
  
  if(try(class(before.package),silent=T)!="character")
    before.package <- deparse(substitute(before.package))

  pkg <- paste("package", package, sep=":")
  b4.pkg <- paste("package", before.package, sep=":")
  
  test.b4 <- try(as.environment(b4.pkg), silent=T)
  if(class(test.b4) == "try-error") {
    test.b4 <- try(as.environment(before.package), silent=T)
    if(class(test.b4) == "try-error") {
      stop(paste(before.package, "not found"))
    } else {
      b4.pkg <- before.package
    }
  }
  
  test <- try(as.environment(pkg), silent=T)
  if(class(test) == "environment") {
    detach(pkg, character.only=T)
  }

  search <-search()
  pos <- (1:length(search))[search == b4.pkg]
  library(package, character.only = T, pos=pos+1)
  invisible()
     
}