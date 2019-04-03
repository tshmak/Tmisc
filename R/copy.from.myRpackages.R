copy.from.myRpackages <- function(functions, package, file, For.real=F, append=F) {
  
  dir <- paste0(getOption("TimLibDir"), package, "/R/")
  stopifnot(dir.exists(dir))
  files <- paste0(dir, functions, ".R")
  stopifnot(sapply(files, file.exists))
  
  for(i in 1:length(functions)) {
    if(i==1 && !append) {
      cmd <- paste("cat", files[i], ">", file)
    } else {
      cmd <- paste("cat", files[i], ">>", file)
    }
    if(!For.real) print(cmd) else {
      system(cmd)
    }
  }
  if(!For.real) message("Specify For.real=T to run for real")
}