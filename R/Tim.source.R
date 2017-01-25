Tim.source <- function(name=NULL, environment=
                         as.environment("TimEnv"), 
                       detach=F, 
                       directory=NULL) {
  if(is.null(directory)) 
    TimScriptsDir <- getOption("TimScriptsDir")
  else 
    TimScriptsDir <- directory
  
  
  if(try(class(name),silent=T)=="character") Name <- name
  else {
    Name <- deparse(substitute(name))
    if(Name == "NULL") return(dir(TimScriptsDir))
  }
  
  # env <- attach(NULL, 2, name=Name)
  # print(Name)

  if(detach) {
    detach(Name, character.only=T)
    return(invisible())
  }
  
  testdir <- paste(TimScriptsDir, Name, sep="")
  test <- file.info(testdir)$isdir
  #print(test)
  if(!is.na(test)) {
    if(test) {
      # Load required packages
        suppressWarnings(try(
          source(paste(testdir, ".required.packages.R", sep="/")), silent=T))
      
      # Detach first, if alredy existed in search path
      test.env <- try(as.environment(Name), silent=T)
      if(class(test.env) == "environment") {
        Tim.source(Name, detach=T)
      }
      
      # source everything else
      to.source <- dir(testdir, pattern="\\.R$")
      if(length(to.source) > 0) {
        new.env <- attach(NULL, pos=2, name=Name)
        for(i in to.source) {
          Tim.source(sub("\\.R$","", 
                         paste(Name, i, sep="/" )), 
                     envir=new.env, directory=directory)
        }
      }
      else return()
    }
    else {
      sys.source(paste(TimScriptsDir, Name,".R", sep=""), 
               envir=environment)
    }
  }
  else {
    sys.source(paste(TimScriptsDir, Name,".R", sep=""), 
               envir=environment)
  }
  
}
