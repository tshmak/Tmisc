Tim.load <- function(name) {

  if(try(class(name),silent=T)=="character") Name <- name
  else Name <- deparse(substitute(name))

  my.lib.dir <- getOption("TimLibDir")
  devtools::load_all(paste0(my.lib.dir, Name))

}
