Tim.load <- function(name, lib.dir=getOption("TimLibDir"), ...) {

  if(try(class(name),silent=T)=="character") Name <- name
  else Name <- deparse(substitute(name))

  devtools::load_all(paste0(lib.dir, "/", Name), ...)

}
