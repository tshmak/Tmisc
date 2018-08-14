unload <- function(name) {
  if(try(class(name),silent=T)=="character") Name <- name
  else Name <- deparse(substitute(name))
  
  Name <- paste0("package:", Name)
  detach(Name, unload=TRUE, character.only = TRUE)
  
}