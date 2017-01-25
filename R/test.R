test <- function() {
  
  c(parent.frame(), parent.frame(1), parent.frame(2))
  eval(parse(text="ls()"), envir=.GlobalEnv)  
}
