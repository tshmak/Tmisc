save.to.list <- function(list.name="to.save", ...) {
  ## ...: Supply descriptor values

  list.exists <- exists(list.name, inherits=F, envir=.GlobalEnv)
  if(list.exists) {
    to.save <- get(list.name, inherits=F, envir=.GlobalEnv)
  } else {
    to.save <- list()
  }

  to.save <- c(to.save, list(...))

  assign(list.name, to.save, envir=.GlobalEnv)

}
