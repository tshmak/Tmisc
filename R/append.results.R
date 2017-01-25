append.results <- function(file, ...) {
  #' Function that appends to lists/data.frame instead of overwriting them
  #' (in contrast to load())
  objects <- ls(all=T, envir=.GlobalEnv)
  env <- new.env()
  load(file, envir = env, ...)
  names <- ls(all=T, envir=env)
  for(j in 1:length(names)) {
    if(names[j] %in% objects) {
      old.obj <- get(names[j], envir=.GlobalEnv)
      new.obj <- get(names[j], envir=env)
      if("data.frame" %in% class(new.obj)) {
        if(!("data.frame" %in% class(old.obj))) {
          stop(paste(names[j], ": new object is data.frame but old object is not."))
        }
        assign(names[j], rbind_df_force(factor2char(old.obj), factor2char(new.obj)), envir=.GlobalEnv)
      } else {
        assign(names[j], c(old.obj, new.obj), envir=.GlobalEnv)
      }
    } else {
      assign(names[j], get(names[j], envir=env), envir=.GlobalEnv)
    }

  }

}
