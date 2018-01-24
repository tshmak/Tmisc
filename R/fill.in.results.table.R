fill.in.results.table <- function(table.name="results.table", ...) {
  ## ...: Supply descriptor values
  arguments <- list(...)
  
  ## Other variables
  if(exists("results.table.vars", envir=.GlobalEnv, inherits=FALSE)) {
    vars <- get("results.table.vars", envir=.GlobalEnv, inherits=FALSE)
    stopifnot(is.character(vars))
    l <- list()
    for(i in 1:length(vars)) {
      l[[i]] <- get(vars[i], envir=.GlobalEnv, inherits=FALSE)
    }
    names(l) <- vars
    arguments <- c(arguments, l)
  }
  
  arguments <- lapply(arguments, as.vector)
  lengths <- sapply(arguments, length)
  maxlength <- max(lengths)
  if(maxlength > 1) stopifnot(all(lengths[lengths > 1] == lengths[lengths > 1][1]))
  
  if(!exists(table.name, envir = .GlobalEnv, inherits=F)) {
    new.table <- data.frame()
    class(new.table) <- c("results.table", class(new.table))
    assign(table.name, new.table, envir = .GlobalEnv)
  }
  results.table <- get(table.name, envir=.GlobalEnv)
  
  names <- names(arguments)
  nrow <- nrow(results.table)
  for(i in 1:length(arguments)) {
    name <- names[i]
    tofill <- as.vector(arguments[[i]])
    results.table[nrow+(1:maxlength), name] <- tofill
  }

  assign(table.name, results.table, envir=.GlobalEnv)
}
