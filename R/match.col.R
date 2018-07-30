#' Column name matching function: Used by matchpos() and locuszoom()
match.col <- function(test, target, default.targets,
                      auto.detect=missing(target) || is.null(target),
                      ignore.multiple.matches=F,
                      mustfind=!(missing(target) || is.null(target) || all(target == "")),
                      regex=F,
                      ignore.case=!mustfind,
                      fuzzy=!mustfind,
                      silent=F) {

  #' matching columns by the first 3 letters of the column names (unless regex=T)

  if(regex) {
    Fun <- function(test,target) grepl(target, test)
  } else if(fuzzy) {
    Fun <- function(test,target) ifelse(nchar(test) < 3, test == target,
                                        startsWith(target, test))
  } else {
    Fun <- function(test,target) test == target
  }

  #### mustfind determines whether to use the default targets ####
  if(mustfind) {
    test2 <- test
    targets2 <- target
    target.name <- target[1]
    # ignore.case ignored when mustfind
  } else if(auto.detect && !missing(default.targets)) {
    test2 <- test
    targets2 <- default.targets
    target.name <- default.targets[1]
  } else return(integer(0))

  if(ignore.case) {
    test2 <- tolower(test2)
    targets2 <- tolower(targets2)
  }

  #### Matching ####
  pos <- Fun(test2, targets2[1])
  if(length(targets2) > 1) {
    for(i in 2:length(targets2)) {
      pos <- pos | Fun(test2, targets2[i])
    }
  }
  pos <- which(pos)
  if(length(pos) > 1) {
    vars <- paste(test[pos], collapse = ", ")
    mess <- paste0("Multiple columns for ", target.name, ". Possible matches: ", vars)
    if(ignore.multiple.matches) {
      pos <- integer(0)
      message(mess)
    } else {
      stop(mess)
    }
  } else if(length(pos) == 1) {
    if(!silent) cat(paste(test[pos], "identified as", target.name, "\n"))
  } else if(mustfind) {
    stop(paste("No column identified for ", target.name))
  } else {
    if(!silent) cat("No column identified for ", target.name, "\n")
  }

  return(pos)
}
