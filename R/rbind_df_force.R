rbind_df_force <- function(..., makeindex=NULL, deparse.level = 1, make.row.names = TRUE) {
  is.df <- sapply(list(...), is.data.frame)
  stopifnot(all(is.df))
  
  colnames <- lapply(list(...), colnames)
  unique.colnames <- unique(unlist(colnames))
  
  not.in.common <- lapply(colnames, function(x) 
    unique.colnames[!(unique.colnames %in% x)])
  
  added.dfs <- lapply(not.in.common, function(x) {
    df <- as.data.frame(rep(list(a=NA), length(x)))
    colnames(df) <- x
    return(df)
  })
  
  add.cols <- lapply(1:length(list(...)), function(x) {
    if(nrow(list(...)[[x]]) == 0) {
      df <- as.data.frame(rep(list(a=character(0)), length(unique.colnames)))
      colnames(df) <- unique.colnames
    } else {
      if(length(not.in.common[[x]]) == 0) df <- list(...)[[x]] else 
        df <- cbind(list(...)[[x]], added.dfs[[x]])
    }
    return(df)
  })
                     
  add.cols$deparse.level <- deparse.level
  add.cols$make.row.names <- make.row.names
  toreturn <- do.call("rbind", add.cols)
  
  if(!is.null(makeindex)) {
    nrows <- sapply(list(...), nrow)
    index <- rep(1:length(nrows), nrows)
    toreturn[,makeindex] <- index
  }
  
  return(toreturn)
  
}