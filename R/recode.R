recode <- function(vec, ..., copy=FALSE, as.factor=T, NArecode=NA, others=NA) {
  
  ### Function to recode categorical variables
  
  list <- list(...)
  if(length(list) == 1 && "list" %in% class(list[[1]])) {
    list <- list[[1]]
  }
  
  if(copy)  y <- as.character(vec)
  else y <- as.character(rep(NA, length(vec)))
  
  names <- names(list)
  if(is.null(names)) names <- paste0("V", 1:length(list))
  else {
    V <- paste0("V", 1:length(list))
    names[names == ""] <- V[names == ""]
  }
  
  for(i in 1:length(list)) {
    y[vec %in% list[[i]]] <- names[i]
  }
  
  names <- unlist(list)
  y[!(vec %in% names)] <- others
  y[is.na(vec)] <- NArecode
  
  if(as.factor) y <- as.factor(y)
  return(y)
  
}