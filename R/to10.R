to10 <- function(cat) {
  #' Function to convert binary/dichotomous factor/variable to 1/0 coding
  uniq <- sort(unique(cat))
  if(length(uniq) != 2) stop("Number of categories in cat not equal 2")
  stopifnot(!any(is.na(cat)))

  cat2 <- rep(0, length(cat))
  cat2[cat == uniq[2]] <- 1
  return(cat2)
}
