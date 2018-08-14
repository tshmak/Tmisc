devel <- function(packages=c("devtools", "roxygen2")) {
  #' Function to load libraries for development
  for(i in packages) {
    library(i, character.only = TRUE)   
  }
}