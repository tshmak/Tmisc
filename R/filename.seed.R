filename.seed <- function(filename="test", seed=NULL, prompt=T) {

  # Returns "filename.###" as a string where ### is a random number seed
  #         ### is the time of day to the nearest centi-second (or so)
  #         unless it is specified.
  # Also runs set.seed(###)
  # Typical use:  filename <- filename.seed("MySimulation")
  #               ...
  #               save.image(paste0(filename, ".RData"))
  # Will also take the first argument when run from Rscript as the seed
  # e.g. run from shell:
  #       Rscript myscript.R 245
  # If myscript includes filename.seed(), this will set the seed to 245
  #   have the script run.

  if(is.null(seed)) {
    seed  <- as.integer((as.numeric(Sys.time()) %% 86400) * 100)
    if(interactive() && prompt) {
      input <- strsplit(readline("seed? "), "\\s+")[[1]]
      if(input != "") seed <- input[1]
    } else {
      input <- commandArgs(T)
      if(length(input) > 0) seed <- input[1]
    }
  }

  set.seed(seed)
  toreturn <- paste(filename,seed,sep=".")
  attr(toreturn, "seed") <- seed
  attr(toreturn, "filestub") <- filename
  attr(toreturn, "input") <- input
  return(toreturn)

}
