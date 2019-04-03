Rfilename <- function(filename="test", seed=NULL, set.seed=TRUE) {
  #' Function to Automatically retrieve filename of R script file
  #' when run using Rscript. Similar to filename.seed().
  #' @param seed Can be either "prompt", or any integer
  #' @param set.seed If set.seed==FALSE, then seed is only used to
  #' prompt for other options
  #' @details If seed is "prompt", then in Interactive mode,
  #' you'll be prompted to input a seed and in non-Interactive mode,
  #' your arguments following the script file will be your seed.
  #' If you don't input anything or your seed is non-numeric,
  #' a seed will be generated based on the current time.
  #' You can additionally input other options following the seed,
  #' separating them by ";" or spaces.
  if(!Interactive()) {
    opts <- parseargs()
    filename <- opts$.SYSTEM[which(grepl("^--file=", opts$.SYSTEM))[1]]
    filename <- sub("^--file=", "", filename)
    filename <- sub("\\.R$", "", filename, ignore.case = TRUE)
  }
  message(paste("R script file is", filename))

  if(is.null(seed)) {
    return(filename)
  }
  input <- NULL
  if(seed == "prompt") {
    if(Interactive()) {
      input <- readline("seed? ")
    } else {
      input <- paste(commandArgs(T), collapse=" ")
    }
    input <- strsplit(input, ";|\\s+")[[1]]
    input <- input[input != ""]
    seed <- as.integer((as.numeric(Sys.time()) %% 86400) * 100)
    if(length(input) > 0) {
      Seed <- input[1]
      if(is.numeric(as.integer(Seed))) seed <- Seed
    }
  }

  if(set.seed) {
    seed <- as.integer(seed)
    set.seed(seed)
    message(paste("Seed set to", seed))
    toreturn <- paste(filename,seed,sep=".")
    attr(toreturn, "seed") <- seed
  } else {
    toreturn <- filename
  }

  attr(toreturn, "filestub") <- filename
  attr(toreturn, "input") <- input
  return(toreturn)

}
