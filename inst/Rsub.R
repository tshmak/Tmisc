#!/home/tshmak/ropen/3.4.2/lib64/R/bin/Rscript --vanilla
#'
#' Script to determine the queue, memory requirements, and cores. Your Rscript can provide
#' memory requirements by giving two types of directives:
#' ## mem.per.thread = 2gb
#' ## fixed.mem = 4gb
#' Number of threads to be used should be passed to the Rscript via --nthreads.
#'
#' Example:
#'
#' Rsub -m 45gb -w 12:00:00 test.R --nthreads 10
#' Rsub test.R --nthreads 2
#' Rsub -p 6 test.R

#### .R file ####
args <- commandArgs(TRUE)
if(args[length(args)] == "--TEST") {
  print(args)
}
if(any(args == "--nthreads")) {
  w <- which(args == "--nthreads")
  if(w == length(args)) {
    stop("It seems --nthreads is specified without an argument")
  }
  nthreads <- args[w + 1]
  nthreads <- as.integer(nthreads)
  if(is.na(nthreads)) stop("nthreads needs to be an integer. ")
} else {
  nthreads <- NULL
}

# print("args"); print(args)

w <- which(grepl("\\.r$", args, ignore.case = TRUE))
Rfile <- args[w]
if(w > 1) preRfile <- paste(args[1:(w-1)], collapse=" ") else
  preRfile <- ""
if(length(Rfile) == 0) {
  stop("Cannot find .R file")
} else if(length(Rfile) > 1) {
  stop("Multiple .R files found.")
}

#### Function for converting sizes ####
sizes <- c(gb=1e9, mb=1e6, kb=1e3)
sizeconvert <- function(x) {
  x <- trimws(x)
  out <- x
  for(i in 1:length(sizes)) {
    if(grepl(paste0(names(sizes)[i], "$"), x, ignore.case = T)) {
      out <- as.numeric(sub(paste0(names(sizes)[i], "$"), "",
                            x, ignore.case = T)) * sizes[i]
    }
  }
  return(out)
}

#### Other options to quicksub ####
O <- trimws(strsplit(preRfile, split="-")[[1]])
# print(O)
Walltime <- grepl("^w[[:space:]]", O)
if(any(Walltime)) {
  walltime <- sub("^w[[:space:]]", "", O[Walltime])
} else {
  walltime <- NULL
}

Processes <- grepl("^p[[:space:]]", O)
if(any(Processes)) {
  processes <- sub("^p[[:space:]]", "", O[Processes])
} else {
  processes <- NULL
}

Mem <- grepl("^m[[:space:]]", O)
if(any(Mem)) {
  mem <- sub("^m[[:space:]]", "", O[Mem])
  mem <- sizeconvert(mem)
} else {
  mem <- NULL
}

Queue <- grepl("^q[[:space:]]", O)
if(any(Queue)) {
  queue <- sub("^q[[:space:]]", "", O[Queue])
} else {
  queue <- NULL
}

blanks <- grepl("^[[:space:]]*$", O)
selected <- Walltime | Processes | Mem | Queue | blanks
# print(preRfile)
# print(selected)
if(any(!selected)) others <- paste(paste0("-", O[!selected]), collapse = " ") else
  others <- ""

#### Resources from .R file: Mem.per.thread ####
rfile <- readLines(Rfile)

Mem.per.thread <- grepl("^##[[:space:]]*mem\\.per\\.thread", rfile, ignore.case = T)
if(sum(Mem.per.thread) == 1) {
  x <- rfile[Mem.per.thread]
  x2 <- strsplit(x, split="=")[[1]][2]
  mem.per.thread <- sizeconvert(x2)
} else {
  mem.per.thread <- 0
}

#### Resources from .R file: fixed.mem ####
Fixed.mem <- grepl("^##[[:space:]]*fixed\\.mem", rfile, ignore.case = T)
if(sum(Fixed.mem) == 1) {
  x <- rfile[Fixed.mem]
  x2 <- strsplit(x, split="=")[[1]][2]

  fixed.mem <- sizeconvert(x2)
} else {
  fixed.mem <- 1e9
}

#### Total walltime ####
Total.walltime <- grepl("^##[[:space:]]*total\\.walltime", rfile, ignore.case = T)
if(sum(Total.walltime) == 1) {
  x <- rfile[Total.walltime]
  x2 <- strsplit(x, split="=")[[1]][2]

  if(is.null(walltime)) {
    walltime <- x2
  }
}

if(!is.null(walltime)) {
  walltime <- as.numeric(as.difftime(walltime, format="%H:%M:%S"))
}

#### Default ####
if(is.null(nthreads)) nthreads <- if(is.null(processes)) 1 else as.integer(processes)
total.mem <- fixed.mem + mem.per.thread * nthreads
stopifnot(!is.na(total.mem))
if(is.null(mem)) mem <- total.mem
mem <- ceiling(mem / 1e9) # in gb
if(is.null(walltime)) walltime <- 6
walltime <- ceiling(walltime) # in hours

#### Determine queue ####
queue.resources <- list(max.processors=data.frame(small=2,small_ext=2, medium=12, medium_ext=12, large=12, test=24),
                        max.mem=data.frame(small=10, small_ext=10, medium=50, medium_ext=50, large=120, test=190),
                        max.walltime=data.frame(small=6, small_ext=60, medium=24, medium_ext=60, large=84, test=1))
queue.resources <- as.data.frame(t(do.call("rbind", queue.resources)))

# print(total.mem)
message("Resources:\nnthreads=", nthreads, "; mem=", mem, "; walltime=", walltime, "\n")

if(is.null(queue)) {
  poss.queues <- rownames(subset(queue.resources, nthreads <= max.processors & mem <= max.mem & walltime <= max.walltime))
  if(length(poss.queues) == 0) stop("No queue found that matched criteria")
  queue <- poss.queues[1]
}

#### Options after .r file ####
other.options <- paste(args, collapse=" ")
other.options <- sub(paste0("(.*[[:space:]]*", Rfile, "[[:space:]]*)(.*)"), "\\2", other.options)
if(!grepl("--nthreads", other.options)) other.options <- paste(other.options, "--nthreads", nthreads)

qsub.opt <- paste("quicksub -q", queue, "-m", paste0(mem, "gb"), "-p", nthreads,
                  "-w", paste0(walltime, ":00:00"), others, '"Rscript', Rfile, other.options, '"')
message(qsub.opt)

# print(Sys.info()["nodename"])
if(Sys.info()["nodename"] == "omics") system(qsub.opt)


