### Common startup file (both Linux and windows)
# .rootdir <- Sys.getenv("HOME")
if(!exists(".rootdir")) {
  stop("Please define .rootdir before calling Startup. Can consider modifying .Rprofile file.")
}

options(.rootdir=.rootdir)

Startup <- new.env()
startup.funcs <- c("clear", "attachroot", "Tim.load", "Tmisc", "devel", "install.packages")
for(f in startup.funcs) {
  sys.source(paste0(.rootdir, "/WORK/myRpackages/Tmisc/R/", f, ".R"), envir=Startup)  
}

attach(Startup)

#### Using MRAN repositories ####

if(Sys.info()['user'] == "tshmak") {
  
  required.dir <- paste0(.rootdir, "/WORK/Rpackages2")
  if(!dir.exists(required.dir)) stop(paste(required.dir, "does not exist yet. Please create it."))
  
  #### snapshotDate ####
  if(!exists(".snapshotDate")) {
    available.dates <- list.dirs(path=required.dir, recur=F, full.names=F)
    available.dates <- available.dates[available.dates != "nonMRAN"]
    # dates <- as.Date(available.dates, "%Y-%m-%d")
    if(length(available.dates) > 0) {
      .snapshotDate <- as.character(max(available.dates))  
    } else {
      stop("Please specify .snapshotDate in the .Rprofile file.")
    }
  }
  snapshotDate <- .snapshotDate
  message("New snapshotDate defined as ", snapshotDate)
  options(snapshotDate=snapshotDate)
  
  #### .libPaths ####
  libpath <- paste0(attachroot("/WORK/Rpackages2/"), snapshotDate)
  if(!dir.exists(libpath)) dir.create(libpath)
  
  nonMRAN <- attachroot("/WORK/Rpackages2/nonMRAN")
  if(!dir.exists(nonMRAN)) dir.create(nonMRAN)
  .libPaths(c(nonMRAN, libpath))
  
  #### Setting up repos for downloading new packages ####
  options(repos=paste0("https://mran.microsoft.com/snapshot/", snapshotDate))
  
  #### set MKL threads to 1 if not on Mac ####
  if(!grepl("darwin", Sys.info()['sysname'], ignore.case = T)) {
    
    # Actually this should only be relevant if running Microsoft R... 
    setMKLthreads(1)
    message("Number of MKL threads set to ", getMKLthreads())
  }
  
  #### Check if basic packages installed ####
  basic.packages <- c("Rcpp", "roxygen2", "devtools", "data.table", "ggplot2")
  # print(search())
  if(!all(basic.packages %in% rownames(utils::installed.packages(.libPaths()[2]) ) )) {
    stop(paste("Not all of the basic packages: (", paste(basic.packages, collapse = ", "), ") are installed.",
                  "type 'install.packages(basic.packages, lib=libpath)' to install them."))
  }

}

#### Error handling ####
options(TimScriptsDir=attachroot("/WORK/Rscripts/"),
        TimLibDir=attachroot("/WORK/myRpackages/"))
if(!interactive()) {
  options(error = quote({
    if(exists("filename", envir=.GlobalEnv, inherits=F)) {
      dumpfile <- get("filename", envir=.GlobalEnv, inherits=F)
    } else dumpfile <- "zzz"
    dumpfile <- paste0(dumpfile, ".error.dump")
    dump.frames(dumpto=dumpfile, to.file = TRUE); q(status = 1)}))
} else {
  if(Sys.info()["sysname"] == "Linux") {
    options(error =  quote(dump.frames()))
  } else {
    # options(error =  quote(dump.frames()))
  }
}

rm(list=ls(all=T))
