### Common startup file (both Linux and windows)

if(!exists(".rootdir")) {
  stop("Please define .rootdir before calling Startup. Can consider modifying .Rprofile file.")
}
.rootdir.windows <- "D:/"
.rootdir.server <- "/home/tshmak/"
# .rootdir <- switch(Sys.info()["nodename"],
#                 GRC124=.rootdir.windows,
#                 statgenpro.psychiatry.hku.hk=.rootdir.server)

options(.rootdir=.rootdir,
        .rootdir.windows=.rootdir.windows,
        .rootdir.server=.rootdir.server)

Startup <- new.env()
startup.funcs <- c("clear", "attachroot", "Tim.load", "Tmisc", "devel")
for(f in startup.funcs) {
  sys.source(paste0(.rootdir, "/WORK/myRpackages/Tmisc/R/", f, ".R"), envir=Startup)  
}

attach(Startup)

if(Sys.info()['user'] == "tshmak") {
  #### Setting up local libraries ####
  repos <- getOption("repos")[1]
  if(grepl("mran\\.microsoft\\.com/snapshot", repos)) {
    # Note that checkpoint does not alter getOption("repos")
    # if(as.integer(as.Date(Sys.Date())) <  Inf) { # Change Inf to another date when you want another snapshot
    #   snapshotDate <- "2017-10-15"
    if(version$major == "3" && version$minor == "4.2") {
      snapshotDate <- "2017-10-15"
    } else if(version$major == "3" && version$minor == "5.1") {
      snapshotDate <- "2018-08-01"
    } else {
      # Seems like every time you change the snapshotDate for the CRAN repository,
      # you need to install.packages(c("Rcpp", "roxygen2", "devtools", "data.table", "ggplot2")) at least.
      stop("Not sure which repository to use")
    }
    
    options(repos=sub("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",
                      snapshotDate, repos))
    message("New snapshotDate defined as ", snapshotDate)
    options(snapshotDate=snapshotDate)
    libpath <- paste0(attachroot("/WORK/Rpackages2/"), snapshotDate)
    if(!dir.exists(libpath)) dir.create(libpath)
    
    nonMRAN <- attachroot("/WORK/Rpackages2/nonMRAN")
    if(!dir.exists(nonMRAN)) dir.create(nonMRAN)
    .libPaths(c(nonMRAN, libpath))
    
    # ************************************ #
    # !!! Right now this structure does not integrate well with checkpoint !!!
    # But I'm thinking maybe I shouldn't really use checkpoint.
    
    # The reason I'm not using checkpoint() is that the libPath actually default
    # to .../.checkpoint/snapshotDate/lib/x86_64-w64-mingw32/3.4.2
    # So it depends on the R version
    
    # checkpointLocation <- attachroot("/WORK/")
    # options(checkpointLocation=checkpointLocation)
    
    # libpath <- paste0(checkpointLocation, ".checkpoint/", snapshotDate)
    # if(!dir.exists(libpath)) dir.create(libpath)
    # checkpoint(snapshotDate,
    # project=paste0(checkpointLocation, ".checkpoint/dummy"),
    # checkpointLocation=checkpointLocation)
    # ************************************ #
    
    setMKLthreads(1)
    message("Number of MKL threads set to ", getMKLthreads())
    
  } else {
    .libPaths(c(attachroot("/WORK/Rpackages")))
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
