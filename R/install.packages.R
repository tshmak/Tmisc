install.packages <- function(..., repos=getOption("repos")) {
  #' Function to install to local MRAN library if from MRAN. 
  #' Otherwise install to default (should be attachroot("/WORK/Rpackages/nonMRAN"))
  
  if(isTRUE(getOption("running.condaR"))) stop("You need to use 'conda install r-[Rpackage]' rather than this command since you're running R in conda.") 
  
  date <- "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
  libPaths <- .libPaths()
  
  if(!is.null(repos) && grepl("mran\\.microsoft\\.com", repos)) {
    date.repos <- sub(paste0(".*(", date, ")"), "\\1", repos)
    date.libPaths <- sub(paste0(".*(", date, ")"), "\\1", libPaths)
    avail.repos <- which(date.libPaths == date.repos)
    if(length(avail.repos) > 0) {
      message("Installing from MRAN to ", libPaths[avail.repos])
      utils::install.packages(..., lib=libPaths[avail.repos], repos=repos)
      return(invisible())
    } 
    
  } 
  libPaths <- c(list(...)$lib, libPaths)
  if(grepl(paste0(date, "$"), libPaths[1])) {
    date.libPaths1 <- sub(paste0(".*(", date, ")"), "\\1", libPaths[1])
    stop(paste("Default library is associated with", 
               date.libPaths1, 
               ". Specify another lib location to install."))
  }
  message("Installing to ", libPaths[1])
  utils::install.packages(..., repos=repos)
  
}
