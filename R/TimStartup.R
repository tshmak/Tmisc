TimStartup <-function() {
  #' Copy this to the start of a script to enable other CGS users to run my scripts
  cat('if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools"); .rootdir <- "/home/tshmak/"; source("/home/tshmak/WORK/myRpackages/Startup.R")
\n')

}