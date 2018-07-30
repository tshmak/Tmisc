remote.fread <- function(file, ssh.server="hpcf2.cgs.hku.hk", cat=TRUE,
                         ...) {
  #' Function to fread directly from server
  if(Sys.info()["nodename"] %in% c("GRC170")) {
    cat <- ifelse(cat, "cat", "")
    string <- paste("ssh", ssh.server, '"', cat, file, '"')
    return(data.table::fread(string, ...))
  } else {
    return(data.table::fread(file, ...))
  }
}