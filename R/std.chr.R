std.chr <- function(chr, prefix="", XYM=TRUE) {
  #' Function to standardize (human) chromosome codes
  CHR <- sub("^chr", "", chr, ignore.case = TRUE)
  if(XYM) {
    CHR[CHR == "23"] <- "X"
    CHR[CHR == "24"] <- "Y"
    CHR[CHR == "25"] <- "XY"
    CHR[CHR == "26" | CHR == "M"] <- "MT"
    if(!all(CHR %in% c(0:22, "X", "Y", "XY", "MT"))) {
      problems <- CHR[!(CHR %in% c(0:22, "X", "Y", "XY", "MT"))]
      message("Some chromosome codes not converted.")
      print(problems)
    }
  } else {
    CHR[CHR == "X"] <- "23"
    CHR[CHR == "Y"] <- "24"
    CHR[CHR == "XY"] <- "25"
    CHR[CHR == "MT" | CHR == "M"] <- "26"
    CHR <- as.integer(CHR)
    if(!all(CHR %in% 0:26)) {
      problems <- CHR[!(CHR %in% 0:26)]
      message("Some chromosome codes not converted.")
      print(problems)
    }
  }
  return(CHR)
}