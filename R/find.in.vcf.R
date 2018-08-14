find.in.vcf <- function(vcf, genotype.ignore=c("0/0", "./.", "0|0", ".|.")) {
  format.col <- which(colnames(vcf) == "FORMAT")
  format <- vcf[,format.col]
  formats <- strsplit(format, ":")
  ncols <- sapply(formats, length)
  GT.col <- sapply(formats, function(x) which(x == "GT"))
  # if(all(GT.col == GT.col)) GT.col <- GT.col[1] else {
  #   stop("GT not found in the same columns")
  # }
  
  df <- as.list(vcf[,(format.col+1):ncol(vcf)])
  ret <- ""
  for(i in 1:length(df)) {
    col <- df[[i]]
    s <- strsplit(col, ":")
    ncols.i <- sapply(s, length)
    match <- ncols.i == ncols
    ss <- sapply(1:length(s), function(i) s[[i]][GT.col[i]])
    geno <- ifelse(match, ss, genotype.ignore[1])
    exclude <- geno %in% genotype.ignore
    ret <- paste0(ret, ifelse(exclude, "", paste0(", ", names(df)[i], ":", geno)))
  }
  ret <- sub("^, ", "", ret)
  return(ret)
}
