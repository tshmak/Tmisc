get1000GpseudoID <- function(df, rm.duplicates=T, 
                             chromosomes=c(1:22, "X", "Y", "M"), 
                             cluster=NULL, 
                             ...) {
  #' Find the pseudoID used in the 1000G dataset on our server 
  #' given rsID or chromosome and position and alleles
  
  library(data.table)
  id <- rep("", nrow(df))
  
  f <- function(chr) {
    cat("Processing chromosome", chr, "\n")
    bfile <- use1000G(chr=chr, .bfile=F)
    bimfile <- paste0(bfile, ".rsID.bim")
    bim <- fread(bimfile)
    colnames(bim) <- c("chr", "rsid", "cm", "pos", "alt", "ref", "pseudoid")
    m <- matchpos(df, bim, rm.duplicates=rm.duplicates, ...)
    return(data.frame(order=m$order, pseudoid=bim$pseudoid[m$ref.extract]))
  }
  
  if(is.null(cluster)) {
    l <- lapply(chromosomes, f)
  } else {
    l <- parallel::parLapplyLB(cluster, chromosomes, f)
  }
  
  L <- do.call("rbind", l)
  id[L$order] <- L$pseudoid
  
  # for(i in 1:length(chromosomes)) {
  #   # i <- 22; df <- data
  #   chr <- chromosomes[i]
  #   cat("Processing chromosome", chr, "\n")
  #   bfile <- use1000G(chr=chr, .bfile=F)
  #   bimfile <- paste0(bfile, ".rsID.bim")
  #   bim <- fread(bimfile)
  #   colnames(bim) <- c("chr", "rsid", "cm", "pos", "alt", "ref", "pseudoid")
  #   m <- matchpos(df, bim, rm.duplicates=rm.duplicates, ...)
  #   id[m$order] <- bim$pseudoid[m$ref.extract]
  # }
  
  return(id)
  
}
