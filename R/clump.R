clump <- function(pvals, genotype, pos, SNP=paste0("SNP", 1:length(pvals)), 
                  r2=0.2, window.kb=250000, p1=1, p2=1, trace=0) {
  #' Function to mimic Plink's --clump command
  #' It's not equivalent to plink's command. 

  P <- length(pvals)
  stopifnot(P == ncol(genotype))
  stopifnot(P == length(pos))
  stopifnot(P == length(SNP))
  stopifnot(all(sort(pos) == pos))
  stopifnot(!any(is.na(pos)))
  sd0 <- apply(genotype,MARGIN = 2,FUN=sd) == 0
  tickedoff <- is.na(pvals) | sd0
  pvals[tickedoff] <- Inf
  
  order <- order(pvals)
  results <- data.frame(SNP=SNP, p=NA, clumped.SNPs = "")
  
  for(i in 1:P) {
    snp=order[i]
    if(tickedoff[snp]) next
    pval <- pvals[snp]
    if(trace > 0) {
      message <- cat(paste0("i=", i, ", pval=", pval, ", %marked=", 
                            sum(tickedoff)/length(tickedoff), "\n"))
    }
    if(pval > p1) break
    po <- pos[snp]
    toinclude <- pos >= po - window.kb * 1000 & pos <= po + window.kb * 1000 &
      pvals < p2 & pos != pos[snp] & !tickedoff
    R2 <- cor(genotype[,snp], genotype[,toinclude], 
              use = "pairwise.complete.obs")^2
    R2[is.na(R2)] <- 0
    toinclude[toinclude] <- R2 >= r2

    results$SNP[i] <- SNP[snp]
    results$clumped.SNPs[i] <- paste(SNP[toinclude], collapse = ",")
    results$p[i] <- pvals[snp]
    
    tickedoff[toinclude] <- TRUE
    tickedoff[snp] <- TRUE
  }
  return(results[!is.na(results$p), ])
}