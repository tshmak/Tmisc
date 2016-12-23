pruneSNP.kernal <- function(data, window, step, r2, 
                            verbose=F, NAcheck=T, keeplargerMAF=T, force=F) {
  
  ### Function for pruning SNPs in similar way to PLINK 1.07 
  ### with the --indep-pairwise option. 
  
  # data: Data.frame/matrix/ff (ffdf?)
  # window: 
  # step
  # r2: minimum r2 
  # verbose: 
  # NAcheck: Make sure data has no NA
  # keeplargerMAF: If the correlation between 2 SNPs
  #   is larger than corr, then the one with larger MAF is kept
  # force: Force calculation of correlation matrix with 
  #   "pairwise.complete.obs" (see help(cor))
  

  if(is.null(data)) return(NULL)
  corr <- sqrt(r2)
  
  start.col <- 1
  end.col <- ncol(data)
  
  nSNPs <- end.col - start.col + 1
  include <- rep(TRUE, nSNPs)
  
  if(NAcheck==T) {
    if(any(class(data) == "ff")) {
      stopifnot(!ffcolapply(EXPR=is.na(data[,i1:i2,drop=F]),X=data, RETURN=T,CFUN="any"))
    }
    else if(any(class(data) == "ffdf")) stop("Method not yet implemented for ffdf")
    else {
      stopifnot(any(is.na(data))==FALSE)
    }
  }

  if(any(class(data) == "ff")) {
    MAF <- ffcolapply(EXPR=colMeans(data[,i1:i2,drop=F], na.rm=T),X=data, RETURN=T,CFUN="c")
  }
  else if(any(class(data) == "ffdf")) stop("Method not yet implemented for ffdf")
  else {
    MAF <- colMeans(data, na.rm=T)
  }
  
  include[MAF == 0 | MAF == 2] <- FALSE
  counter <- 0
  
	i.start <- 1

	while(i.start <= nSNPs) {
	  i.end <- min(i.start + window - 1, nSNPs)
	  
	  counter <- counter + 1
	  if(verbose == T) {
		cat("[", paste(as.character(counter), "] i.start: ",
					   as.character(i.start), "; i.end: ", 
					   as.character(i.end), "\n", sep = ""))
		flush.console()
	  }
	  
	  subset <- data[,i.start:i.end]
	  
	  if(is.null(ncol(subset))) {
		i.start <- i.start + step
		break
	  }
	  
	  subset <- subset[,include[i.start:i.end]]
	  MAF.subset <- MAF[i.start:i.end]
	  MAF.subset <- MAF.subset[include[i.start:i.end]]
	  if(is.null(ncol(subset))) {
		i.start <- i.start + step
		break
	  }
	  
	  if(ncol(subset) > 1) {
      if(force==T) cor.mat <- abs(cor(subset, use="pairwise.complete.obs"))
  		else cor.mat <- abs(cor(subset))
  		diag(cor.mat) <- 0
  		
  		winsize <- ncol(cor.mat)
  		cur <- rep(TRUE, winsize)
  		while(1) {
  			done <- T
  			for(i in 1:(winsize-1)) {
  				if(cur[i]) {
  				  test.vec <- cor.mat[i,] * cur
  				  if(max(test.vec[(i+1):winsize]) > corr) {
    					for(j in (i+1):winsize) {
    					  test <- cor.mat[i,j] * cur[j]
    					  if(test > corr) {
    						done <- FALSE
    						maf <- MAF.subset[c(i,j)]
    						if(abs(maf[2] - 1) < abs(maf[1] - 1) && keeplargerMAF == T) {
    						  cur[i] <- FALSE
    						}
    						else {
    						  cur[j] <- FALSE
    						}
    						break
    					  }
    					}
  				  }
  				}
  			}
  			if (done) break
  		}
  			
  		current.include <- include[i.start:i.end]
  		current.include[current.include] <- cur
  		include[i.start:i.end] <- current.include
	  }
	  i.start <- i.start + step
	  
	}
    
  return(data[,include])
  
}

pruneSNP <- function(data, window=200, step=50, r2=0.25, 
                     start.col=7, end.col=ncol(data), verbose=F,
                     keeplargerMAF=T) {
  
  ### Function for pruning SNPs in similar way to PLINK 1.07 
  ### with the --indep-pairwise option. 
  
  if(any(class(data) == "ff" || any(class(data) == "ffdf"))) 
    stop("Please use pruneSNP.kernal instead for ff or ffdf data")
  
  nSNPs <- end.col - start.col + 1
  include <- rep(TRUE, nSNPs)
  
  data.included <- data[,start.col:end.col]
  subsett <- pruneSNP.kernal(data=data.included, window=window, 
                             step=step, r2=r2, verbose=verbose,
                             keeplargerMAF=keeplargerMAF)
  
  if (start.col > 1) {
    pre.data <- data[,1:(start.col-1)]
    return(cbind(pre.data,subsett))
  } 
  else {
    return(subsett)
  } 
}
