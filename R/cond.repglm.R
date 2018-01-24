cond.repglm <- function(x, y, r2=0.9, p.threshold = 5e-8, cluster=NULL, 
                        ..., to.test=NULL, cond.pos=integer(0), 
                        trace=0, detailed.output=F) {
  #' Perform conditional analysis a la Yang et al (2012) Nature Genetics
  min2 <- function(p) min(p, na.rm=TRUE)
  max2 <- function(p) {
    if(all(is.na(p))) return(NA) else 
    return(max(p, na.rm=TRUE))
  }
  
  
  if(is.null(to.test)) {
    to.test <- rep(TRUE, ncol(x))
  }
  
  if(detailed.output) {
    detailed.p <- matrix(numeric(0), nrow=ncol(x))
    rownames(detailed.p) <- colnames(x)
  } else  detailed.p <- NULL
  
  options <- list(...)
  ### Additional covariates not supported ### 
  if(!is.null(options$covariates)) 
    stop("additional covariates not supported.")

  if(!is.null(options$constant)) 
    stop("Must have constant")
  
  # cors.save <- NULL
  # I wanted to save the correlations for output but decided it's not worth
  # it cos cond.pos can increase or decrease at each step, and to.test
  # is also different. It's easier just to do a total cor at the end. 
  while(sum(to.test) > 0) {
    
    if(length(cond.pos) == 0) {
      included.cols <- NULL
    } else {
      
      ### Testing for collinearity 1 ###
      included.cols <- x[,cond.pos, drop=F]
      to.test[cond.pos] <- F
      suppressWarnings(
        cors <- cor(x[,to.test, drop=F], included.cols, use = "pairwise"))
      max.cors <- as.vector(apply(cors^2, 1, max2))
      to.test[max.cors >= r2] <- F
    }
    if(trace > 0) {
      print(cond.pos)
    }
    p <- repglm.wrapper(y=y, x=x[,to.test, drop=F], cluster=cluster, 
                        covariates=included.cols, trace=trace-1, ..., 
                        type="lrt")
    if(detailed.output) {
      pp <- rep(NA, nrow(detailed.p))
      pp[to.test] <- p
      detailed.p <- cbind(detailed.p, pp)
      # cors.save <- c(cors.save, list(cors, to.test))
    } 
    min.p.pos <- which(p == min2(p))[1]
    
    if(p[min.p.pos] > p.threshold) {
      message("No more SNP can be added.")
      summary.g <- summary(glm(y~x[, cond.pos, drop=F], ...))
      pvals <- lrt.repglm(x=x[,cond.pos, drop=F], y=y, ...)
      return(list(converged=TRUE, cond.pos=cond.pos, summary.g=summary.g, 
                  lrt.p=pvals, detailed.p=detailed.p))
    }
    min.p.pos2 <- which(to.test)[min.p.pos]
    cond.pos <- c(cond.pos, min.p.pos2)
    
    ### Testing for collinearity 2 ###
    included.cols <- x[,cond.pos, drop=F]
    cors <- cor(included.cols, use = "pairwise")
    diag(cors) <- 0
    if(max2(cors^2) >= r2) {
      cond.pos <- cond.pos[-length(cond.pos)]
      to.test[min.p.pos2] <- F
      message("New SNP causes collinearity problem. Need to refit.")
      next
    }
    
    ### Joint model ###
    pvals <- lrt.repglm(x=x[,cond.pos, drop=F], y=y, ...)
    pvals.over <- pvals >= p.threshold
    if(any(pvals.over)) {
      to.remove <- which(pvals == max2(pvals))[1]
      to.test[cond.pos[to.remove]] <- FALSE
      cond.pos <- cond.pos[-to.remove]
      message("New SNP bumped out one of the old SNPs.")
      next
    }
    
    ### Adds new SNP ### 
    message("New SNP included.")
    to.test[min.p.pos2] <- FALSE
    
  }
  summary.g <- summary(glm(y~x[, cond.pos, drop=F], ...))
  pvals <- lrt.repglm(x=x[,cond.pos, drop=F], y=y, ...)
  return(list(converged=TRUE, cond.pos=cond.pos, summary.g=summary.g, 
              lrt.p=pvals, detailed.p=detailed.p))
  
}
