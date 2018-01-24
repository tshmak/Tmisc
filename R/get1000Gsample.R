get1000Gsample <- function(super_pop=NULL, pop=NULL, FIDIID=TRUE) {
  sample <- read.table2(attachroot("/DATA/1000G/PLINK/integrated_call_samples_v3.20130502.ALL.panel"), 
                        header=T)
  if(!is.null(super_pop)) {
    sample <- sample[sample$super_pop %in% super_pop,]
  }
  if(!is.null(pop)) {
    sample <- sample[sample$pop %in% pop,]
  }
  if(nrow(sample) == 0) warning("No samples left!!")
  
  if(FIDIID) return(data.frame(FID=0, IID=sample$sample))
  
  return(sample)
}