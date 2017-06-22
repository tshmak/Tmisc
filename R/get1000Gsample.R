get1000Gsample <- function(super_pop=NULL, pop=NULL, FIDIID=TRUE) {
  sample <- read.table2(paste0("/psychipc01/disk2/references/1000Genome/release/20130502_v5a/PLINK/", 
                               "integrated_call_samples_v3.20130502.ALL.panel"), 
                        header=T)
  if(!is.null(super_pop)) {
    sample <- sample[sample$super_pop == super_pop,]
  }
  if(!is.null(pop)) {
    sample <- sample[sample$pop == pop,]
  }
  if(nrow(sample) == 0) warning("No samples left!!")
  
  if(FIDIID) return(data.frame(FID=0, IID=sample$sample))
  
  return(sample)
}