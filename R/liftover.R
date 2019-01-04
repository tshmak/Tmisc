liftover <- function(chr, pos, pos2=NULL, from, to, bedformat=FALSE, 
                     liftover=attachroot("software/LiftOver/20160217/liftOver"), 
                     chain.files.dir=attachroot("software/LiftOver/resources/")) {
  #' Program to perform liftover of coordinates using LiftOver
  #' using program from https://genome.ucsc.edu/cgi-bin/hgLiftOver
  
  # pos2 <- NULL
  POS2 <- pos2

  from[from == 37] <- 19; to[to == 37] <- 19
  from[from == 36] <- 18; to[to == 36] <- 18
  from[from == 35] <- 17; to[to == 35] <- 17
  from[from == 34] <- 16; to[to == 34] <- 16
  
  chain.file <- paste0("hg", from, "ToHg", to, ".over.chain.gz")
  Chain.file <- file.path(chain.files.dir, chain.file)
  stopifnot(file.exists(Chain.file))
  stopifnot(file.exists(liftover))
  
  if(is.null(pos2)) pos2 <- pos
  if(!bedformat) pos1 <- pos - 1 else pos1 <- pos
  chr <- std.chr(chr, prefix="chr", XYM = TRUE)
  bed <- data.frame(chr=chr, from=pos1, to=pos2, 
                                order=1:length(chr))
  
  # bed <- data.table::as.data.table(bed)
  
  bedfile <- tempfile("liftover")
  newbedfile <- tempfile("liftover")
  newfailfile <- tempfile("liftover")
  write.table(format(bed, scientific = F), 
              file=bedfile, col.names = F, row.names = F, 
              quote = FALSE, sep="\t")
  cmd <- paste(liftover, 
               bedfile, 
               Chain.file, 
               newbedfile,
               newfailfile)
  system(cmd)
  newbed <- data.table::fread(newbedfile)
  colnames(newbed) <- c("chr", "from", "to", "order")
  if(file.size(newfailfile) > 0) {
    fail <- read.table.tim(newfailfile, comment="#")
    stopifnot(nrow(fail) + nrow(newbed) == nrow(bed))
    colnames(fail) <- c("chr", "from", "to", "order")
    fail$from <- NA
    fail$to <- NA
    result <- rbind(newbed, fail)
  } else {
    stopifnot(nrow(newbed) == nrow(bed))
    result <- newbed
  }
  result <- result[order(result$order), ]
  
  if(bedformat) pos <- result$from else pos <- result$from + 1

  if(is.null(POS2)) {
    return(data.frame(chr=result$chr, pos=pos))
  } else {
    return(data.frame(chr=result$chr, pos=pos, pos2=result$to))
  }
  
}