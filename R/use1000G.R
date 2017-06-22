use1000G <- function(chr=22, .bfile=TRUE) {
  # Tim.load(Rplink)
  dir <- "/psychipc01/disk2/references/1000Genome/release/20130502_v5a/PLINK/"
  bfile <- paste0(dir, "chr", chr)
  if(.bfile) bfile(bfile)
  else return(bfile)
}