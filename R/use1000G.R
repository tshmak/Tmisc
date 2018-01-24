use1000G <- function(chr=22, .bfile=TRUE) {
  # Tim.load(Rplink)
  dir <- attachroot("/DATA/1000G/PLINK/")
  bfile <- paste0(dir, "chr", chr)
  if(.bfile) bfile(bfile)
  else return(bfile)
}