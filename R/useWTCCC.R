useWTCCC <- function(.bfile=TRUE) {
  bfile <- attachroot("/DATA/WTCCC_QC/WTCCC_QC2")
  if(.bfile) bfile(bfile)
  else return(bfile)
}
