selectColBySNP <- function(matrix, SNPs) {
  selection <- !is.na(match(remove_to(colnames(matrix)), SNPs))
  return(matrix[,selection])
}