AUC <- function(predictor, cat) {
  stopifnot(length(cat) == length(predictor))
  stopifnot(is.vector(predictor) && is.numeric(predictor))
  stopifnot(!any(is.na(cat)))
  stopifnot(!any(is.na(predictor)))
  
  return(AUC::auc(AUC::roc(predictor, as.factor(to10(cat)))))
  
}