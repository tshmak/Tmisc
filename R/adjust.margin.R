adjust.margin <- function(bottom=5.1, left=4.1, top=4.1, right=2.1, rel=F) {
  if(rel) {
    par <- par("mar") 
    par(mar=par + c(bottom, left, top, right))
  } else {
    par(mar=c(bottom, left, top, right))
  }
}