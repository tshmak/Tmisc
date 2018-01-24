ggbar <- function(stat="summary", fun.y="mean",
                  position="dodge", ...) {
  return(geom_bar(stat=stat, fun.y=fun.y, position=position, ...))
}

