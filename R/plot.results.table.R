plot.results.table <- function(data, x=NULL, y=NULL, fill=NULL, X=NULL, Y=NULL,
                               wrap=NULL, scales=NULL) {
  #' Automatic plotting of results.table...
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  fill <- deparse(substitute(fill))

  p <- ggplot(data=data, map=aes_string(x=x, y=y, fill=fill))
#   p <- ggplot(data=data, map=aes(x=substitute(x), y=substitute(y),
#                                  fill=substitute(fill)))

  dodge <- position_dodge(0.9)
  bar <- stat_summary(fun.y=mean, geom="bar", pos=dodge)
  errorbar <- stat_summary(fun.data=function(x) mean_sdl(x,mult=1),
                           geom="errorbar",
                            position=dodge, aes_string(group=fill))

  todraw <- p + bar + errorbar

  X <- substitute(X)
  Y <- substitute(Y)
  wrap <- substitute(wrap)

  if(!is.null(wrap)) formula <- facet_wrap(as.formula(paste("~", wrap)), scales=scales)
  if(!is.null(X) && !is.null(Y)) formula <- facet_grid(as.formula(paste(Y, "~", X)), scales=scales)
  else if(!is.null(X)) formula <- facet_grid(as.formula(paste(".", "~", X)), scales=scales)
  else if(!is.null(Y)) formula <- facet_grid(as.formula(paste(Y, "~", ".")), scales=scales)

  if(!is.null(formula)) todraw <- todraw + formula
  return(todraw)

}
