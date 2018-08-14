ggerrorbar <- function(type=c("sd", "ci", "se"),
                       stat="summary", position="dodge", ...) {
  type <- match.arg(type)
  fun <- ifelse(type == "sd", "mean_sdl",
                ifelse(type == "ci", "mean_cl_normal",
                       ifelse(type == "se", "mean_se", type)))
  return(geom_errorbar(stat=stat, fun.data=fun,
                  position=position, ...))
}

