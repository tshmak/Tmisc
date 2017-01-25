summary.glm.fit <- function(object, ...) {
  class(object) <- "glm"
  summary(object, ...)
}