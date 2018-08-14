vcov.glm.fit <- function(object, ...) {
  class(object) <- "glm"
  vcov(object, ...)
}