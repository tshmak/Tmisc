repglm.wrapper <- function(..., type=c("deviance", "coefficients",
                                       "lrt.p", "coef_n_se")
                           , help=F) {

  if(help) {
    x <- matrix(rnorm(200),100,2)
    y <- rnorm(100)
    arguments <- c(list(x=x,y=y))
    glm.res <- do.call("glm.fit", arguments)
    str(glm.res,1)
    assign(".glm.fit.obj", glm.res, envir=.GlobalEnv)
    message("a mock glm.fit object is created as .glm.fit.obj in the global environment")
    return(invisible(NULL))
  }

  type <- match.arg(type)
  if(type == "deviance") {
    return <- "res"
    intermediate.instructions <- '{
      glm.res$res <- c(glm.res$deviance, glm.res$df.residual)
    }'
  } else if(type == "coefficients") {
    return <- "coefficients"
    intermediate.instructions <- NULL
  } else if(type == "lrt.p") {
    ## Null model ##
    options <- list(...)
    cons <- rep(1, length(options$y))
    if(!is.null(options$constant)) if(!options$constant) cons <- cons * 0
    options$x <- cbind(cons, options$covariates)
    options$covariates <- NULL
    options$cluster <- NULL
    options$trace <- NULL
    options$rm.na <- NULL
    g0 <- do.call("glm.fit", options)
    ## Non-null model ##
    g <- repglm.wrapper(..., type="deviance")
    p <- sapply(g, function(X) pchisq(q=g0$deviance - X[1],
                                      df=g0$df.residual - X[2],
                                      lower.tail = F))
    return(p)
  } else if(type == "coef_n_se") {
    return <- "res"
    intermediate.instructions <- '{
      class(glm.res) <- "glm"
      se <- sqrt(diag(vcov(glm.res)))
      glm.res$res <- data.frame(coef=glm.res$coefficients,
                                se=se)
    }'
  }
  g <- repglm(..., return = return,
              intermediate.instructions = intermediate.instructions)
  return(g)

}

