#' Estimates slopes at extremes
#'
#' This function estimates the slope of the relationship at the extreme points of the independent variable
#' @param lmObject The model to be tested
#' @param vars A vector with the name of the linear and squared terms
#' @param .vcov The covariance matrix to use
#' @param  x.min Lower bound of interval
#' @param  x.max Upper bound of interval




USlopes <- function (lmObject, vars, .vcov = NULL, x.min = NULL, x.max = NULL) {
  # Produce table with slopes

    ## if `.vcov` missing, use the one returned by `lm`
  if (is.null(.vcov)) .vcov <- vcov(lmObject)
  ## estimated coefficients
  beta <- coef(lmObject)

  if (is.null(x.min)) x.min <- min(lmObject$model[vars[1]])
  if (is.null(x.max)) x.max <- max(lmObject$model[vars[1]])
  wt.min <- c(1,2*x.min)  # 1 is weight on beta, 2 is 2*x (weight on gamma)
  wt.max <- c(1,2*x.max)  # 1 is weight on beta, 2 is 2*x (weight on gamma)

  ## Slope at min, max
  slope.min <- wt.min %*% beta[vars]
  slope.max <- wt.max %*% beta[vars]

  ## Std err at min, max
  se.min <- (wt.min %*% .vcov[vars, vars] %*% wt.min) ^ 0.5
  se.max <- (wt.max %*% .vcov[vars, vars] %*% wt.max) ^ 0.5

  ## Test of slope=0
  t.min <- slope.min / se.min
  t.max <- slope.max / se.max
  p.min <- pt(abs(t.min), lmObject$df.residual, lower.tail = FALSE)
  p.max <- pt(abs(t.max), lmObject$df.residual, lower.tail = FALSE)

  RVAL <- list(interval = c(x.min,x.max),
               slope = c(slope.min,slope.max),
               tval = c(t.min,t.max),
               pval = c(p.min,p.max))
  class(RVAL) <- "uslopes"
  return(RVAL)
}


print.uslopes <- function(x, ...) {
  digits <- getOption("digits")
  cat(" --------------------------------------------\n",
      "           :  Lower bound \t Upper bound \n",
      "--------------------------------------------\n",
      "Interval   : ", formatC(x$interval[1],digits=digits),  "\t", formatC(x$interval[2],digits=digits), "\n",
      "Slope      : ", formatC(x$slope[1],digits=digits),     "\t", formatC(x$slope[2],digits=digits), "\n",
      "t-value    : ", formatC(x$tval[1],digits=digits),  "\t", formatC(x$tval[2],digits=digits), "\n",
      "P>|t|      : ", formatC(x$pval[1],digits=digits),  "\t", formatC(x$pval[2],digits=digits), "\n",
      "--------------------------------------------")
}
