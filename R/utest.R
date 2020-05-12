#' Perform Utest
#'
#' This function computes the Lind/Mehlum test of a U shaped relationship.
#' @param lmObject The model to be tested
#' @param vars A vector with the name of the linear and squared terms
#' @param .vcov The covariance matrix to use
#' @param  x.min Lower bound of interval
#' @param  x.max Upper bound of interval



UTest <- function (lmObject, vars, .vcov = NULL) {

  dname <- paste(deparse(substitute(lmObject)))

  ## estimated coefficients
  beta <- coef(lmObject)

  if (beta[vars[2]]<0) {
    method <- "Test of inverted U shape"
    alt <- "Monotone or U shape"
  } else {
    method <- "Test of U shape"
    alt <- "Monotone or inverse U shape"
  }

  slopes <- USlopes(lmObject, vars, .vcov)

  if (slopes$slope[1]*slopes$slope[2]>0) {
    RVAL <- list(statistic = "Extremum outside interval - trivial failure to reject H0",
                 method=method,
                 data.name = dname)
    class(RVAL) <- "htest"
    return(RVAL)
  }

  ## Joint test
  t=min(abs(slopes$tval))

  names(t) <- "t-value"
  RVAL <- list(statistic = t,
               p.value=max(abs(slopes$pval)),
               method=method,
               data.name = dname,
               alternative=alt,
               ekstra=5)
  class(RVAL) <- "htest"
  return(RVAL)
}
