#' Perform Utest
#'
#' This function computes the Lind/Mehlum test of a U shaped relationship.
#' @param lmObject The model to be tested
#' @param vars A vector with the name of the linear and squared terms. Can also be provided as a formula
#' @param .vcov The covariance matrix to use
#' @param  x.min Lower bound of interval. If \code{NULL}, the minimum observed in the data is used.
#' @param  x.max Upper bound of interval. If \code{NULL}, the maximum observed in the data is used.
#'
#' @details To test for a U shaped or inverse U shaped relationship, it is necessary to provide an interval where the shape is located.
#'   A U shaped relationship is downward sloping at the lower bound and upward sloping at the upper bound, and vice versa for an inverted U shape.
#'
#'   The function assumes inputs from a model including a squared specification. The sign of the squared term is used to differentiate between U
#'   shaped and inverted U shaped relationships.
#'
#'   The function provides a test of the joint hypothesis of a downward sloping relationship at \code{x.min} and an uppward sloping relationship
#'   at \code{x.max} for U shaped relationships and vice versa for inverted U shaped relationships, as detailed in Lind and Mehlum (2010).
#'
#' @references J. T. Lind and H. Mehlum (2010), \href{https://doi.org/10.1111/j.1468-0084.2009.00569.x}{
#'   With or without U? The appropriate test for a U
#'   shaped relationship}. \emph{Oxford Bulletin of Economics and Statistics} \strong{72(1)}: 109-18.
#'
#' @examples
#' x <- runif(100,min=-1,max=1)
#' xsq <- x^2
#' y <- x^2+rnorm(100)
#' mod <- lm(y~x+xsq)
#'
#' utest(mod,c("x","xsq"))
#' utest(mod,c("x","xsq"),x.max=0.8)
#'
#' mod.logit <- glm((y>0)~x+xsq,family="binomial")
#' utest(mod.logit,c("x","xsq"))
#' @import stats
#' @export
utest <- function (lmObject, vars, .vcov = NULL, x.min = NULL, x.max = NULL) {

  dname <- paste(deparse(substitute(lmObject)))

  ## estimated coefficients
  beta <- coef(lmObject)
  
  ## Extract vector of variable names if a formula is provided
  if (inherits(vars,"formula"))  vars <- all.vars(vars)

  if (beta[vars[2]]<0) {
    method <- "Test of inverted U shape"
    alt <- "Monotone or U shape"
  } else {
    method <- "Test of U shape"
    alt <- "Monotone or inverse U shape"
  }

  slopes <- uslopes(lmObject, vars, .vcov, x.min=x.min, x.max=x.max)

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
