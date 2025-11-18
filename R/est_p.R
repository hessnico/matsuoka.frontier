#' Estimate the Parameter \eqn{p} from Residuals
#'
#' Estimates the shape parameter \eqn{p} of the Matsuoka distribution
#' using the **method of moments**, based on the residuals
#' \eqn{z - g(x)} obtained in the first estimation step.
#'
#' @param residuals A numeric vector of residuals, typically computed as
#'   \eqn{z - g_hat}, where \eqn{z = -\log(y)}.
#'
#' @return A positive numeric scalar giving the estimated value of \eqn{p}.
#'
#' @seealso
#' \code{\link{estimate}} for the full three-step frontier estimation workflow.  
#' \code{\link{estimate.frontier}} for constructing the estimated frontier \eqn{f(x)}.
#'
#' @export
estimate.p <- function(residuals) {
    stopifnot(is.numeric(residuals), length(residuals) > 0)
    return(sqrt(3 / (2 * mean(residuals^2))))
}
