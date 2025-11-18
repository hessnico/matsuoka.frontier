#' Estimate the Production Frontier \eqn{f(x)}
#'
#' Computes the plug-in estimator of the production frontier
#' \eqn{f(x)} using the estimated regression function \eqn{g(x)} and
#' the estimated parameter \eqn{p}.
#'
#' The estimator is defined as:
#' \deqn{
#' \hat{f}(x) = \exp\!\left( \frac{3}{2\hat{p}} - \hat{g}(x) \right),
#' \qquad p > 0.
#' }
#'
#' @param g_hat Numeric vector containing the estimated values of \eqn{g(x)}.
#' @param p_hat Numeric scalar estimate of the parameter \eqn{p}.  
#'   Must satisfy \code{p_hat > 0}, as required by the Matsuoka distribution.
#'
#' @return A numeric vector of the same length as \code{g_hat}, containing the
#'   estimated frontier values \eqn{f(x)}.
#'
#' @seealso
#'   \code{\link{estimate}} for the full three-step frontier estimation workflow.  
#'   \code{\link{estimate.g}} for estimating \eqn{g(x)}.  
#'   \code{\link{estimate.p}} for estimating the parameter \eqn{p}.
#'
#' @export
estimate.frontier <- function(g_hat, p_hat) {
    stopifnot(is.numeric(g_hat), length(g_hat) > 0)
    stopifnot(is.numeric(p_hat), length(p_hat) == 1, p_hat > 0)
    
    f_hat <- exp((3 / (2 * p_hat)) - g_hat)
    return(f_hat)
}