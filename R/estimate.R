#' Three-Step Frontier Estimation (Matsuoka Frontier)
#'
#' Implements the three-step frontier estimation procedure based on the
#' Matsuoka distribution. The method consists of:
#' \enumerate{
#'   \item Nonparametric regression to estimate \eqn{g(x)},
#'   \item Estimation of the Matsuoka parameter \eqn{p} via the method of moments,
#'   \item Construction of the frontier estimate \eqn{f(x)}.
#' }
#'
#' @param X A data frame or matrix of input variables. Each row corresponds to
#'   one observation, and each column represents an input dimension.
#' @param y A numeric vector of outputs, strictly positive and of the same
#'   length as the number of rows in \code{X}.
#' @param g_strategy Either a character string or a function specifying the
#'   method for estimating the regression function \eqn{g(x)}.
#'   See \code{\link{matsuoka3step}} for the complete list of available 
#'   strategies and instructions for implementing custom estimators.
#' 
#'   If a **function**, it must have signature \code{function(x, z, ...)} and
#'   return a list containing at least the element \code{estimate}, and typically
#'   also \code{model} and \code{meta}. If wanted, custom functions can be registered using
#'   \code{\link{register_strategy}}.
#' @param ... Additional arguments passed to the underlying \code{estimate.g()}
#'   method depending on \code{g_strategy}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{g_hat}{The fitted nonparametric regression object returned by
#'     \code{estimate.g()}.}
#'   \item{residuals}{The vector of residuals \eqn{z - \hat{g}(x)}, where
#'     \eqn{z = -\log(y)}.}
#'   \item{p_hat}{The estimated Matsuoka parameter obtained via the method of
#'     moments.}
#'   \item{f_hat}{The estimated frontier values computed}
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' X <- data.frame(x = runif(100))
#' y <- exp(-0.5 * X$x) * runif(100, 0.8, 1)
#'
#' fit <- estimate(X, y, g_strategy = "splines")
#' fit$p_hat
#' }
#'
#' @seealso
#'   \code{\link{estimate.g}},
#'   \code{\link{estimate.p}},
#'   \code{\link{estimate.frontier}},
#'
#' @export
estimate <- function(X, y, g_strategy = "splines", ...) {
    z <- -log(y)

    g_res <- estimate.g(X, z, strategy = g_strategy, ...)
    
    residuals <- z - g_res$estimate
    p_hat <- estimate.p(residuals)
    f_hat <- estimate.frontier(g_res$estimate, p_hat)
    
    list(
        g_hat = g_res,
        residuals = residuals,
        p_hat = p_hat,
        f_hat = f_hat
    )
}
