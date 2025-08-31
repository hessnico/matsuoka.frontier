#' matsuoka3step Class Constructor
#'
#' @param x A data.frame of input variables.
#' @param y A numeric vector of outputs (must be > 0).
#' @param g A strategy for estimating g(x) ("spline" or custom function).
#'   Possible entries include:
#'   \describe{
#'     \item{"spline"}{Default value. Accepts only one indepedent variable.}
#'     \item{"gam"}{Wrapper for `gam` package function. Uses smoothing splines in the formula, e.g.: \eqn{z ~ s(x_1) + s(x_2)}.}
#'   }
#' @param ... Extra args passed to g(x) estimator.
#'
#' @return An object of class `matsuoka3step`:
#' \describe{
#'   \item{x}{The dependent variables.}
#'   \item{y}{The independet variable.}
#'   \item{g_hat}{Estimated values of the function \( g(x) \).}
#'   \item{g_hat_res}{Residuals from the estimation of \( g(x) \).}
#'   \item{p_hat}{Estimated parameter \( p \).}
#'   \item{f_hat}{Estimated productive frontier.}
#' }
#'
#' @export
matsuoka3step <- function(x, y, g = "spline", ...) {
    validate.X(x)
    validate.y(y)
    
    est <- estimate(x, y, g_strategy = g, ...)
    
    structure(
        list(
            x = x,
            y = y,
            g_hat = est$g_hat,
            g_hat_res = est$residuals,
            p_hat = est$p_hat,
            f_hat = est$f_hat
        ),
        class = "matsuoka3step"
    )
}
