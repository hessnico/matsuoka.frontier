#' matsuoka3step Class Constructor
#'
#' @param x A data.frame of input variables.
#' @param y A numeric vector of outputs (must be > 0).
#' @param g A strategy for estimating g(x) ("spline" or custom function).
#'   Possible entries include:
#'   \describe{
#'     \item{"spline"}{Default value. Accepts only one depedent variable.}
#'     \item{"gam"}{Wrapper for `gam` package function. Uses smoothing splines in the formula, e.g.: \eqn{z = s(x_1) + s(x_2)}.}
#'   }
#' @param ... Extra args passed to g(x) estimator.
#'
#' @return An object of class `matsuoka3step`:
#' \describe{
#'   \item{x}{dependent variables.}
#'   \item{y}{independet variable.}
#'   \item{efficience}{deterministic efficiency calculated, being \eqn{y/\hat{f(x)}}}
#'   \item{call}{the matched call.}
#'   \item{g_hat}{estimated values of the function \( g(x) \).}
#'   \item{g_hat_res}{residuals from the estimation of \( g(x) \).}
#'   \item{p_hat}{estimated parameter \( p \).}
#'   \item{f_hat}{estimated productive frontier.}
#' }
#'
#' @export
matsuoka3step <- function(x, y, g = "spline", ...) {
    validate.X(x)
    validate.y(y)
    
    call_this <- match.call()
    
    est <- estimate(x, y, g_strategy = g, ...)
    
    structure(
        list(
            x = x,
            y = y,
            efficiency = y / est$f_hat,
            call = call_this,
            g_hat = est$g_hat,
            g_hat_res = est$residuals,
            p_hat = est$p_hat,
            f_hat = est$f_hat
        ),
        class = "matsuoka3step"
    )
}
