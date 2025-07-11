#' matsuoka3step Class Constructor
#'
#' @param x A data.frame of input variables.
#' @param y A numeric vector of outputs (must be > 0).
#' @param g A strategy for estimating g(x) ("spline" or custom function).
#' @param ... Extra args passed to g(x) estimator.
#'
#' @return An object of class `matsuoka3step`.
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
            residuals = est$residuals,
            p_hat = est$p_hat,
            f_hat = est$f_hat
        ),
        class = "matsuoka3step"
    )
}
