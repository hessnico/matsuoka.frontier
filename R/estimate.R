#' Run the Three-Step Frontier Estimation
#'
#' This function estimates the production frontier using the three-step method:
#' (1) Any nonparametric regression to estimate g(x),
#' (2) estimation of the parameter p via method of moments,
#' (3) construction of f(x) via plug-in.
#'
#' @param X A data.frame of input variables
#' @param y A numeric output vector
#' @param g_strategy Method for estimating g(x). Options: "splines"
#'  TODO: Options: "spline", or "scar"
#' @param ... Additional arguments passed to specific `g` estimators.
#'
#' @return A list with elements: g_hat, residuals, p_hat, f_hat.
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
