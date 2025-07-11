#' Estimate the production frontier f(x)
#'
#' @param g_hat Numeric vector of estimated g(x).
#' @param p_hat Numeric scalar estimate of parameter p.
#' $p$ has to be greater than zero, as cited in the article
#'
#' @return Numeric vector of estimated frontier values f(x).
#' @export
estimate_frontier <- function(g_hat, p_hat) {
    stopifnot(is.numeric(g_hat), length(g_hat) > 0)
    stopifnot(is.numeric(p_hat), length(p_hat) == 1, p_hat > 0)
    
    f_hat <- exp((3 / (2 * p_hat)) - g_hat)
    return(f_hat)
}