#' Matsuoka Density Function
#'
#' Computes the **Matsuoka density function** for a numeric vector of values in the interval (0,1).
#'
#' The density is defined as:
#' \deqn{f(x; p) = 2 \sqrt{\frac{-p^3 \log(x)}{\pi}} \, x^{p-1}, \quad 0 < x < 1, \; p > 0}
#'
#' @param x Numeric vector. Values must lie strictly between 0 and 1. `NA` values are allowed and returned as 0.
#' @param p Positive numeric parameter of the density. Must be greater than 0.
#'
#' @return A numeric vector of the same length as `x`, giving the density values. Positions where `x <= 0`, `x >= 1`, or `x` is `NA` return 0.
#'
#' @export
matsuoka.density <- function(x, p) {
    if (p <= 0) stop("p must be positive.")
    
    ind <- which(!is.na(x) & (x > 0) & (x < 1))

    out <- numeric(length(x))
    out[ind] <- 2 * sqrt((-p^3 * log(x[ind])) / pi) * (x[ind]^(p - 1))
    return(out)
}
