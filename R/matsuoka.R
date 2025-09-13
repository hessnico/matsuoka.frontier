#' Density function for Matsuoka 
#'
#' Computes the **Matsuoka density function** for a numeric vector of values in the interval (0,1).
#'
#' The density is defined as:
#' \deqn{f(x; p) = 2 \sqrt{\frac{-p^3 \log(x)}{\pi}} \, x^{p-1}, \quad 0 < x < 1, \; p > 0}
#'
#' @param x Numeric vector. 
#' @param p Positive numeric parameter of the density. Must be greater than 0.
#'
#' @return A numeric vector of the same length as `x`, giving the density values. Positions where `x <= 0`, `x >= 1`, or `x` is `NA` return 0.
#'
#' @export
d.matsuoka <- function(x, p) {
    if (p <= 0) stop("p must be positive.")
    
    ind <- which(!is.na(x) & (x > 0) & (x < 1))

    out <- numeric(length(x))
    out[ind] <- 2 * sqrt((-p^3 * log(x[ind])) / pi) * (x[ind]^(p - 1))
    return(out)
}

#' Distribution function for Matsuoka
#'
#' @details
#' The cumulative Matsuoka function is defined as
#' \deqn{
#'   F_p(x) = \frac{2}{\sqrt{\pi}} \, \Gamma\!\left(\tfrac{3}{2}, -p \ln(x)\right) \, I(0 < x < 1)
#'            \;+\; I(x \geq 1)
#' }
#' where \eqn{\Gamma(a, t)} is the upper incomplete gamma function
#' and \eqn{I(\cdot)} denotes the indicator function.
#'
#' As mentioned \eqn{\Gamma(a, t)} is the upper incomplete gamma and
#' \code{pgamma(t, shape = a, lower.tail = FALSE)} returns the regularized
#' upper incomplete gamma \eqn{Q(a,t)=\Gamma(a,t)/\Gamma(a)} following \code{link[stats]{pgamma}} documentation.
#'
#' @param x numeric scalar. Value at which to evaluate the cumulative function.
#'          Must be a single non-NA numeric value.
#' @param p positive numeric scalar parameter (must be > 0).
#' @return A numeric scalar in \[0, 1\].
#' @examples
#' c.matsuoka(0.5, p = 2)
#' c.matsuoka(1, p = 2)
#' c.matsuoka(0, p = 2)
#' @seealso \code{\link[stats]{pgamma}}, \code{\link{gamma}}
#' @export
c.matsuoka <- function(x, p) {
    if (!is.numeric(x) || length(x) != 1) stop("`x` must be numeric.")
    if (!is.numeric(p) || length(p) != 1 || is.na(p)) stop("`p` must be a single numeric (non-NA).")
    if (p <= 0) stop("`p` must be positive.")
    
    x <- as.numeric(x)
    
    if (x < 0) {
        return(0)
    }
    
    if (x > 1) {
        return(1)
    }
    
    a <- 3/2
    return(
        2/sqrt(pi) * 
            stats::pgamma(-p*log(x), a, lower.tail = FALSE) * gamma(a)
    )
}
