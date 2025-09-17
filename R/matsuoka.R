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
dmatsuoka <- function(x, p) {
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
#' Following **stats::pgamma** documentation page, \eqn{\Gamma(a,t)} can be computed using
#' \code{pgamma(t, shape = a, lower.tail = FALSE)} which returns the 
#' upper incomplete gamma function value.
#'
#' @param x numeric vector. Values at which to evaluate the cumulative function.
#'          Must be non-NA and numeric.
#' @param p positive numeric scalar parameter (must be > 0).
#' @return A list of numeric values in [0,1].
#' @examples
#' cmatsuoka(0.5, p = 2)
#' cmatsuoka(1, p = 2)
#' cmatsuoka(0, p = 2)
#' @seealso \code{\link[stats]{pgamma}}, \code{\link{gamma}}
#' @export
cmatsuoka <- function(x, p) {
    if (!is.numeric(x)) stop("`x` must be numeric.")
    if (!is.numeric(p) || length(p) != 1 || is.na(p)) stop("`p` must be a single numeric (non-NA).")
    if (p <= 0) stop("`p` must be positive.")
    if (any(x < 0)) stop("All `x` values must be >= 0.")
    
    a <- 3/2
    
    # Compute cumulative values
    result <- sapply(x, function(xi) {
        if (xi >= 1) {
            1
        } else {
            2 / sqrt(pi) * stats::pgamma(-p*log(xi), a, lower.tail = FALSE) * gamma(a)
        }
    })
    
    return(result)
}

#' Random generation from the Matsuoka distribution
#'
#' @param n number of observations
#' @param p positive shape parameter
#' @return numeric vector of length n with random draws
#' @export
rmatsuoka <- function(n, p) {
    if (!is.numeric(n) || length(n) != 1 || n <= 0) {
        stop("`n` must be a positive integer.")
    }
    if (!is.numeric(p) || length(p) != 1 || is.na(p) || p <= 0) {
        stop("`p` must be a single positive numeric parameter.")
    }
    
    # 1. generate uniforms
    u <- runif(n)
    
    # 2. define vectorized CDF
    cdf <- function(x) vapply(x, function(z) c.matsuoka(z, p), numeric(1))
    
    # 3. numeric inverse via uniroot for each u
    q <- vapply(u, function(ui) {
        if (ui <= 0) return(0)
        if (ui >= 1) return(1)
        uniroot(function(x) cdf(x) - ui, interval = c(0, 1))$root
    }, numeric(1))
    
    return(q)
}