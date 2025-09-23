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
#' @seealso \link[stats]{pgamma} 
#' @export
#' 
#' @importFrom stats pgamma
cmatsuoka <- function(x, p) {
    if (!is.numeric(x)) stop("`x` must be numeric.")
    if (!is.numeric(p) || length(p) != 1 || is.na(p)) stop("`p` must be a single numeric (non-NA).")
    if (p <= 0) stop("`p` must be positive.")
    if (any(x < 0)) stop("All `x` values must be >= 0.")
    
    a <- 1.5
    
    result <- sapply(x, function(xi) {
        if (xi >= 1) {
            1
        } else {
            2 / sqrt(pi) * stats::pgamma(-p*log(xi), a, lower.tail = FALSE) * gamma(a)
        }
    })
    
    return(result)
}

#' Quantile function of the Matsuoka distribution
#'
#' Computes the inverse CDF (quantile function) of the Matsuoka distribution
#' with parameter `p`.
#'
#' @param q Numeric vector of probabilities (values in [0,1]).
#' @param p Positive numeric scalar, distribution parameter.
#' @param igamma_inv_fun Optional function to compute the inverse incomplete gamma.
#'        Defaults to `Igamma.inv` from the zipfR package.
#' @return Numeric vector of quantiles corresponding to `q`.
#' @export
F.mv.i <- function(q, p, igamma_inv_fun = zipfR::Igamma.inv) {
    if (!is.numeric(q) || any(q < 0 | q > 1)) {
        stop("`q` must be numeric values in [0,1].")
    }
    if (!is.numeric(p) || length(p) != 1 || p <= 0 || is.na(p)) {
        stop("`p` must be a single positive numeric value.")
    }
    
    quantiles <- exp(-1 / p * igamma_inv_fun(a = 1.5, y = q * sqrt(pi) / 2, lower = FALSE))
    return(quantiles)
}

#' Random generation from the Matsuoka distribution
#'
#' Generates random samples from the Matsuoka distribution using the
#' inverse CDF method.
#'
#' @param n Number of samples to generate (positive integer).
#' @param p Positive numeric scalar, distribution parameter.
#' @param igamma_inv_fun Optional function to compute the inverse incomplete gamma.
#'        Defaults to `Igamma.inv` from the zipfR package.
#' @return Numeric vector of length `n` of random Matsuoka samples.
#' @examples
#' set.seed(123)
#' rmv(10, p = 0.5)
#' @export
rmv <- function(n, p, igamma_inv_fun = zipfR::Igamma.inv) {
    if (!is.numeric(n) || length(n) != 1 || n <= 0) {
        stop("`n` must be a single positive number.")
    }
    
    u <- stats::runif(n)
    
    F.mv.i(q = u, p = p, igamma_inv_fun = igamma_inv_fun)
}
