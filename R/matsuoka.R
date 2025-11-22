#' Probability Density Function of the Matsuoka Distribution
#'
#' Computes the probability density function (PDF) of the Matsuoka distribution
#' with parameter \code{p}.
#'
#' The density is defined as:
#' \deqn{
#' f(x; p) = 2 \sqrt{\frac{-p^3 \log(x)}{\pi}} \, x^{p-1},
#'  \quad 0 < x < 1, \; p > 0
#'  }
#'
#' @param x Numeric vector of evaluation points.
#' @param p Positive numeric scalar (parameter of the distribution).
#'
#' @return A numeric vector of the same length as \code{x}, giving the density
#'   values. Values where \code{x <= 0}, \code{x >= 1}, or \code{x} is \code{NA}
#'   return 0.
#'
#' @examples
#' # Density at specific points
#' dmatsuoka(c(0.2, 0.5, 0.8), p = 0.5)
#'
#' # Plotting the density
#' curve(dmatsuoka(x, p = 0.5), from = 0, to = 1)
#' 
#' @seealso
#' \code{\link{cmatsuoka}} for the cumulative distribution function,
#' \code{\link{F.mv.i}} for the inverse Matsuoka distribution function.
#'
#' @export
#' 
dmatsuoka <- function(x, p) {
    if (p <= 0) stop("p must be positive.")
    
    ind <- which(!is.na(x) & (x > 0) & (x < 1))

    out <- numeric(length(x))
    out[ind] <- 2 * sqrt((-p^3 * log(x[ind])) / pi) * (x[ind]^(p - 1))
    return(out)
}

#' Cumulative distribution function for Matsuoka
#'
#' @details
#' The cumulative distribution function for Matsuoka distribution is defined as
#' \deqn{
#'   F_p(x) = \frac{2}{\sqrt{\pi}} \, \Gamma\!\left(\tfrac{3}{2}, -p \ln(x)\right) \, I(0 < x < 1)
#'            \;+\; I(x \geq 1)
#' }
#' where \eqn{\Gamma(a, t)} is the upper incomplete gamma function
#' and \eqn{I(\cdot)} denotes the indicator function.
#'
#' According to **stats::pgamma**'s documentation page, \eqn{\Gamma(a,t)} can be computed using
#' \code{pgamma(t, shape = a, lower.tail = FALSE)} which returns the 
#' upper incomplete gamma function value.
#'
#' See the package vignette for details.
#'
#' @param x numeric vector. Values at which to evaluate the cumulative function.
#'          Must be non-NA and numeric.
#' @param p positive numeric scalar parameter (must be > 0).
#' @return A list of numeric values in [0,1].
#' @examples
#' cmatsuoka(0.5, p = 2)
#' cmatsuoka(1, p = 2)
#' cmatsuoka(0, p = 2)
#' @seealso \link[dmatsuoka]{dmatsuoka}
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

#' Quantile Function of the Matsuoka Distribution
#'
#' Computes the inverse cumulative distribution function (quantile function)
#' of the Matsuoka distribution with parameter \eqn{p > 0}.
#' 
#' The quantile function is given by:
#' \deqn{
#' F^{-1}(q; p) = \exp\!\left(
#'   \frac{1}{p} \, \Gamma^{-1}\!\left(
#'      3/2,\; q \, \frac{\sqrt{\pi}}{2}
#'   \right)
#' \right),
#' }
#' where \eqn{\Gamma^{-1}(a, \cdot)} denotes the inverse upper incomplete
#' gamma function.
#' @param q Numeric vector of probabilities in \eqn{[0, 1]}.
#' @param p Positive numeric scalar representing the Matsuoka distribution
#'   parameter.
#' @param igamma_inv_fun Optional function used to compute the inverse incomplete
#'   gamma.  
#'   Defaults to \code{zipfR::Igamma.inv}.  
#'   The function must accept arguments \code{a}, \code{y}, and \code{lower}.
#'
#' @return A numeric vector of quantiles corresponding to the probabilities
#'   supplied in \code{q}.
#'
#' @examples
#' # Median of a Matsuoka distribution with p = 0.5
#' F.mv.i(0.5, p = 0.5)
#'
#' # Vector of quantiles
#' F.mv.i(c(0.1, 0.5, 0.9), p = 1)
#'
#' @seealso
#' \code{\link{dmatsuoka}} for the density function.  
#' \code{\link{cmatsuoka}} for the cumulative distribution function.  
#' \code{\link{rmv}} for random generation from the Matsuoka distribution.
#'
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
#' @param n Integer. Number of samples to generate (must be positive).
#' @param p Positive numeric scalar. Shape parameter of the Matsuoka distribution.
#' @param igamma_inv_fun Optional function computing the inverse incomplete gamma.
#'   Defaults to `Igamma.inv` from the **zipfR** package.
#'
#' @return A numeric vector of length `n` containing random variates from the
#'   Matsuoka distribution.
#'
#' @seealso
#'   `F.mv.i()` for the inverse CDF used internally,  
#'   `dmv()` and `pmv()` for the density and distribution functions,  
#'   and the vignette section *"Matsuoka distribution and its components"*  
#'   for a full description of the model.
#'
#' @examples
#' set.seed(123)
#' rmv(10, p = 0.5)
#'
#' @export
rmv <- function(n, p, igamma_inv_fun = zipfR::Igamma.inv) {
    if (!is.numeric(n) || length(n) != 1 || n <= 0) {
        stop("`n` must be a single positive number.")
    }
    
    u <- stats::runif(n)
    
    F.mv.i(q = u, p = p, igamma_inv_fun = igamma_inv_fun)
}
