#' Estimate parameter p from residuals
#'
#' Estimates the shape parameter \( p \) for Matsuoka's distribution
#' using the method of moments.
#'
#' @param residuals Numeric vector of residuals (z - g_hat).
#'
#' @return Numeric estimate of p.
#' @export
estimate_p <- function(residuals) {
    stopifnot(is.numeric(residuals), length(residuals) > 0)
    return(sqrt(3 / (2 * mean(residuals^2))))
}
