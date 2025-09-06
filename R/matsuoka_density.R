#' Implements Matsuoka density function
#'
#' @param x numeric vector of values \eqn{x \contains (0,1)}
#' @param p positive parameter.
#' @return numeric vector of density values.
#' @export
matsuoka.density <- function(x, p) {
    if (p <= 0) stop("p must be positive.")
    
    ind <- which(!is.na(x) & (x > 0) & (x < 1))

    out <- numeric(length(x))
    out[ind] <- 2 * sqrt((-p^3 * log(x[ind])) / pi) * (x[ind]^(p - 1))
    return(out)
}
