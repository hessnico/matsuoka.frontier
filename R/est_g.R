#' Estimate g(x) using specified strategy
#'
#' @param X A data.frame of input variables 
#' @param z Numeric vector (-log(y))
#' @param strategy Character string specifying the strategy ("spline") or a custom function 
#' for estimating function g
#' @param ... Additional parameters passed to the spline function or custom function
#'
#' @return List with element `estimate` (numeric vector).
#' @export
#' @importFrom mgcv gam
#' @importFrom KernSmooth locpoly
#' @importFrom stats smooth.spline predict sd approx
estimate.g <- function(X, z, strategy = "spline", ...) {
    if (!is.data.frame(X)) stop("X must be a data.frame.")
    
    if (is.function(strategy)) {
        res <- strategy(X, z, ...)
        stopifnot(is.list(res), !is.null(res$estimate))
        return(res)
    }
    
    if (strategy == "spline") {
        if (ncol(X) != 1) {
            stop("Spline strategy currently supports univariate input only.")
        }
        fit <- smooth.spline(x = X[[1]], y = z, ...)
        return(list(estimate = predict(fit, x = X[[1]])$y))
    } else if (strategy == "gam") {
        terms <- paste0("s(", names(X), ")", collapse = " + ")
        formula <- stats::as.formula(paste("z ~", terms))
        
        fit <- mgcv::gam(formula, data = cbind(z = z, X), ...)
        return(list(estimate = predict(fit, newdata = X)))
    }
    
    stop("Unknown strategy. Only 'spline' or a custom function supported.")
}