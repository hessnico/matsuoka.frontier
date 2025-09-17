#' Estimate g(x) using a specified strategy
#'
#' This function provides a unified interface to estimate the function \eqn{g(x)}
#' using different nonparametric strategies. Strategies can be built-in methods
#' (e.g., spline, GAM) or user-supplied custom functions.
#'
#' @details
#' The estimation process relies on the **Strategy design pattern**, where the 
#' actual estimation is delegated to a strategy function. The strategy must accept
#' the following inputs and return a standardized output:
#'
#' - **Input**: 
#'   - `X`: a `data.frame` of predictors.  
#'   - `z`: a numeric response vector.  
#'   - `...`: additional arguments passed to the underlying estimator.
#'
#' - **Output**: 
#'   - A `list` with components:  
#'     - `estimate`: numeric vector of estimated values.  
#'     - `model`: the fitted model object.  
#'     - `meta`: a list with metadata about the method.  
#'
#' @param X A `data.frame` of input variables.
#' @param z A numeric vector of responses (e.g., `-log(y)`).
#' @param strategy A character string specifying the strategy name (e.g., `"spline"`, `"gam"`) 
#'   or a custom function with the signature `function(X, z, ...)`.
#' @param ... Additional arguments passed to the chosen strategy.
#'
#' @return A `list` with elements:
#' \itemize{
#'   \item `estimate`: numeric vector of estimated values.
#'   \item `model`: fitted model object.
#'   \item `meta`: list of metadata describing the estimation method.
#' }
#'
#' @section Available Strategies:
#' The following strategies are currently registered:
#' \itemize{
#'   \item `"spline"`: Fits a smoothing spline using \code{stats::smooth.spline}.
#'   \item `"gam"`: Fits a generalized additive model using \code{mgcv::gam}.
#' }
#'
#' @export
estimate.g <- function(X, z, strategy = "gam", ...) {
    if (!is.data.frame(X)) stop("X must be a data.frame.")
    
    if (is.function(strategy)) {
        res <- strategy(X, z, ...)
        stopifnot(is.list(res), !is.null(res$estimate))
        return(res)
    }
    
    if (is.character(strategy) && length(strategy) == 1) {
        fn <- get(strategy, envir = .strategy_env, inherits = FALSE)
        if (!is.function(fn)) {
            stop(sprintf("Unknown strategy '%s'. Did you register it?", strategy))
        }
        res <- fn(X, z, ...)
        stopifnot(is.list(res), !is.null(res$estimate))
        return(res)
    }
    
    stop("Invalid 'strategy': must be a function or a registered strategy name.")
}
