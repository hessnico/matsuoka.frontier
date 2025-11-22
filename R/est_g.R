#' Estimate \eqn{g(x)} Using a Specified Strategy
#'
#' Provides a unified interface to estimate the regression function \eqn{g(x)}
#' using different nonparametric strategies. Strategies may be one of the
#' built-in methods or a user-supplied custom function.
#'
#' @details
#' This function follows the **Strategy design pattern**: the actual estimation
#' is delegated to a strategy function. The strategy must accept:
#'
#' - **Inputs**:
#'   - `X`: a `data.frame` of predictors  
#'   - `z`: a numeric response vector  
#'   - `...`: optional additional arguments passed to the estimator  
#'
#' - **Output**:  
#'   A `list` with components:  
#'   - `estimate`: numeric vector of fitted values  
#'   - `model`: the fitted model object  
#'   - `meta`: list of metadata (method name, call, etc.)  
#'
#' @param X A `data.frame` of input variables.
#' @param z A numeric vector of responses (e.g., `-log(y)`).
#' @param strategy Either:
#'   - A **character string** naming a registered strategy  
#'   - OR a **function** with signature `function(X, z, ...)` returning the
#'     required list (`estimate`, `model`, `meta`).
#' @param ... Additional arguments passed to the chosen strategy.
#'
#' @return A `list` with elements:
#' \itemize{
#'   \item `estimate`: numeric vector of estimated values.
#'   \item `model`: the fitted model object.
#'   \item `meta`: metadata describing the estimation method.
#' }
#'
#' You may also register additional custom strategies via
#' \code{\link{register_strategy}}.
#'
#' @seealso
#' \code{\link{register_strategy}} for adding new strategies.  
#' \code{\link{estimate}} for the full three-step frontier estimation workflow.
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
        if (!exists(strategy, envir = .strategy_env, inherits = FALSE)) {
            stop(sprintf("Unknown strategy '%s'. Did you register it?", strategy))
        }
        
        fn <- get(strategy, envir = .strategy_env, inherits = FALSE)
        res <- fn(X, z, ...)
        stopifnot(is.list(res), !is.null(res$estimate), !is.null(res$model), !is.null(res$meta))
        return(res)
    }
    
    stop("Invalid 'strategy': must be a function or a registered strategy name.")
}
