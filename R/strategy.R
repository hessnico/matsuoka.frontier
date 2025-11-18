.strategy_env <- new.env(parent = emptyenv())

#' Register a Custom Strategy for Estimating \eqn{g(x)}
#'
#' Registers a user-defined strategy function into the internal strategy 
#' environment used by \code{estimate.g()}.  
#' This enables users to extend the set of available nonparametric estimators
#' by providing their own modeling routines.
#'
#' @param name A character string of length 1.  
#'   The name under which the strategy will be registered.
#'
#' @param fn A function with signature \code{function(X, z, ...)} that performs 
#'   the estimation of \eqn{g(x)}.  
#'   The function **must return a list** with the following components:
#'   \describe{
#'     \item{estimate}{Numeric vector of estimated values \eqn{\hat g(x)}.}
#'     \item{model}{The fitted model object produced by the strategy.}
#'     \item{meta}{A list containing metadata (e.g., method name, call, parameters).}
#'   }
#'
#' @details
#' The registered strategy becomes accessible through \code{estimate.g()} by
#' passing its name to the argument \code{strategy}.  
#' This mechanism follows the **Strategy design pattern**, allowing flexible
#' substitution of nonparametric methods without modifying existing code.
#'
#' Once registered, a strategy can be used like:
#' \preformatted{
#'   register_strategy("my_method", function(X, z, ...) {
#'       # custom estimation
#'       list(
#'           estimate = fitted_values,
#'           model = model_object,
#'           meta = list(method = "my_method")
#'       )
#'   })
#'
#'   estimate.g(X, z, strategy = "my_method")
#' }
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of storing
#'   the strategy in the internal environment.
#'
#' @seealso 
#'   `register_strategy()`, `estimate.g()`, `matsuoka3step()`,  
#'   and the vignette section *"Registering a new method with register_strategy()"*.
#'
#' @export
register_strategy <- function(name, fn) {
    stopifnot(is.character(name), length(name) == 1)
    if (!is.function(fn)) stop("fn must be a function(x, z, ...) returning list(estimate, model, meta).")
    assign(name, fn, envir = .strategy_env)
}

#' @importFrom stats smooth.spline predict
.spline <- function(x, z, ...) {
    if (!is.data.frame(x)) stop("x must be a data.frame.")
    if (ncol(x) != 1L) stop("Spline strategy supports only univariate input.")
    
    x_vec <- x[[1]]
    fit <- smooth.spline(x = x_vec, y = z, ...)
    estimate <- as.numeric(stats::predict(fit, x_vec, deriv = 1)$y)
    return(
        list(estimate = estimate, model = fit, meta = list(method = "spline", call = match.call()))
    )
}

#' @importFrom mgcv gam
.gam <- function(x, z, ...) {
    if (!is.data.frame(x)) stop("x must be a data.frame.")
    if (ncol(x) < 1L) stop("At least one predictor is required.")
    
    dots <- list(...)
    if (!is.null(dots$formula)) {
        formula <- dots$formula
        dots$formula <- NULL
    } else {
        terms <- paste0("s(", names(x), ")", collapse = " + ")
        formula <- stats::as.formula(paste("z ~", terms))
    }
    df <- cbind(z = z, x)
    fit <- mgcv::gam(formula, data = df, ...)
    
    estimate <- predict(fit, newdata = x)
    return(
        list(estimate = estimate, model = fit, meta = list(method = "gam", call = match.call()))
    )
}

#' @importFrom scar scar
.scar <- function(x, z, ...) {
    if (is.data.frame(x)) {
        if (ncol(x) == 1L) {
            x <- as.numeric(x[[1L]])
        } else {
            x <- as.matrix(x)
        }
    }
    
    fit <- scar::scar(x = x, y = z, ...)
    estimate <- predict(fit, newdata = x)
    
    list(
        estimate = as.numeric(estimate),
        model = fit,
        meta = list(
            method = "scar",
            call = match.call()
        )
    )
}

#' @importFrom KernSmooth locpoly
.locpoly <- function(x, z, ...) {
    if (!is.data.frame(x)) stop("x must be a data.frame.")
    if (ncol(x) != 1L) stop("local polynomial supports only univariate input.")
    
    x_vec <- x[[1]]
    fit <- KernSmooth::locpoly(x = x_vec, y = z, ...)
    estimate <- stats::approx(fit$x, fit$y, xout = x_vec, rule = 2)$y
    
    list(
        estimate = as.numeric(estimate),
        model = fit,
        meta = list(
            method = "locpoly",
            call = match.call()
        )
    )
}

#' @importFrom RBF backf.cl
.backf.cl <- function(x, z, ...) {
    if (!is.data.frame(x)) stop("x must be a data.frame.")
    if (ncol(x) < 1L) stop("At least one predictor is required.")
    
    df <- cbind(z = z, x)
    terms <- paste(names(x), collapse = " + ")
    formula <- stats::as.formula(paste("z ~", terms))
    
    fit <- RBF::backf.cl(formula = formula, data = df, ...)
    est_mat <- fit$g.matrix
    estimate <- fit$alpha + rowSums(est_mat)
    
    list(
        estimate = as.numeric(estimate),
        model = fit,
        meta = list(method = "backf.cl", call = match.call())
    )
}

#' @importFrom wsbackfit sback sb
.sback <- function(x, z, ...) {
    if (!is.data.frame(x)) stop("x must be a data.frame.")
    if (ncol(x) < 1L) stop("At least one predictor is required.")
    
    df <- data.frame(z = z, x)
    terms <- paste("sb(", names(x), ")", collapse = " + ")
    
    env <- environment()
    env$sb <- wsbackfit::sb
    
    formula <- stats::as.formula(paste("z ~", terms))
    fit <- wsbackfit::sback(formula, data = df)
    
    list(
        estimate = fit$fitted.values,
        model = fit,
        meta = list(method = "sback", call = match.call())
    )
}

#' @importFrom RBF backf.rob
.backf.rob <- function(x, z, ...) {
    if (!is.data.frame(x)) stop("x must be a data.frame.")
    if (ncol(x) < 1L) stop("At least one predictor is required.")
    
    df <- cbind(z = z, x)
    terms <- paste(names(x), collapse = " + ")
    formula <- stats::as.formula(paste("z ~", terms))
    
    fit <- RBF::backf.rob(formula = formula, data = df, ...)
    est_mat <- fit$g.matrix
    estimate <- fit$alpha + rowSums(est_mat)
    
    list(
        estimate = as.numeric(estimate),
        model = fit,
        meta = list(method = "backf.rob", call = match.call())
    )
}