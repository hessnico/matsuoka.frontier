.strategy_env <- new.env(parent = emptyenv())

#' @export
register_strategy <- function(name, fn) {
    stopifnot(is.character(name), length(name) == 1)
    if (!is.function(fn)) stop("fn must be a function(x, z, ...) returning list(estimate, model, meta).")
    assign(name, fn, envir = .strategy_env)
}

#' @importFrom stats smooth.spline predict
spline <- function(x, z, ...) {
    if (ncol(x) != 1) {
        stop("Spline strategy currently supports univariate input only.")
    }
    x_vec <- x[[1]]
    fit <- smooth.spline(x = x_vec, y = z, ...)
    estimate <- as.numeric(stats::predict(fit, x_vec, deriv = 1)$y)
    return(
        list(estimate = estimate, model = fit, meta = list(method = "smooth.spline", call = match.call()))
    )
}

#' @importFrom mgcv gam
gam <- function(x, z, ...) {
    terms <- paste0("s(", names(x), ")", collapse = " + ")
    formula <- stats::as.formula(paste("z ~", terms))
    df <- cbind(z = z, x)
    fit <- mgcv::gam(formula, data = df, ...)
    
    estimate <- predict(fit, newdata = x)
    return(
        list(estimate = estimate, model = fit, meta = list(method = "gam", call = match.call()))
    )
}


