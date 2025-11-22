#' Summary method for `matsuoka3step` objects
#'
#' Provides a concise summary of the results produced by
#' `matsuoka3step()`, including the estimated efficiency scores,
#' efficiency statistics, the estimated Matsuoka parameter `p`, and
#' the calls used to estimate \eqn{g(x)} and to fit the frontier model.
#'
#' @param object An object of class `"matsuoka3step"`.
#' @param digits Integer. Number of digits to display for printed numerical
#'   output. Defaults to 4.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' This method prints:
#' \itemize{
#'   \item The original model call used in `matsuoka3step()`.
#'   \item The call used to estimate the \eqn{g(x)} component.
#'   \item Summary statistics of the efficiency scores, including:
#'     \itemize{
#'       \item Minimum, median, mean, maximum efficiency;
#'       \item Quantiles;
#'       \item Percentage of fully efficient units (efficiency \eqn{\ge 1}).
#'     }
#'   \item The estimated Matsuoka parameter `p`.
#' }
#'
#' Users can inspect the underlying \eqn{g(x)} model via:
#' `summary(object$g_hat$model)`.
#'
#' @return (Invisibly) a list containing:
#' \describe{
#'   \item{efficiency}{Vector of efficiency scores.}
#'   \item{efficiency_summary}{Summary statistics.}
#'   \item{n}{Number of observations.}
#'   \item{n_efficient}{Count of fully efficient observations (eff \eqn{\ge 1}).}
#'   \item{pct_efficient}{Percentage of efficient observations.}
#'   \item{model_call}{The recorded model call.}
#'   \item{gcall}{The recorded \eqn{g(x)} estimation call.}
#' }
#'
#' @seealso
#'   `matsuoka3step()` for fitting the model,
#'   `register_strategy()` to define custom \eqn{g(x)} estimators
#'
#' @export
summary.matsuoka3step <- function(object, digits = 4, ...) {
    if (!inherits(object, "matsuoka3step")) {
        stop("object must be of class 'matsuoka3step'")
    }

    eff <- as.numeric(object$efficiency)
    eff_stats <- calculate.efficiency.stats(eff)

    get_gcall <- function(obj) {
        if (!is.null(obj$g_hat.model.call)) {
            return(obj$g_hat.model.call)
        }
        if (!is.null(obj$g_hat) && is.list(obj$g_hat)) {
            return(obj$g_hat$model$call %||% obj$g_hat$call)
        }
        NULL
    }

    gcall <- get_gcall(object)
    gcall_txt   <- get_deparsed(gcall)

    model_call <- object$call %||% object$est.call %||% NULL
    model_call_txt <- get_deparsed(model_call)

    cat("Summary for 'matsuoka3step'\n\n")
    if (!is.null(model_call_txt)) {
        cat("Model call:\n  ", model_call_txt, "\n\n")
    }
    if (!is.null(gcall_txt)) {
        cat("g(x) call (g_hat function):\n  ", gcall_txt, "\n")
        cat("")
        cat("    - For more info of g(x) estimation, please use summary(...$g_hat$model) \n\n")
    }

    n <- length(eff)
    n_efficient <- sum(eff >= 1, na.rm = TRUE)
    pct_efficient <- 100 * n_efficient / n

    cat("Efficiency summary:\n")
    print(round(eff_stats, digits = digits))
    cat("\n")
    cat(sprintf("%% efficient (score >= 1): %.2f%% (%d of %d)\n",
                pct_efficient, n_efficient, n))
    cat("\n")

    cat(sprintf("Matsuoka's p parameter estimated value: %.*f\n", digits, object$p_hat))

    cat("\n")

    invisible(list(
        efficiency = eff,
        efficiency_summary = eff_stats,
        n = n,
        n_efficient = n_efficient,
        pct_efficient = pct_efficient,
        model_call = model_call_txt,
        gcall = gcall_txt
    ))
}

#' @importFrom stats median
calculate.efficiency.stats <- function(eff) {
    return(c(
        Min = min(eff, na.rm = TRUE),
        `1st Qu.` = as.numeric(stats::quantile(eff, 0.25, na.rm = TRUE)),
        Median = median(eff, na.rm = TRUE),
        Mean = mean(eff, na.rm = TRUE),
        `3rd Qu.` = as.numeric(stats::quantile(eff, 0.75, na.rm = TRUE)),
        Max = max(eff, na.rm = TRUE)
    ))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

get_deparsed <- function(x) {
    if (is.null(x)) return(NULL)
    paste0(deparse(x), collapse = "\n")
}
