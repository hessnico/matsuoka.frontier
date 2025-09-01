#' Summary for matsuoka3step
#' 
#' @importFrom stats median
#'
#' @param x A 'matsuoka3step' object.
#' @param digits Number of digits to print.
#' @param tol Tolerance to treat values as efficient (score >= 1 - tol).
#' @param ... unused
#' @export
summary.matsuoka3step <- function(x, digits = 4, tol = 1e-8, ...) {
    if (!inherits(x, "matsuoka3step")) {
        stop("object must be of class 'matsuoka3step'")
    }
    
    eff <- as.numeric(x$efficiency)
    eff_stats <- calculate.efficiency.stats(eff)
    
    gcall <- NULL
    if (!is.null(x$g_hat.fn_obj.call)) {
        gcall <- x$g_hat.fn_obj.call
    } else if (!is.null(x$g_hat) && is.list(x$g_hat)) {
        if (!is.null(x$g_hat$fn_obj) && !is.null(x$g_hat$fn_obj$call)) {
            gcall <- x$g_hat$fn_obj$call
        } else if (!is.null(x$g_hat$call)) {
            gcall <- x$g_hat$call
        }
    }
    
    model_call <- x$call %||% x$est.call %||% NULL
    gcall_txt   <- get_deparsed(gcall)
    model_call_txt <- get_deparsed(model_call)
    
    cat("Summary for 'matsuoka3step'\n\n")
    if (!is.null(model_call_txt)) {
        cat("Model call:\n  ", model_call_txt, "\n\n")
    }
    if (!is.null(gcall_txt)) {
        cat("g(x) call (g_hat function):\n  ", gcall_txt, "\n")
        cat("    - For more info of g(x) estimation, please use summary(...$g_hat$fn_obj) \n\n")
    }
    
    n <- length(eff)
    n_efficient <- sum(eff >= 1 - tol, na.rm = TRUE)
    pct_efficient <- 100 * n_efficient / n
    
    cat("Efficiency summary:\n")
    print(round(eff_stats, digits = digits))
    cat("\n")
    cat(sprintf("%% efficient (score >= 1 - %g): %.2f%% (%d of %d)\n",
                tol, pct_efficient, n_efficient, n))
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
    paste0(deparse(x), collapse = " ")
}
