#' Matsuoka 3-Step Estimation Procedure
#'
#' Executes the full estimation pipeline:
#' 1) estimate \eqn{g(x)} using a selected strategy,
#' 2) estimate the Matsuoka shape parameter \eqn{p},
#' 3) compute the productive frontier \eqn{f(x)}.
#'
#' @param x A data.frame of input variables.
#' @param y A numeric vector of outputs (must be > 0).
#'
#' @param g A strategy for estimating \eqn{g(x)}. The value may be:
#'   - a character string naming a built-in strategy, or  
#'   - a user-defined function with signature \code{fun(X, z, ...)}.
#'   
#' @section Available Strategies:
#' The following strategy names are currently registered and available:
#'
#' \itemize{
#'   \item `"spline"` — Smoothing spline using \code{stats::smooth.spline}.
#'   \item `"gam"` — Generalized Additive Model using \code{mgcv::gam}.
#'   \item `"scar"` — Shape-Constrained Additive Regression using \code{scar::scar}.
#'   \item `"locpoly"` — Local polynomial regression using \code{KernSmooth::locpoly}.
#'   \item `"backf.cl"` — Backfitting using componentwise linear functions via \code{RBF::backf.cl}.
#'   \item `"sback"` — Smooth backfitting using \code{wsbackfit::sback}.
#'   \item `"backf.rob"` — Robust backfitting using \code{RBF::backf.rob}.
#' }
#' @details
#' The estimation of \eqn{g(x)} follows a **Strategy design pattern**.
#' See \code{\link{estimate.g}} for the complete list of available
#' strategies and instructions for implementing custom estimators.
#'
#' @param ... Additional parameters passed to the selected g-strategy.
#'
#' @return A `matsuoka3step` object containing:
#' \describe{
#'   \item{x}{Input variables.}
#'   \item{y}{Output variable.}
#'   \item{efficiency}{Deterministic efficiency estimate: \eqn{y / \hat f(x)}.}
#'   \item{call}{The matched call.}
#'   \item{g_hat}{Estimated \eqn{g(x)}.}
#'   \item{g_hat_res}{Residuals from the \eqn{g(x)} estimation.}
#'   \item{p_hat}{Estimated parameter \eqn{p}.}
#'   \item{f_hat}{Estimated frontier.}
#' }
#'
#' @seealso
#'   \code{\link{estimate.g}} for available strategies;  
#'   \code{\link{estimate}} for the full 3-step algorithm;  
#'   \code{\link{estimate.p}} for estimation of \eqn{p};  
#'   \code{\link{cmatsuoka}} and \code{\link{F.mv.i}} for distribution functions.
#'
#' @export
matsuoka3step <- function(x, y, g = "spline", ...) {
    validate.X(x)
    validate.y(y)
    
    call_this <- match.call()
    
    est <- estimate(x, y, g_strategy = g, ...)
    
    structure(
        list(
            x = x,
            y = y,
            efficiency = y / est$f_hat,
            call = call_this,
            g_hat = est$g_hat,
            g_hat_res = est$residuals,
            p_hat = est$p_hat,
            f_hat = est$f_hat
        ),
        class = "matsuoka3step"
    )
}
