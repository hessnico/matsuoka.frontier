#' Plot Method for `matsuoka3step` Objects
#'
#' Generates diagnostic visualizations for a fitted frontier model.
#' The available plots include:
#' \enumerate{
#'   \item The fitted Matsuoka density function derived from the estimated parameter \code{p_hat};
#'   \item A filled contour plot of the estimated production frontier
#'         (requires two-dimensional input \code{x}).
#' }
#'
#' @param x An object of class \code{matsuoka3step}.
#' @param which Specifies which plot(s) to display:
#'   \describe{
#'     \item{\code{1}}{Density plot.}
#'     \item{\code{2}}{Frontier contour plot (only for 2D inputs).}
#'     \item{\code{c(1, 2)} or \code{"both"}}{Produce both plots.}
#'   }
#' @param ngrid Integer; number of grid points per axis used for interpolation
#'   in the contour plot.
#' @param counter_levels Integer; number of contour levels to draw (contour plot only).
#' @param ask Logical; if \code{TRUE}, the user will be prompted before each plot.
#' @param ... Additional graphical parameters passed to the underlying plotting
#'   functions.
#'
#' @details
#' The density plot visualizes the estimated Matsuoka density based solely on
#' \code{p_hat}.
#' The contour plot is based on an interpolated grid of \eqn{\hat{f}(x)} values
#' using the information stored in the \code{matsuoka3step} object.
#'
#' @return Returns \code{NULL} invisibly. Called for its side effects (plots).
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics filled.contour grid lines par
#'
#' @seealso
#'   \code{\link{cmatsuoka}} for the distribution function of the Matsuoka model;
#'   \code{\link{den.plot}} for the density visualization helper.
#'
#' @export
plot.matsuoka3step <- function(x,
                               which = NULL,
                               ngrid = 500,
                               counter_levels = 8,
                               ask = FALSE,
                               ...) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    if (ask && interactive()) par(ask = TRUE)

    validate.matsuoka.plot(x)

    if (is.null(which)) {
        which <- c(1, 2, 3)
    }

    if (1 %in% which) {
        den.plot(x)
    }

    if (2 %in% which && ncol(x$x) == 2) {
        contour.plot.helper(x, ngrid = ngrid, counter_levels = counter_levels, ...)
    }

    invisible(NULL)
}

#' Validate the object and inputs
#' @keywords internal
validate.matsuoka.plot <- function(x) {
    if (!inherits(x, "matsuoka3step")) {
        stop("x must be of class 'matsuoka3step'.")
    }
    if (is.null(x$x) || is.null(x$f_hat) || is.null(x$p_hat) || is.null(x$y)) {
        stop("object must contain elements 'x', 'y', 'f_hat', and 'p_hat'.")
    }
    if (x$p_hat <= 0) {
        stop("p_hat must be positive.")
    }
}

#' Filled contour plot helper
#' @keywords internal
contour.plot.helper <- function(x, ngrid = 500, counter_levels = 8, ...) {
    x1 <- x$x[, 1]
    x2 <- x$x[, 2]

    z  <- as.numeric(x$f_hat)

    interp_grid <- interp::interp(
        x = x1, y = x2, z = z,
        nx = ngrid, ny = ngrid,
        linear = TRUE, extrap = FALSE
    )

    breaks <- pretty(range(interp_grid$z, na.rm = TRUE), n = counter_levels)

    dots <- list(...)
    dots$xlab = names(x$x)[1]
    dots$ylab = names(x$x)[2]
    dots$main <- "Contour plot of estimated production frontier"
    if (is.null(dots$color.palette)) {
        dots$color.palette <- function(n) gray.colors(n, start = 1, end = 0)
    }

    do.call(filled.contour, c(list(
        x = interp_grid,
        levels = breaks,
        plot.axes = quote({
            axis(1); axis(2); grid()
            contour(interp_grid, add = TRUE, levels = breaks,
                    labcex = 0.9, vfont = c("sans serif", "bold"))
            points(x1, x2, pch = 19, col = 1)
        })
    ), dots))
}


#' Plot the Matsuoka Density for a Fitted Model
#'
#' Produces a base R plot of the estimated Matsuoka probability density
#' function (PDF) for a given fitted model object. The density is evaluated on
#' a fine grid over the support \eqn{(0, 1)} and drawn as a smooth curve.
#'
#' @details
#' The function extracts the estimated parameter \eqn{p} from the fitted object
#' (using \code{obj$p_hat}) and evaluates the density via
#' \link{dmatsuoka} on a grid
#' \code{x.seq = seq(1e-10, 1 - 1e-10, length.out = 10000)}.
#' A simple base R plot is produced, including axis labels, a main title, and
#' optional grid lines.
#'
#' @param obj A fitted object of class \code{matsuoka3step}
#'   containing a numeric scalar \code{p_hat}, representing the estimated
#'   shape parameter \eqn{p} of the Matsuoka distribution.
#'
#' @seealso
#' \link{dmatsuoka} for the density function,
#'
#' @export
den.plot <- function(obj) {
    x.seq <- seq(1e-10, 1 - 1e-10, length.out = 10000)
    p <- obj$p_hat

    den <- dmatsuoka(x.seq, p)

    plot(x.seq, den, type = "n",
         xlab = "x", ylab = "f(x)",
         main = sprintf("Matsuoka density (p = %.3f)", p),
         xlim = c(0, 1), ylim = c(0, max(den) * 1.1))

    lines(x.seq, den, col = "black", lwd = 1.5)
    grid()
}
