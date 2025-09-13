#' Plot method for matsuoka3step objects
#'
#' Produces diagnostic plots for a fitted Matsuoka frontier model:
#' (i) the fitted density function based on \code{p_hat},
#' (ii) a filled contour of the estimated production frontier.
#' (iii) the plot for each xy values with productive estimated function
#'
#' @param x An object of class \code{matsuoka3step}.
#' @param which Which plot(s) to show: 
#'   \code{1} = density, 
#'   \code{2} = contour (requires 2D input), 
#'   \code{c(1,2)} or \code{"both"} = both.
#' @param ngrid Number of grid points per axis for interpolation (contour only).
#' @param counter_levels Number of contour levels (contour only).
#' @param ask Logical; if \code{TRUE}, pause between plots.
#' @param ... Additional graphical parameters passed to plotting functions.
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics filled.contour grid lines par
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of plotting.
#' @export
#'
#' @seealso [c.matsuoka()], [contour.plot.helper()], [den.plot()]
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
    
    if (3 %in% which) {
        f.hat.x.y.plot(x, ...)
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

#' Plot each xy values with productive estimated function
#' @keywords internal
#' Plot estimated production functions
#'
#' This function plots the observed output \code{y} against each input variable
#' in \code{res$x}, along with the corresponding estimated frontier
#' \code{res$f_hat}.
#'
#' @param res A fitted object containing at least:
#'   \describe{
#'     \item{x}{A data frame of input variables.}
#'     \item{y}{A numeric vector of observed outputs.}
#'     \item{f_hat}{A numeric vector of fitted/estimated outputs.}
#'   }
#'
#' @details
#' Axis labels can be set with \code{xlab} and \code{ylab}.  
#' Defaults are \code{"x"} and \code{"y"} if not provided.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side
#' effect of producing plots.
#' @export
f.hat.x.y.plot <- function(res, ...) {
    y.plot <- sort(res$y)
    f.hat.plot <- sort(res$f_hat)
    ylim = c(min(y.plot, f.hat.plot)*0.95, max(y.plot, f.hat.plot)*1.05)
    
    dots = list(...)
    ylab = dots$ylab
    
    for (n in names(res$x)) {
        x.plot <- sort(res$x[[n]])
        
        plot(
            x.plot, y.plot,
            ylim = ylim,
            ylab = ylab,
            xlab = n,
            xlim = c(min(x.plot)*0.95, max(x.plot)*1.05),
            main = sprintf("Estimated production frontier plot for %s", n)
        )
        lines(x.plot, f.hat.plot, col = "red", lwd = 1)
    }
    
    invisible(NULL)
}

#' Filled contour plot helper
#' @keywords internal
contour.plot.helper <- function(x, ngrid = 500, counter_levels = 8, ...) {
    x1 <- x$x[, 1]
    x2 <- x$x[, 2]
    
    z  <- as.numeric(x$f_hat)
    
    interp_grid <- akima::interp(
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

#' Plot the Matsuoka density for a fitted object
#'
#' This is an internal helper function that produces a base R plot
#' of the estimated Matsuoka probability density function (PDF)
#' for a given fitted object. The density is evaluated on a fine grid
#' over the support \eqn{(0, 1)} and drawn as a smooth curve.
#'
#' @details
#' The function extracts the estimated \eqn{p} parameter from the object
#' (`obj$p_hat`) and calls [matsuoka.density()] to evaluate the density
#' on `x.seq = seq(1e-10, 1 - 1e-10, length.out = 10000)`.  
#' A simple plot with axis labels, main title, and grid lines is generated.
#'
#' @param obj A fitted object of class \code{matsuoka3step} (or a similar class)
#'   that contains a numeric scalar \code{p_hat} element, representing the
#'   estimated \eqn{p} parameter of the distribution.
#' @keywords internal
den.plot <- function(obj) {
    x.seq <- seq(1e-10, 1 - 1e-10, length.out = 10000)
    p <- obj$p_hat
    
    den <- d.matsuoka(x.seq, p)
    
    plot(x.seq, den, type = "n",
         xlab = "x", ylab = "f(x)",
         main = sprintf("Matsuoka density (p = %.3f)", p),
         xlim = c(0, 1), ylim = c(0, max(den) * 1.1))
    
    lines(x.seq, den, col = "black", lwd = 1.5)
    grid()
}
