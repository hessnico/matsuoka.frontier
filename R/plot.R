#' Plot method for matsuoka3step objects
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics filled.contour
#'
#' @param x An object of class `matsuoka3step`.
#' @param ngrid Number of grid points per axis.
#' @param counter_levels levels of contour from the plot.
#' @param ... Extra graphical arguments passed to filled.contour.
#'
#' @export
plot.matsuoka3step <- function(x, ngrid = 500, counter_levels = 8, ...) {
    if (ncol(x$x) != 2) {
        stop("Contour plot only implemented for 2D input (two covariates).")
    }
    
    x1 <- x$x[,1]
    x2 <- x$x[,2]
    z  <- as.numeric(x$f_hat)
    
    interp_grid <- akima::interp(
        x = x1, y = x2, z = z,
        nx = ngrid, ny = ngrid,
        linear = TRUE, extrap = FALSE
    )
    
    breaks <- pretty(range(interp_grid$z, na.rm = TRUE), n = counter_levels)
    colors <- gray.colors(length(breaks) - 1, start = 1, end = 0)
    
    dots <- list(...)
    if (is.null(dots$main)) dots$main <- "Contour of Estimated Production Frontier"
    if (is.null(dots$xlab)) dots$xlab <- "X1"
    if (is.null(dots$ylab)) dots$ylab <- "X2"    
    if (is.null(dots$color.palette)) dots$color.palette <- function(n) gray.colors(n, start = 1, end = 0)
    
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