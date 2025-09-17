.onLoad <- function(libname, pkgname) {
    register_strategy("spline", spline)
    register_strategy("gam", gam)
}