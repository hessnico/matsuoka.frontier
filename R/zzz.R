.onLoad <- function(libname, pkgname) {
    register_strategy("spline", .spline)
    register_strategy("gam", .gam)
    register_strategy("locpoly", .locpoly)
    register_strategy("scar", .scar)
}