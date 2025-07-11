#' Validate that y is numeric and strictly positive
#'
#' @param y Numeric vector
#'
#' @return Stops with error if invalid
#' @keywords internal
validate.y <- function(y) {
    if (!is.numeric(y)) stop("y must be numeric.")
    if (any(y <= 0)) stop("All values in y must be strictly positive (required for log).")
}

#' Validate that X is a data.frame
#'
#' @param X An input object
#'
#' @return Stops with error if invalid
#' @keywords internal
validate.X <- function(X) {
    if (!is.data.frame(X)) stop("X must be a data.frame.")
    if (nrow(X) == 0 || ncol(X) == 0) stop("X must have at least one row and one column.")
}