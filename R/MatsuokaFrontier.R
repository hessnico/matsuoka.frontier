#' MatsuokaFrontier Class
#'
#' This class estimates the production frontier based on a three step approach with the provided inputs.
#' The efficiency is estimated as a Matsuoka distribuition.
#' It can handle both univariate and multivariate input data (`x`).
#'
#' @param x A numeric data frame or matrix representing the independent variables.
#' @param y A numeric vector representing the dependent variable.
#'
#' @return An object of class `MatsuokaFrontier` containing:
#'   \describe{
#'     \item{x}{The input data frame of independent variables.}
#'     \item{y}{The dependent variable vector.}
#'     \item{est_p}{The estimated parameter `p`.}
#'     \item{est_g}{The estimated function `g(x)`.}
#'   }
#' @export
MatsuokaFrontier <- function(x, y) {
  validated <- validate_inputs(x, y)
  
  return(
    structure(
      list(x = validated$x,
           y = validated$y,
           est_p = NULL,
           est_g = NULL),
      class = "MatsuokaFrontier")
    )
}

#' validate inputs
#' @noRd
validate_inputs <- function(x, y) {
  if (nrow(x) == 0 || length(y) == 0) {
    stop("Cannot create frontier with empty data")
  }
  
  if (anyNA(x))
    stop("x contains missing values")
  if (anyNA(y))
    stop("y contains missing values")
  
  if (!is.numeric(y))
    stop("y must be numeric")
  
  x <- as.data.frame(x)
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of x must be numeric")
  }
  
  if (is.matrix(y) || is.data.frame(y)) {
    if (ncol(y) != 1) {
      stop("y must be a vector or single-column matrix")
    }
    y <- as.numeric(y[, 1])
  }
  
  if (length(y) != nrow(x)) {
    stop(sprintf("Dimension mismatch: y has length %d but x has %d rows",
                 length(y), nrow(x)))
  }
  
  list(x = x, y = y)
}