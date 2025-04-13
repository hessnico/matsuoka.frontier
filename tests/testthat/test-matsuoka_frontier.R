library(testthat)

check_columns_numeric <- function(x) {
  for (col in colnames(x)) {
    expect_true(is.numeric(x[[col]]))
  }
}
  
test_that("Successful creation with valid inputs", {
  test_that("successful: one variable", {
    x <- data.frame(a = c(1, 2, 3))
    y <- c(10, 12, 15)
    
    obj <- MatsuokaFrontier(x = x, y = y)
    
    check_columns_numeric(obj$x)
    expect_true(is.numeric(y))
    expect_s3_class(obj, "MatsuokaFrontier")
    expect_equal(ncol(obj$x), 1)
  })
  
  test_that("successful: two variables", {
    x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
    y <- c(10, 12, 15)
    
    obj <- MatsuokaFrontier(x = x, y = y)
    
    check_columns_numeric(obj$x)
    expect_true(is.numeric(y))
    expect_s3_class(obj, "MatsuokaFrontier")
    expect_equal(ncol(obj$x), 2)
  })
  
  test_that("error: y is not numeric", {
    x <- data.frame(a = c(1, 2, 3))
    y <- c("a", "b", "c")  # Not numeric
    
    expect_error(
      MatsuokaFrontier(x = x, y = y),
      regexp = "y must be numeric"
    )
  })
  
  test_that("successful: x is a matrix", {
    x <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
    y <- c(10, 12, 15)
    
    obj <- MatsuokaFrontier(x = x, y = y)
    
    check_columns_numeric(obj$x)
    expect_true(is.numeric(y))
    expect_s3_class(obj, "MatsuokaFrontier")
    expect_equal(ncol(obj$x), 2)
  })
})

test_that("Input validation errors", {
  test_that("error: dimension mismatch", {
    x <- data.frame(a = 1:3)
    y <- c(10, 12)  # Length 2 vs 3 rows
    
    expect_error(
      MatsuokaFrontier(x, y),
      "Dimension mismatch: y has length 2 but x has 3 rows"
    )
  })
  
  test_that("error: non-numeric columns in x", {
    x <- data.frame(a = 1:3, b = c("a", "b", "c"))
    y <- 1:3
    
    expect_error(
      MatsuokaFrontier(x, y),
      "All columns of x must be numeric"
    )
  })
  
  test_that("error: missing values in x", {
    x <- data.frame(a = c(1, NA, 3))
    y <- 1:3
    
    expect_error(
      MatsuokaFrontier(x, y),
      "x contains missing values"
    )
  })
  
  test_that("error: missing values in y", {
    x <- data.frame(a = 1:3)
    y <- c(10, NA, 15)
    
    expect_error(
      MatsuokaFrontier(x, y),
      "y contains missing values"
    )
  })
  
  test_that("error: invalid y dimensions", {
    x <- data.frame(a = 1:3)
    y <- matrix(1:6, ncol = 2)  # Multi-column matrix
    
    expect_error(
      MatsuokaFrontier(x, y),
      "y must be a vector or single-column matrix"
    )
  })
  
  test_that("error: non-numeric y", {
    x <- data.frame(a = 1:3)
    y <- c("a", "b", "c")
    
    expect_error(
      MatsuokaFrontier(x, y),
      "y must be numeric"
    )
  })
})

test_that("Edge cases", {
  test_that("handles single observation", {
    x <- data.frame(a = 1)
    y <- 10
    
    obj <- MatsuokaFrontier(x, y)
    
    expect_s3_class(obj, "MatsuokaFrontier")
    expect_equal(nrow(obj$x), 1)
  })
  
  test_that("handles zero-row data", {
    x <- data.frame(a = numeric(0))
    y <- numeric(0)
    
    expect_error(
      MatsuokaFrontier(x, y),
      "Cannot create frontier with empty data"
      )
  })
})