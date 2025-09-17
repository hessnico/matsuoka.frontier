library(testthat)

# Sample data
X <- data.frame(x = 1:10)
z <- rnorm(10)

test_that("Spline strategy works correctly", {
    res <- .spline(X, z, lambda = 1)
    
    expect_type(res, "list")
    expect_true(is.numeric(res$estimate))
    expect_equal(length(res$estimate), nrow(X))
    expect_true(!is.null(res$model))
    expect_true("method" %in% names(res$meta))
    expect_equal(res$meta$method, "smooth.spline")
    expect_true("call" %in% names(res$meta))
})

test_that("gam strategy works correctly", {
    res <- .gam(X, z, method = "ML", optimizer = "efs")
    
    expect_type(res, "list")
    expect_true(is.numeric(res$estimate))
    expect_equal(length(res$estimate), nrow(X))
    expect_true(!is.null(res$model))
    expect_true("method" %in% names(res$meta))
    expect_equal(res$meta$method, "gam")
    expect_true("call" %in% names(res$meta))
})

test_that("locpoly works correctly (univariate)", {
    res <- .locpoly(X, z, bandwidth = 2)
    
    expect_type(res, "list")
    expect_true(all(c("estimate", "model", "meta") %in% names(res)))
    expect_true(is.numeric(res$estimate))
    expect_equal(length(res$estimate), nrow(X))
    expect_equal(res$meta$method, "locpoly")
    expect_true("call" %in% names(res$meta))
})

test_that("scar works correctly (multivariate)", {
    res <- .scar(x = X, z = z, shape = data.frame("l"))
    
    expect_type(res, "list")
    expect_true(all(c("estimate", "model", "meta") %in% names(res)))
    expect_true(is.numeric(res$estimate))
    expect_equal(length(res$estimate), nrow(X))
    expect_equal(res$meta$method, "scar")
    expect_true("call" %in% names(res$meta))
})
