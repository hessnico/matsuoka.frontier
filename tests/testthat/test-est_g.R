library(testthat)
library(Benchmarking)

test_that("spline strategy", {
    set.seed(123)
    X <- data.frame(x = runif(50))
    z <- log(runif(50, 1, 5))
    
    res <- estimate.g(X, z, strategy = "spline")
    
    expect_true(is.list(res))
    expect_true("estimate" %in% names(res))
    expect_equal(length(res$estimate), nrow(X))
    expect_true(is.numeric(res$estimate))
})

test_that("gam strategy", {
    data(milkProd, package = "Benchmarking")
    
    N <- nrow(milkProd)
    nc <- milkProd$cows
    X <- data.frame(
        vet_per_cow = milkProd$vet / nc,
        energy_per_cow = milkProd$energy / nc
    )
    y <- milkProd$milk / nc / 1e3
    z <- -log(y)
    
    # Run estimate.g with gam
    res <- estimate.g(X, z, strategy = "gam", method = "REML")
    
    expect_type(res, "list")
    expect_true(!is.null(res$estimate))
    expect_equal(length(res$estimate), N)
    expect_false(any(is.na(res$estimate)))
    
    residuals <- z - res$estimate
    expect_lt(sd(residuals), 1)  # variance should be somewhat controlled
})

test_that("estimate.g errors if X is not a data.frame", {
    X_mat <- matrix(runif(50), ncol = 1)
    z <- log(runif(50, 1, 5))
    
    expect_error(estimate.g(X_mat, z, strategy = "spline"))
})

test_that("estimate.g errors if X is not a data.frame", {
    X_mat <- matrix(runif(50), ncol = 1)
    z <- log(runif(50, 1, 5))
    
    expect_error(estimate.g(X_mat, z, strategy = "spline"))
})

test_that("estimate.g errors if spline input is multivariate", {
    X <- data.frame(x1 = runif(10), x2 = runif(10))
    z <- log(runif(10, 1, 5))
    
    expect_error(estimate.g(X, z, strategy = "spline"))
})

test_that("estimate.g works with custom function", {
    X <- data.frame(x = runif(20))
    z <- log(runif(20, 1, 5))
    
    custom_fun <- function(X, z, ...) {
        list(estimate = rep(mean(z), nrow(X)))
    }
    
    res <- estimate.g(X, z, strategy = custom_fun)
    
    expect_true(is.list(res))
    expect_equal(length(res$estimate), nrow(X))
})

test_that("estimate.g errors if custom function returns invalid result", {
    X <- data.frame(x = runif(10))
    z <- log(runif(10, 1, 5))
    
    bad_fun <- function(X, z, ...) NULL
    
    expect_error(estimate.g(X, z, strategy = bad_fun))
})

test_that("estimate.g errors with unknown strategy", {
    X <- data.frame(x = runif(10))
    z <- log(runif(10, 1, 5))
    
    expect_error(estimate.g(X, z, strategy = "not-valid-one"))
})