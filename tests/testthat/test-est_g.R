library(testthat)
library(Benchmarking)

test_that("estimate.g works for multiple strategies with proper data setup", {
    data(milkProd, package = "Benchmarking")
    
    N <- nrow(milkProd)
    nc <- milkProd$cows
    X_full <- data.frame(
        vet_per_cow = milkProd$vet / nc,
        energy_per_cow = milkProd$energy / nc
    )
    y <- milkProd$milk / nc / 1e3
    z <- -log(y)
    
    strategies <- list(
        list(name = "spline", X = X_full[1], args = list()),
        list(name = "gam",    X = X_full, args = list(method = "REML")),
        list(name = "locpoly", X = X_full[1], args = list(bandwidth = 1)),
        list(name = "scar", X = X_full, args = list(shape = data.frame("l", "l")))
    )
    
    for (s in strategies) {
        res <- do.call(
            estimate.g,
            c(list(X = s$X, z = z, strategy = s$name), s$args)
        )
        
        expect_type(res, "list")
        expect_true(!is.null(res$estimate))
        expect_equal(length(res$estimate), N)
        expect_false(any(is.na(res$estimate)))
        expect_contains(res$meta$method, s$name)
    }
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