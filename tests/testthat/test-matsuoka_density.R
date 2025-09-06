library(testthat)

test_that("estimate.p returns correct numeric value", {
    residuals <- c(0.1, -0.1, 0.05, -0.05)
    p <- estimate.p(residuals)
    expect_true(is.numeric(p))
    expect_gt(p, 0)
})

test_that("success", {
    x <- seq(1e-10, 1-1e-10, length.out = 10000)
    p <- 8
    
    expect_no_error(matsuoka.density(x, p))
})

test_that("density is a proper pdf", {
    p <- 8.5

    int <- integrate(matsuoka.density, lower = 0, upper = 1, p = p)
    expect_equal(int$value, 1)
}) 

test_that("density should not receive p less than 0", {
    p <- -1
    expect_error(matsuoka.density(0.5, p), "p must be positive")
})

test_that("is pdf for different values of p", {
    x <- seq(1e-10, 1-1e-10, length.out = 100000)
    ps <- seq(1, 31, 2)
    
    for (p in ps) {
        int <- integrate(matsuoka.density, lower = 0, upper = 1, p = p, rel.tol = 1e-10)
        expect_equal(int$value, 1, info = sprintf("failed at %s", p))
    }
})