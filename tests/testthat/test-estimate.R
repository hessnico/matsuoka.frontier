library(testthat)

test_that("estimate runs with spline g_strategy", {
    set.seed(42)
    X <- data.frame(x = runif(30))
    y <- exp(-runif(30, 0.5, 1.5))  # positive values for y
    
    res <- estimate(X, y, g_strategy = "spline")
    
    expect_type(res, "list")
    expect_named(res, c("g_hat", "residuals", "p_hat", "f_hat"))
    expect_length(res$g_hat$estimate, nrow(X))
    expect_length(res$residuals, nrow(X))
    expect_true(is.numeric(res$p_hat))
    expect_length(res$f_hat, nrow(X))
})

test_that("estimate runs with custom g_strategy function", {
    set.seed(123)
    X <- data.frame(x = runif(20))
    y <- exp(-runif(20, 0.5, 1.5))
    
    custom_g <- function(X, z, ...) {
        list(estimate = rep(mean(z), nrow(X)))
    }
    
    res <- estimate(X, y, g_strategy = custom_g)
    
    expect_type(res, "list")
    expect_named(res, c("g_hat", "residuals", "p_hat", "f_hat"))
    expect_length(res$g_hat$estimate, nrow(X))
    expect_length(res$residuals, nrow(X))
    expect_true(is.numeric(res$p_hat))
    expect_length(res$f_hat, nrow(X))
})

test_that("estimate errors if y contains non-positive values", {
    X <- data.frame(x = runif(10))
    y <- c(1, 2, 0, 4, 5)  # zero is invalid for log
    
    expect_error(estimate(X, y, g_strategy = "spline"))
})

test_that("estimate errors if X is not data.frame", {
    X <- matrix(runif(10), ncol = 1)
    y <- exp(-runif(10, 0.5, 1.5))
    
    expect_error(estimate(X, y, g_strategy = "spline"))
})
