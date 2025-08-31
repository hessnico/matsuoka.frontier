library(testthat)
library(Benchmarking)

test_that("matsuoka3step runs and returns expected structure on milkProd data", {
    # Assuming milkProd is already loaded
    N <- nrow(milkProd)
    nc <- milkProd$cows
    
    X <- data.frame(
        vet_per_cow = milkProd$vet / nc,
        energy_per_cow = milkProd$energy / nc
    )
    y <- milkProd$milk / nc / 1e3
    
    expect_true(all(y > 0))  # y must be positive
    
    fit <- matsuoka3step(x = X, y = y, g = "gam", method = "REML")
    
    expect_s3_class(fit, "matsuoka3step")
    expect_equal(nrow(fit$x), length(fit$y))
    expect_length(fit$g_hat_res, nrow(fit$x))
    expect_length(fit$f_hat, nrow(fit$x))
    expect_true(is.numeric(fit$p_hat))
    expect_true(fit$p_hat > 0)
    
    expect_lt(abs(mean(fit$g_hat_res)), 1e-1)
})

test_that("matsuoka3step returns valid object with spline", {
    set.seed(123)
    X <- data.frame(x = runif(50))
    y <- exp(-runif(50, 0.5, 1.5))
    
    res <- matsuoka3step(x = X, y = y, g = "spline")
    
    expect_s3_class(res, "matsuoka3step")
    expect_type(res, "list")
    
    expect_equal(nrow(res$x), length(res$y))
    expect_equal(length(res$g_hat$estimate), nrow(res$x))
    expect_equal(length(res$g_hat_res), nrow(res$x))
    expect_true(is.numeric(res$p_hat))
    expect_equal(length(res$f_hat), nrow(res$x))
})

test_that("matsuoka3step works with custom g function", {
    set.seed(1)
    X <- data.frame(x = runif(20))
    y <- exp(-runif(20, 0.5, 1.5))
    
    dummy_g <- function(X, z, ...) {
        list(estimate = rep(mean(z), nrow(X)))
    }
    
    res <- matsuoka3step(x = X, y = y, g = dummy_g)
    
    expect_s3_class(res, "matsuoka3step")
    expect_equal(length(res$f_hat), nrow(X))
    expect_true(is.numeric(res$p_hat))
})

test_that("matsuoka3step errors on non-positive y", {
    X <- data.frame(x = runif(10))
    y <- c(1, 2, 0, 4, 5)
    
    expect_error(matsuoka3step(x = X, y = y, g = "spline"), "strictly positive")
})

test_that("matsuoka3step errors if x is not a data.frame", {
    x_mat <- matrix(runif(10), ncol = 1)
    y <- exp(-runif(10, 0.5, 1.5))
    
    expect_error(matsuoka3step(x = x_mat, y = y, g = "spline"), "must be a data.frame")
})
