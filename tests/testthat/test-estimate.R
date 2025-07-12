library(testthat)

test_that("estimate runs with spline g_strategy", {
    set.seed(42)
    X <- data.frame(x = runif(30))
    y <- exp(-runif(30, 0.5, 1.5))
    
    custom.g <- function(X, z, ...) {
        list(estimate = rep(mean(z), nrow(X)))
    }
    
    strategys <- c("spline", custom.g)
    
    for (strategy in strategys) {
        res <- estimate(X, y, g_strategy = strategy)
        
        expect_type(res, "list")
        expect_named(res, c("g_hat", "residuals", "p_hat", "f_hat"))
        expect_length(res$g_hat$estimate, nrow(X))
        expect_length(res$residuals, nrow(X))
        expect_true(is.numeric(res$p_hat))
        expect_length(res$f_hat, nrow(X))
    }
    
})

test_that("estimate errors if X is not data.frame", {
    X <- matrix(runif(10), ncol = 1)
    y <- exp(-runif(10, 0.5, 1.5))
    
    expect_error(estimate(X, y, g_strategy = "spline"))
})
