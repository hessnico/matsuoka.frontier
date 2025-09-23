# Unit tests for summary.matsuoka3step function
library(testthat)

# Helper function to create mock matsuoka3step objects
create_mock_matsuoka3step <- function(efficiency = c(0.8, 1.2, 0.9, 1.5, 0.7),
                                      p_hat = 0.5,
                                      call = NULL,
                                      est.call = NULL,
                                      g_hat = NULL,
                                      g_hat.model.call = NULL) {
    obj <- list(
        efficiency = efficiency,
        p_hat = p_hat,
        call = call,
        est.call = est.call,
        g_hat = g_hat,
        g_hat.model.call = g_hat.model.call
    )
    class(obj) <- "matsuoka3step"
    return(obj)
}

test_that("summary.matsuoka3step handles valid input correctly", {
    obj <- create_mock_matsuoka3step()
    
    expect_no_error(result <- summary(obj))
    expect_type(result, "list")
    expect_named(result, c("efficiency", "efficiency_summary", "n", "n_efficient", 
                           "pct_efficient", "model_call", "gcall"))
})

test_that("summary.matsuoka3step calculates efficiency statistics correctly", {
    efficiency_vals <- c(0.5, 1.0, 1.5, 2.0, 0.8)
    obj <- create_mock_matsuoka3step(efficiency = efficiency_vals)
    
    result <- summary(obj)
    
    expect_equal(result$efficiency, efficiency_vals)
    expect_equal(result$n, 5)
    expect_equal(result$n_efficient, 3)
    expect_equal(result$pct_efficient, 60)
})

test_that("summary.matsuoka3step handles NA values in efficiency", {
    efficiency_with_na <- c(0.5, 1.0, NA, 1.5, 0.8)
    obj <- create_mock_matsuoka3step(efficiency = efficiency_with_na)
    
    result <- summary(obj)
    
    expect_equal(result$n, 5)
    expect_equal(result$n_efficient, 2)
    expect_equal(result$pct_efficient, 40)
})

test_that("summary.matsuoka3step handles model call extraction", {
    model_call <- quote(some_model(y ~ x, data = df))
    obj <- create_mock_matsuoka3step(call = model_call)
    
    result <- summary(obj)
    expect_equal(result$model_call, "some_model(y ~ x, data = df)")
    
    est_call <- quote(estimate_function(data))
    obj2 <- create_mock_matsuoka3step(call = NULL, est.call = est_call)
    
    result2 <- summary(obj2)
    expect_equal(result2$model_call, "estimate_function(data)")
})

test_that("summary.matsuoka3step handles g_hat call extraction", {
    g_call <- quote(smooth.spline(x, y))
    obj <- create_mock_matsuoka3step(g_hat.model.call = g_call)
    
    result <- summary(obj)
    expect_equal(result$gcall, "smooth.spline(x, y)")
    
    nested_g_hat <- list(
        model = list(call = quote(loess(y ~ x)))
    )
    obj2 <- create_mock_matsuoka3step(g_hat = nested_g_hat)
    
    result2 <- summary(obj2)
    expect_equal(result2$gcall, "loess(y ~ x)")
    
    g_hat_with_call <- list(call = quote(gam(y ~ s(x))))
    obj3 <- create_mock_matsuoka3step(g_hat = g_hat_with_call)
    
    result3 <- summary(obj3)
    expect_equal(result3$gcall, "gam(y ~ s(x))")
})

test_that("summary.matsuoka3step handles NULL calls gracefully", {
    obj <- create_mock_matsuoka3step(
        call = NULL,
        est.call = NULL,
        g_hat = NULL,
        g_hat.model.call = NULL
    )
    
    result <- summary(obj)
    expect_null(result$model_call)
    expect_null(result$gcall)
    
    expect_no_error(summary(obj))
})

test_that("summary.matsuoka3step handles edge cases", {
    obj_all_efficient <- create_mock_matsuoka3step(efficiency = c(1.0, 1.5, 2.0))
    result_all <- summary(obj_all_efficient)
    expect_equal(result_all$pct_efficient, 100)
    
    obj_none_efficient <- create_mock_matsuoka3step(efficiency = c(0.1, 0.5, 0.9))
    result_none <- summary(obj_none_efficient)
    expect_equal(result_none$pct_efficient, 0)
    
    obj_single <- create_mock_matsuoka3step(efficiency = 1.5)
    result_single <- summary(obj_single)
    expect_equal(result_single$n, 1)
    expect_equal(result_single$n_efficient, 1)
    expect_equal(result_single$pct_efficient, 100)
})
