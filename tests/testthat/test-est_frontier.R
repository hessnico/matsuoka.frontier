library(testthat)

test_that("estimate_frontier returns correct values", {
    g_hat <- c(0, 0.5, 1)
    p_hat <- 2
    
    expected <- exp((3 / (2 * p_hat)) - g_hat)
    result <-estimate_frontier(g_hat, p_hat)
    
    expect_equal(result, expected)
})

test_that("estimate_frontier checks input validity", {
    expect_error(estimate_frontier("not numeric", 2))
    expect_error(estimate_frontier(c(1, 2), -1))
    expect_error(estimate_frontier(numeric(0), 2))
})

test_that("estimate frontier when I know the result", {
    g_hat <- c(0.0, 1)
    p_hat <- 1
    
    expect_equal(round(estimate_frontier(g_hat, p_hat),3), round(c(4.481689, 1.648721),3))
})