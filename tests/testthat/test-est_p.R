library(testthat)

test_that("estimate_p returns correct numeric value", {
    residuals <- c(0.1, -0.1, 0.05, -0.05)
    p <- estimate_p(residuals)
    expect_true(is.numeric(p))
    expect_gt(p, 0)
})

test_that("estimate_p checks input validity", {
    expect_error(estimate_p(character(0)))
    expect_error(estimate_p(numeric(0)))
})