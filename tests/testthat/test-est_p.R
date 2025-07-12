library(testthat)

test_that("estimate.p returns correct numeric value", {
    residuals <- c(0.1, -0.1, 0.05, -0.05)
    p <- estimate.p(residuals)
    expect_true(is.numeric(p))
    expect_gt(p, 0)
})

test_that("estimate.p checks input validity", {
    expect_error(estimate.p(character(0)))
    expect_error(estimate.p(numeric(0)))
})