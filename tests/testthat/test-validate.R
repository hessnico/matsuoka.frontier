library(testthat)

test_that("validate_y works as expected", {
    expect_silent(validate.y(c(1, 2, 3)))
    expect_error(validate.y(c(0, 1)), "strictly positive")
    expect_error(validate.y(c(-1, 2)), "strictly positive")
    expect_error(validate.y("text"), "must be numeric")
})

test_that("validate_X works as expected", {
    expect_silent(validate.X(data.frame(x = 1:5)))
    expect_error(validate.X(matrix(1:5)), "must be a data.frame")
    expect_error(validate.X(data.frame()), "must have at least one")
})
