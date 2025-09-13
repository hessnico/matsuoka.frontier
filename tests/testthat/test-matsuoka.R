library(testthat)

################# DENSITY #################

test_that("success", {
    x <- seq(1e-10, 1-1e-10, length.out = 10000)
    p <- 8
    
    expect_no_error(d.matsuoka(x, p))
})

test_that("density is a proper pdf", {
    p <- 8.5

    int <- integrate(d.matsuoka, lower = 0, upper = 1, p = p)
    expect_equal(int$value, 1)
}) 

test_that("density should not receive p less than 0", {
    p <- -1
    expect_error(d.matsuoka(0.5, p), "p must be positive")
})

test_that("is pdf for different values of p", {
    x <- seq(1e-10, 1-1e-10, length.out = 100000)
    ps <- seq(1, 31, 2)
    
    for (p in ps) {
        int <- integrate(d.matsuoka, lower = 0, upper = 1, p = p, rel.tol = 1e-10)
        expect_equal(int$value, 1, info = sprintf("failed at %s", p))
    }
})

############# DISTRIBUTION #############

test_that("argument validation: x and p must be single non-NA numeric values", {
    expect_error(c.matsuoka("a", 1), regexp = "`x` must")
    expect_error(c.matsuoka(c(0.1, 0.2), 1), regexp = "`x` must")
    expect_error(c.matsuoka(0.1, "p"), regexp = "`p` must")
    expect_error(c.matsuoka(0.1, NA_real_), regexp = "`p` must")
    expect_error(c.matsuoka(0.1, c(1,2)), regexp = "`p` must")
})

test_that("p must be positive", {
    expect_error(c.matsuoka(0.5, 0), regexp = "must be positive")
    expect_error(c.matsuoka(0.5, -1), regexp = "must be positive")
})

test_that("boundary behaviour: x <= 0 -> 0, x >= 1 -> 1", {
    expect_equal(c.matsuoka(-1, 2), 0)
    expect_equal(c.matsuoka(0, 2), 0)
    expect_equal(c.matsuoka(1, 2), 1)
    expect_equal(c.matsuoka(10, 2), 1)
})

test_that("internal formula matches pgamma-based computation for several points following Matsuoka's article", {
    a <- 3/2
    p_vals <- c(0.5, 1, 2, 5)
    x_vals  <- c(0.001, 0.05, 0.2, 0.5, 0.9)
    for (p in p_vals) {
        for (x in x_vals) {
            t <- -p * log(x)
            expected <- (2 / sqrt(pi)) * gamma(a) * stats::pgamma(t, shape = a, lower.tail = FALSE)
            got <- c.matsuoka(x, p)
            expect_equal(got, expected, tolerance = 1e-10,
                         info = sprintf("p=%g, x=%g", p, x))
        }
    }
})

test_that("values are always in [0,1] for a variety of inputs", {
    p_set <- c(0.5, 1, 2, 10)
    x_set <- c(1e-6, 1e-4, 0.01, 0.1, 0.3, 0.7, 0.95, 0.999)
    for (p in p_set) {
        for (x in x_set) {
            v <- c.matsuoka(x, p)
            expect_true(is.finite(v))
            expect_true(v >= 0 - 1e-12 && v <= 1 + 1e-12,
                        info = sprintf("out of range p=%g x=%g value=%g", p, x, v))
        }
    }
})

test_that("monotonicity in x on (0,1): F(x) non-decreasing", {
    p <- 2
    xs <- c(1e-6, 1e-4, 0.01, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 0.95, 0.999)
    vals <- vapply(xs, function(xx) c.matsuoka(xx, p), numeric(1))
    expect_true(all(diff(vals) >= -1e-12), info = "F_p should be non-decreasing in x on (0,1)")
})

test_that("limit behaviour near 0 and 1", {
    p <- 3
    near_zero <- c.matsuoka(1e-12, p)
    near_one  <- c.matsuoka(1 - 1e-12, p)
    expect_true(near_zero >= 0 && near_zero < 1e-6)
    expect_true(near_one <= 1 && (1 - near_one) < 1e-6)
})

test_that("numeric stability / no unexpected warnings for typical inputs", {
    expect_silent(c.matsuoka(0.5, 1))
    expect_silent(c.matsuoka(0.2, 10))
})
