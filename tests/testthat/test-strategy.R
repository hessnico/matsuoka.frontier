library(testthat)

set.seed(666)

X_multi <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
z <- rnorm(100)

strategies <- list(
    list(name = "spline", fun = .spline, X = X_multi["x1"], args = list(lambda = 1)),
    list(name = "gam", fun = .gam,  X = X_multi, args = list(method = "ML", optimizer = "efs")),
    list(name = "locpoly", fun = .locpoly, X = X_multi["x1"], args = list(bandwidth = 2)),
    list(name = "scar", fun = .scar, X = X_multi, args = list(shape = data.frame("l", "l"))),
    list(name = "sback", fun = .sback, X = X_multi, args = list()),
    list(name = "backf.cl", fun = .backf.cl, X = X_multi, args = list(windows=c(1, 1))),
    list(name = "backf.rob", fun = .backf.rob, X = X_multi, args = list(windows=c(1, 1)))
)

test_that("All strategies produce valid results", {
    for (s in strategies) {
        args <- c(list(x = s$X, z = z), s$args)
        res <- do.call(s$fun, args)
        
        expect_type(res, "list")
        expect_true(all(c("estimate", "model", "meta") %in% names(res)))
        expect_true(is.numeric(res$estimate))
        expect_equal(length(res$estimate), nrow(s$X))
        expect_equal(res$meta$method, s$name)
        expect_true("call" %in% names(res$meta))
    }
})
