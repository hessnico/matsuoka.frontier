library(testthat)
library(grDevices)

test_that("plot.matsuoka3step produces and displays a plot", {
    set.seed(666)
    x_data <- matrix(runif(20), ncol = 2)
    f_hat <- runif(10)
    obj <- structure(list(x = x_data, f_hat = f_hat), class = "matsuoka3step")
    
    plot(obj, ngrid = 10, counter_levels = 3,
         xlab = "dddummy", ylab = "dummy", main = "test it")
    
    p <- recordPlot()
    replayPlot(p)
    
    expect_true(inherits(p, "recordedplot"))
})


test_that("plot.matsuoka3step fails for non-2D input", {
    # Example invalid 1D data
    x_data <- matrix(runif(10), ncol = 1)
    f_hat <- runif(10)
    
    obj <- structure(list(x = x_data, f_hat = f_hat), class = "matsuoka3step")
    
    # Expect an error
    expect_error(
        recordPlot({ plot(obj) }),
        "Contour plot only implemented for 2D input"
    )
})
