#' Cramer-von Mises criterion test for the Matsuoka distribution
#'
#' Performs the Cramer-von Mises goodness-of-fit test to check whether
#' a numeric vector `x` follows a Matsuoka distribution.
#'
#' @param x Numeric vector of observations.
#' @param p Numeric scalar, the Matsuoka parameter.
#' @param estimated Logical, whether the parameter `p` is estimated from the data, default is set to TRUE.
#' @param nullname Character, name of the null distribution for reporting.
#' @param ... Additional arguments passed to \code{\link[goftest]{cvm.test}}.
#' @return An object of class `htest`, as returned by `cvm.test()`.
#' @examples
#' # In practice, x_sample would be obtained as y / f_hat after using matsuoka3step.
#' # For reproducibility in examples, we simulate Matsuoka data using `rmv()` function:
#' x_sample <- rmv(100, p = 0.5)
#'
#' # Perform Cramer-von Mises test
#' cvm_matsuoka_test(x_sample, p = 0.5)
#'
#' # Pass additional arguments to cvm.test()
#' cvm_matsuoka_test(x_sample, p = 0.5, estimated = FALSE)
#'
#' @seealso
#' \code{\link[goftest]{cvm.test}} for the underlying test implementation.
#' \code{\link{visual_cvm_matsuoka}} for graphical assessment of the test.
#'
#' @export
#' @importFrom goftest cvm.test
cvm_matsuoka_test <- function(x, p, estimated = TRUE, nullname = "Matsuoka Distribution", ...) {
    if (!is.numeric(x)) stop("`x` must be numeric.")
    if (!is.null(p) && (!is.numeric(p) || length(p) != 1 || p <= 0)) {
        stop("`p` must be a single positive numeric value or NULL.")
    }

    F_p <- function(xi) cmatsuoka(xi, p = p)

    res <- goftest::cvm.test(
        x,
        F_p,
        estimated = estimated,
        nullname = nullname,
        ...
    )

    return(res)
}

#' Visual Cramer-von Mises Goodness-of-Fit Test for the Matsuoka Distribution
#'
#' Produces a diagnostic plot comparing the empirical cumulative
#' distribution function (ECDF) of a numeric sample to the theoretical
#' CDF of the Matsuoka distribution with parameter \eqn{p}.
#' Internally, the function runs \code{cvm_matsuoka_test()}, and the
#' resulting Cramer-von Mises statistic and p-value are shown in the plot subtitle.
#'
#' This plot serves as an intuitive visual check for the adequacy of
#' the Matsuoka distribution as a model for the data.
#'
#' @param x Numeric vector of observations.
#' @param p Positive numeric scalar; the Matsuoka shape parameter.
#' @param ... Additional arguments forwarded to \code{cvm_matsuoka_test()}.
#'
#' @return
#' A \code{ggplot} object displaying:
#' \itemize{
#'   \item the empirical CDF (ECDF),
#'   \item the theoretical Matsuoka CDF,
#'   \item the Cramer-von Mises statistic and p-value in the subtitle.
#' }
#'
#' @details
#' The visual diagnostic highlights discrepancies between the empirical and
#' theoretical distributions. Major deviations along the CDF curve indicate
#' departures from the Matsuoka model for the specified \eqn{p}.
#'
#' @seealso
#'   \code{\link{cvm_matsuoka_test}} for the numerical test,
#'   \code{\link{cmatsuoka}} for the CDF,
#'   \code{\link{F.mv.i}} for the quantile function
#'   \code{vignette("matsuoka.frontier")} - see section
#'   *"Cramer-Von Mises test with the Matsuoka's distribution after estimating Matsuoka parameter"*.
#'
#' @import ggplot2
#' @importFrom stats ecdf
#' @export
visual_cvm_matsuoka <- function(x, p, ...) {
    if (!is.numeric(x)) stop("`x` must be numeric.")
    if (length(x) < 2) stop("`x` must have at least 2 observations.")
    if (!is.numeric(p) || length(p) != 1 || p <= 0) {
        stop("`p` must be a single positive numeric value.")
    }

    CDF <- Distribution <- NULL

    test_res <- cvm_matsuoka_test(x = x, p = p, ...)

    x_sorted <- sort(x)
    df_plot <- data.frame(
        x = x_sorted,
        ecdf = ecdf(x)(x_sorted),
        theoretical = cmatsuoka(x_sorted, p = p)
    )

    df_long <- data.frame(
        x = rep(x_sorted, 2),
        CDF = c(df_plot$ecdf, df_plot$theoretical),
        Distribution = factor(rep(c("Empirical", "Theoretical"), each = length(x_sorted)),
                              levels = c("Empirical", "Theoretical"))
    )

    cvm_statistic <- test_res$statistic

    p_plot <- ggplot2::ggplot(df_long, ggplot2::aes(x = x, y = CDF, color = Distribution)) +
        ggplot2::geom_step(data = subset(df_long, Distribution == "Empirical"),
                           direction = "hv", linewidth = 0.8, alpha = 0.9) +
        ggplot2::geom_line(data = subset(df_long, Distribution == "Theoretical"),
                           linewidth = 0.8, alpha = 0.9) +
        ggplot2::scale_color_manual(values = c("Empirical" = "#D55E00",
                                               "Theoretical" = "#0072B2"),
                                    name = "Distribution") +
        ggplot2::labs(
            x = "x",
            y = "Cumulative Distribution Function",
            title = "Cramer-von Mises Goodness-of-Fit Test",
            subtitle = paste0("CvM statistic = ", signif(cvm_statistic, 6),
                              ", p-value = ", signif(test_res$p.value, 6),
                              "\nMatsuoka parameter p = ", signif(p, 6))
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 12, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 10),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(face = "bold"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(color = "grey80", fill = NA)
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                                    breaks = seq(0, 1, 0.2)) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(
            linetype = c("solid", "solid"),
            linewidth = c(1, 1)
        )))

    return(p_plot)
}
