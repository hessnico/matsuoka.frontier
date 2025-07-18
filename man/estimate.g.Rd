% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/est_g.R
\name{estimate.g}
\alias{estimate.g}
\title{Estimate g(x) using specified strategy}
\usage{
estimate.g(X, z, strategy = "spline", ...)
}
\arguments{
\item{X}{A data.frame of input variables}

\item{z}{Numeric vector (-log(y))}

\item{strategy}{Character string specifying the strategy ("spline") or a custom function
for estimating function g}

\item{...}{Additional parameters passed to the spline function or custom function}
}
\value{
List with element \code{estimate} (numeric vector).
}
\description{
Estimate g(x) using specified strategy
}
