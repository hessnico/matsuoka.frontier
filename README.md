# matsuoka.frontier

![R-CMD-check](https://github.com/hessnico/matsuoka.frontier/actions/workflows/r-cmd-check.yaml/badge.svg)

An R package for estimating production frontiers using the **three-step approach** proposed by this paper:
 
> [A three-step approach to production frontier estimation and the Matsuoka's distribution](https://arxiv.org/abs/2311.06086). arXiv:2311.06086.  
> Matsuoka, D. H., Pumi, G., Torrent, H. da S., Valk, M. (2024).

---

## Features

- Estimate the non-parametric regression function `g(x)` using pre-defined function or custom ones
- Compute the Matsuoka parameter `p` via method of moments
- Construct the production frontier `f(x)` via plug-in formula
- Includes `matsuoka3step()` class for a clean, object-oriented interface
- Includes built-in S3 methods for `plot` and `summary` for diagnose
- Designed for both univariate and multivariate inputs

---

## Installation

```r
# Install from GitHub
remotes::install_github("hessnico/matsuoka.frontier")
```

## Usage 

For detailed examples and explanations, please see the package vignette:

[**Matsuoka Vignette**](vignettes/MatsuokaDistribution.Rmd)

### Quick Example

```r
# Load the package
library(matsuokafrontier)

x <- ...
y <- ...

# Example using the normal method
result <- matsuoka.frontier::matsuoka3step(x = x, y = y)

summary(result)
plot(result)
```

## About

This package was developed as part of the final project for the Bachelor's degree in Statistics at UFRGS (Universidade Federal do Rio Grande do Sul).

It aims to provide a practical and flexible implementation of the three-step production frontier estimation method.


## Some enhancements to do 
- [X] **Add more private methods in `estimate_g`**  
  Expand the internal functionality of the `estimate_g` function to include additional helper methods for improved estimation, as the ones cited in this package article;
  - [X] Local linear smoothing (using `KernSmooth`)
  - [X] Spline regression (using `stats`)
  - [X] Classical backfitting (using `RBF`)
  - [X] Smooth backfitting (using `wsbackfit`)
  - [X] Robust backfitting (using `RBF`)
  - [X] Shape-Constrained Additive Regression (using `scar`)
  
- [X] **Plotting the output of `matsuokafrontier::matsuoka3step`**  
  Visualize the estimated frontiers or densities depending on the data dimension:
  - [X] **1D data**: Plot `x` vs `y` and overlay the estimated frontier line `f_hat`.  
  - [X] **2D data**: Create a contour plot of the estimated frontier.  
  - [X] **Parameter `p`**: Visualize the density of the Matsuoka distribution.

- [X] **Enhance documentation with usage examples**  
  Provide clear examples for users to understand and apply the package functions:
  - [X] Using the **normal method**.  
  - [X] Using a method with **specific arguments**.  
  - [X] Using a **user-defined custom method**.

- [X] **Cramer-Von Mises Test of Goodness-of-Fit test with the Matsuoka distribution**  
  Perform a goodness-of-fit test comparing estimated values `r_hat` to the theoretical `Matsuoka(p_hat)` distribution.
