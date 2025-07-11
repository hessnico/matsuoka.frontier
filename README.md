# matsuoka.frontier

![R-CMD-check](https://github.com/hessnico/matsuoka.frontier/actions/workflows/r-cmd-check.yaml/badge.svg)

An R package for estimating production frontiers using the **three-step approach** proposed by this paper:

> Matsuoka, A. and Azom, S. (2023).  
> [A three-step approach to production frontier estimation and the Matsuoka's distribution](https://arxiv.org/abs/2311.06086). arXiv:2311.06086.

---

## Features

- Estimate the non-parametric regression function `g(x)` using pre-defined function or custom ones
- Compute the Matsuoka parameter `p` via method of moments
- Construct the production frontier `f(x)` via plug-in formula
- Includes `matsuoka3step()` class for a clean, object-oriented interface
- Designed for both univariate and multivariate inputs

---

## Installation

```r
# Install from GitHub
remotes::install_github("hessnico/matsuoka.frontier")
```

## Usage 

```r
library(matsuoka.frontier)

x <- ...
y <- ...

result <- matsuoka.frontier::matsuoka3step(x = x, y = y)
str(result)
```

## About

This package was developed as part of the final project for the Bachelor's degree in Statistics at UFRGS (Universidade Federal do Rio Grande do Sul).
It aims to provide a practical and flexible implementation of the three-step production frontier estimation method.