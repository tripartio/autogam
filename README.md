
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autogam

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/autogam)](https://CRAN.R-project.org/package=autogam)
[![R-CMD-check](https://github.com/tripartio/autogam/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tripartio/autogam/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

AutoGAM is a wrapper package for `mgcv` that makes it easier to create
high-performing Generalized Additive Models (GAMs). With its central
function `autogam()`, by entering just a dataset and the name of the
outcome column as inputs, AutoGAM tries to automate as much as possible
the procedure of configuring a highly accurate GAM at reasonably high
speed, even for large datasets.

## Installation

You can install the development version of autogam like so:

``` r
# install.packages("devtools")
devtools::install_github("tripartio/autogam")
```

## Example

Here’s a simple example using the `mtcars` dataset to predict `mpg`:

``` r
library(autogam)

autogam(mtcars, 'mpg')
#> Detecting distribution of `mpg`...
#> Loading required package: intervals
#> 
#> Fitting GAM with `Inverse Gaussian` distribution...
#> ✔ GAM successfully fit with 86.1% standardized accuracy.
#> 
#> Family: gaussian 
#> Link function: inverse 
#> 
#> Formula:
#> mpg ~ cyl + s(disp, bs = "cr") + s(hp, bs = "cr") + s(drat, bs = "cr") + 
#>     s(wt, bs = "cr") + s(qsec, bs = "cr") + vs + am + gear + 
#>     s(carb, k = 3, bs = "cr")
#> 
#> Estimated degrees of freedom:
#> 1.00 1.00 1.00 1.00 1.37 1.00  total = 11.37 
#> 
#> fREML score: 114.8354     
#> 
#> MAE: 1.307; Std. accuracy: 86.1%
```
