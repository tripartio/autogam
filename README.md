
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

ag <- autogam(mtcars, 'mpg')

summary(ag)
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> mpg ~ cyl + s(disp) + s(hp) + s(drat) + s(wt) + s(qsec) + vs + 
#>     am + gear + s(carb, k = 3)
#> 
#> Parametric coefficients:
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)   7.3453     5.3267   1.379   0.2671  
#> cyl           0.5814     0.5264   1.104   0.3547  
#> vs           10.3131     1.7012   6.062   0.0107 *
#> am            4.9605     0.8490   5.842   0.0118 *
#> gear          0.7107     0.7857   0.905   0.4362  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>           edf Ref.df      F p-value   
#> s(disp) 1.000  1.000  4.984  0.1117   
#> s(hp)   8.739  8.868 17.975  0.0170 * 
#> s(drat) 1.987  2.220 16.275  0.0395 * 
#> s(wt)   1.764  2.083  2.669  0.1891   
#> s(qsec) 8.904  8.970 28.950  0.0089 **
#> s(carb) 1.785  1.876  1.382  0.4412   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> R-sq.(adj) =  0.996   Deviance explained =  100%
#> GCV = 1.7279  Scale est. = 0.1523    n = 32
```
