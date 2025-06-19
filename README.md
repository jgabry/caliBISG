
<!-- README.md is generated from README.Rmd. Please edit that file -->

# caliBISG

<!-- badges: start -->

[![R-CMD-check](https://github.com/jgabry/caliBISG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jgabry/caliBISG/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jgabry/caliBISG/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jgabry/caliBISG?branch=main)
<!-- badges: end -->

This R package implements the calibrated Bayesian improved surname
geocoding (caliBISG) method from Greengard and Gelman (2025). caliBISG
is easily interpretable, more accurate, and better calibrated than
traditional BISG. The package is currently a work in progress. We
encourage you to test it out and provide feedback, but please note that
the API may still change.

By inputting an individual’s surname, state, and county, users can
obtain probabilistic estimates of race and ethnicity that can be used by
researchers when self-reported data is unavailable or incomplete. The
package provides functions to identify the single most probable race or
to retrieve the full set of probabilities across six standard race
categories often used in social science research.

The package provides the calibrated BISG estimates and traditional BISG
estimates side by side for direct comparison. This comparative output
will allow researchers to understand the practical implications of using
the calibrated approach and easily identify cases where the new and
traditional methods disagree.

## Installation

You can install the development version of caliBISG from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("jgabry/caliBISG")
```

To also build the tutorial vignette add the argument
`build_vignettes = TRUE`. This will take longer to install because
building the vignette requires downloading some caliBISG data files.

``` r
remotes::install_github("jgabry/caliBISG", build_vignettes = TRUE)
```

After installing the package with the vignette you can view the vignette
by running `vignette("caliBISG", package = "caliBISG")` or
`browseVignettes("caliBISG")`

## Quick example

We recommend the package vignette for a more detailed introduction to
the package. Here is a quick example.

``` r
library(caliBISG)
#> This is caliBISG version 0.1.0
```

First we load the package and download the caliBISG data file for
Oklahoma. If the data has already been downloaded then it will not be
downloaded again.

``` r
download_data("OK")
#> 
#> OK-2020.rds already exists. Skipping.
```

We then ask for the most probable race based on name, state, and county.
Both caliBISG and tradtional BISG are provided. Here the two methods
agree on the surname Lopez but disagree on the most probable race for
the surname Jackson in Tulsa, OK.

``` r
most_probable_race(name = c("Lopez", "Jackson"), state = "OK", county = "Tulsa")
#>      name year state county calibisg_race bisg_race in_census
#> 1   lopez 2020    OK  tulsa      hispanic  hispanic      TRUE
#> 2 jackson 2020    OK  tulsa      white_nh  black_nh      TRUE
```

We can also compare the underlying probabilities.

``` r
race_probabilities(name = c("Lopez", "Jackson"), state = "OK", county = "Tulsa")
#> Surname:  Lopez     
#> State:    OK        
#> County:   Tulsa     
#> Year:     2020      
#> 
#> Race       Pr_calibisg  Pr_bisg   
#> ---------------------------------------- 
#> AIAN       0.03         0.04      
#> API        0.00         0.01      
#> Black      0.00         0.01      
#> Hispanic   0.91         0.89      
#> White      0.05         0.06      
#> Other      0.00         0.01      
#> ---------------------------------------- 
#> 
#> Surname:  Jackson   
#> State:    OK        
#> County:   Tulsa     
#> Year:     2020      
#> 
#> Race       Pr_calibisg  Pr_bisg   
#> ---------------------------------------- 
#> AIAN       0.09         0.09      
#> API        0.00         0.00      
#> Black      0.37         0.44      
#> Hispanic   0.03         0.02      
#> White      0.48         0.38      
#> Other      0.04         0.07      
#> ----------------------------------------
```

## References

Philip Greengard and Andrew Gelman (2025). A calibrated BISG for
inferring race from surname and geolocation. *Journal of the Royal
Statistical Society Series A: Statistics in Society*.
<https://doi.org/10.1093/jrsssa/qnaf003>
