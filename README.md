
<!-- README.md is generated from README.Rmd. Please edit that file -->

# compassutils

<!-- badges: start -->

<!-- badges: end -->

The goal of compassutils is to supply useful plotting and/or utility
functions for working with output from COMPASS.

## Installation

You can install the latest version of computils from
[GitHub](https://www.github.com) with

``` r
if(!require("devtools", quietly = TRUE)) install.packages('devtools')
devtools::install_github("SATVILab/compassutils.git")
```

## Example

You can create a box- and dot-plot(s) of posterior probabilities for
individual cytokine combinations (more concise than `COMPASS::plot`
heatmap):

``` r
library(compassutils)
data('c_obj', package = compassutils)
plot_compass(c_obj = c_obj)
```

You can convert cytokine combination formats between standard “+/-”
format and COMPASS “\!&” format.

``` r
library(compassutils)
data('c_obj', package = compassutils)
plot_compass(c_obj = c_obj)
```
