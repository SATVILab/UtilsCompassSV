# Getting Started with UtilsCompassSV

``` r
library(UtilsCompassSV)
```

## Introduction

UtilsCompassSV provides utility functions for plotting and working with
COMPASS (Combinatorial Polyfunctionality Analysis of Single Cells)
output. The primary function is `plot_compass`, which creates concise
visualizations of posterior probabilities for cytokine combinations.

## Available Data

The package includes sample COMPASS result objects for demonstration:

- `c_obj`: A single COMPASSResult object
- `c_obj_list`: A named list of COMPASSResult objects

``` r
data("c_obj", package = "UtilsCompassSV")
data("c_obj_list", package = "UtilsCompassSV")
```

## Main Functions

### plot_compass

The `plot_compass` function creates visualizations of COMPASS posterior
probabilities as boxplots, with optional PFS (Polyfunctionality Score)
plots.

``` r
# Basic usage with a list of COMPASS objects
plot_compass(
  c_obj = c_obj_list,
  type = c("pp", "scores"),
  return_plot_list = FALSE,
  shift_plot_scores = c(-0.05, 0.05),
  shift_plot_pp_y = -0.075,
  shift_plot_heatmap_x = 0.052
)
```

The plot will be saved to the working directory as a PNG file (by
default, `save = TRUE` and `save_format = "png"`).

Key parameters:

- `c_obj`: A COMPASSResult object or a list of such objects
- `type`: What to plot - `"pp"` for posterior probabilities, `"scores"`
  for PFS/FS scores
- `prob_min`, `quant_min`: Filter cytokine combinations by minimum
  probability threshold
- `save_format`: Output format (`"png"` or `"pdf"`)

### convert_cyt_combn_format

Convert cytokine combination notation between COMPASS format and
standard +/- format:

``` r
# Convert from COMPASS format to standard format
convert_cyt_combn_format(c("IFNg&!IL2&TNF"), to = "std")
#> [1] "IFNg+IL2-TNF+"

# Convert from standard format to COMPASS format
convert_cyt_combn_format(c("IFNg+IL2-TNF+"), to = "compass")
#> [1] "IFNg&!IL2&TNF"
```

Format descriptions:

- **Standard format**: `IFNg+IL2-TNF+` (+ for positive, - for negative)
- **COMPASS format**: `IFNg&!IL2&TNF` (& as separator, ! for negation)

### response_prob

Calculate the overall probability of responding to at least one cytokine
combination:

``` r
# Calculate response probability
response_prob(c_obj = c_obj)
#> # A tibble: 148 × 2
#>    sampleid     prob
#>    <chr>       <dbl>
#>  1 010673_D0   1    
#>  2 010782_D0   1    
#>  3 010782_D720 1    
#>  4 010978_D0   1    
#>  5 010993_D0   1    
#>  6 020185_D0   1    
#>  7 030442_D0   1    
#>  8 030448_D0   1    
#>  9 030545_D0   1    
#> 10 030697_D0   0.521
#> # ℹ 138 more rows

# Exclude specific cytokine combinations
response_prob(
  c_obj = c_obj,
  exc = c("IFNg&IL2&TNF&!IL17&!IL6&!IL22")
)
#> # A tibble: 148 × 2
#>    sampleid     prob
#>    <chr>       <dbl>
#>  1 010673_D0   1    
#>  2 010782_D0   0.992
#>  3 010782_D720 0.997
#>  4 010978_D0   1    
#>  5 010993_D0   1    
#>  6 020185_D0   0.998
#>  7 030442_D0   1    
#>  8 030448_D0   1    
#>  9 030545_D0   0.911
#> 10 030697_D0   0.521
#> # ℹ 138 more rows
```

## Customizing Plots

### Using Greek Symbols for Cytokines

You can customize cytokine labels to use Greek symbols:

``` r
get_cyt_lab <- function(cyt) {
  lapply(cyt, function(cyt_ind) {
    switch(cyt_ind,
      "IFNg" = bquote("IFN" ~ gamma),
      cyt_ind
    )
  })
}

plot_compass(
  c_obj_list[1],
  type = "pp",
  cyt_lab = get_cyt_lab
)
```

### Saving Options

Control how and where plots are saved:

``` r
plot_compass(
  c_obj = c_obj_list,
  save = TRUE,
  save_format = "pdf",
  save_grid = TRUE,
  save_ind = FALSE,
  dir_save = tempdir(),
  file_grid = "my_compass_plot"
)
```

## Further Information

For more details on individual functions, see their help pages:

- [`?plot_compass`](https://satvilab.github.io/UtilsCompassSV/reference/plot_compass.md)
- [`?convert_cyt_combn_format`](https://satvilab.github.io/UtilsCompassSV/reference/convert_cyt_combn_format.md)
- [`?response_prob`](https://satvilab.github.io/UtilsCompassSV/reference/response_prob.md)
