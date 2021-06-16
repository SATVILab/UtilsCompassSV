
<!-- README.md is generated from README.Rmd. Please edit that file -->

# compassutils

<!-- badges: start -->
<!-- badges: end -->

The goal of compassutils is to supply useful plotting and utility
functions for working with output from COMPASS.

## Installation

You can install the latest version of compassutils from
[GitHub](https://www.github.com) with

``` r
if(!require("devtools", quietly = TRUE)) install.packages('devtools')
devtools::install_github("SATVILab/compassutils")
```

## Example

### Plots

Create boxplots of posterior probabilities for individual cytokine
combinations, as well as the PFS and FS responses. These are more
concise than standard `COMPASS` heatmaps and automatically force the
same cytokine combinations to be present and cytokines in the same order
for all “groups” plotted together. Note that the PFS and FS scores plot
need not be added.

``` r
library(compassutils)
data('c_obj_list', package = 'compassutils')
plot_compass(
  c_obj = c_obj_list, 
  dir_save = here::here('data-raw'),
  type = c('pp', 'scores'),
  return_plot_list = FALSE, 
  shift_plot_scores = c(-0.05, 0.05), 
  shift_plot_pp_y = -0.075, 
  shift_plot_heatmap_x = 0.052
  )
knitr::include_graphics('data-raw/compass_boxplots_grid.png')
```

<img src="data-raw/compass_boxplots_grid.png" width="100%" />

### Utilities

Convert cytokine combination formats between standard “+/-” format and
COMPASS “!&” format.

``` r
cyt_combn_vec_compass <- c("IFNg&!IL2&TNF&IL6&!IL22",
                           "IFNg&IL2&TNF&IL6&IL22",
                           "!IFNg&!IL2&!TNF&!IL6&!IL22")
cyt_combn_vec_std <- compassutils::convert_cyt_combn_format(cyt_combn_vec_compass, 
                                                            to = 'std')
pander::pandoc.table(tibble::tibble(
  `COMPASS format` = cyt_combn_vec_compass, 
  `Standard format` = cyt_combn_vec_std 
  ))
```

|       COMPASS format       |    Standard format     |
|:--------------------------:|:----------------------:|
|  IFNg&!IL2&TNF&IL6&!IL22   | IFNg+IL2-TNF+IL6+IL22- |
|   IFNg&IL2&TNF&IL6&IL22    | IFNg+IL2+TNF+IL6+IL22+ |
| !IFNg&!IL2&!TNF&!IL6&!IL22 | IFNg-IL2-TNF-IL6-IL22- |

Calculate the probability of an individual responding to at least one
cytokine combination, assuming independence of the probability estimates
between cytokine combinations.

``` r
response_prob(c_obj = c_obj_list[[4]]) %>%
  head() %>%
  pander::pandoc.table()
```

|   sampleid   |  prob  |
|:------------:|:------:|
|  010673\_D0  | 0.076  |
|  010782\_D0  | 0.4405 |
| 010782\_D720 | 0.903  |
|  010978\_D0  |   1    |
|  010993\_D0  | 0.8039 |
|  020185\_D0  |   0    |
