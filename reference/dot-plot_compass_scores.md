# Plot COMPASS PFS and FS scores

Plot COMPASS PFS and FS scores

## Usage

``` r
.plot_compass_scores(
  c_obj,
  plot_prob_fill,
  boxplot_width,
  plot_scores_lims_y,
  font_size,
  line_width = 0.5
)
```

## Arguments

- c_obj:

  a named list of COMPASSResult objects. Provides data to plot.

- plot_prob_fill:

  character. If a colour, then the boxplots are filled according to the
  specified value. If a named vector of colours names correspond to
  names of the bottom-most level of the `c_obj` lists, then boxplots are
  filled accordingly. Otherwise, boxplots are filled according to names
  of lower-most list but using

- boxplot_width:

  numeric. Width parameter for geom_boxplot.

- plot_scores_lims_y:

  numeric vector of length 2. If not `NULL`, specifies the y-axis range
  for the scores plots. Default is `NULL`.

- font_size:

  numeric. Base font size for plots.

- line_width:

  numeric. Width of lines in plots.

## Value

A named list with names set to the names of `c_obj`, where each element
is the corresponding boxplot of PFS and FS scores.
