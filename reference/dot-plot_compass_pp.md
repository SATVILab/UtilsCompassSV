# Plot COMPASS posterior probabilites

Plot COMPASS posterior probabilites

## Usage

``` r
.plot_compass_pp(
  c_obj,
  dir_save,
  prob_min,
  quant_min,
  silent,
  cyt_order,
  plot_prob_fill,
  facet,
  cyt_lab,
  boxplot_width,
  font_size,
  line_width,
  tile_colour,
  tile_alpha,
  tile_fill
)
```

## Arguments

- c_obj:

  a named list of COMPASSResult objects. Provides data to plot.

- dir_save:

  character. Where to save the output. Default is working directory.

- prob_min, quant_min:

  \[0,1\]. Specify the minimum probability of a response for the minimum
  quantile of samples that a cytokine combination must have to be
  included. For example, if `prob_min == 0.5` and `quant_min == 0.1`,
  then only cytokine combinations for which at least 10 Default is `0.8`
  and `0.25`.

- silent:

  logical. If `TRUE`, then any warnings that would have been otherwise
  given are not. Default is `FALSE`.

- cyt_order:

  character. If not `NULL`, then the order in which cytokines are
  arranged vertically along the annotation grid is given by this vector
  (with first element going at the bottom). If `NULL`, then cytokines
  are ordered by their order in COMPASS output. Default is `NULL`.

- plot_prob_fill:

  character. If a colour, then the boxplots are filled according to the
  specified value. If a named vector of colours names correspond to
  names of the bottom-most level of the `c_obj` lists, then boxplots are
  filled accordingly. Otherwise, boxplots are filled according to names
  of lower-most list but using

- facet:

  logical vector. Whether to facet or save individual plots for each
  group in `c_obj`. If `TRUE` only, then only a faceted version is
  saved. If `FALSE` only, then plots are saved individually. If
  `c(TRUE, FALSE)`, then both the facted and individual plots are saved.

- cyt_lab:

  function. A function that takes the cytokine names as input and
  returns the value desired to be plotted al ong the y-axis of the grid
  plot. If not `NULL`, then it is supplied when creating the grid plot
  to the `scale_y_continuous` function via the `labels` parameter. For
  example, if we have two cytokines, `"IFNg"` and `"TNF"`, but we want
  to display `"IFNg"` with the Greek gamma symbol, then we can set
  `cyt_lab` equal to the following:
  `cyt_lab = function(cyt) purrr::map(cyt, function(cyt_ind){switch(cyt_ind, "IFNg" = bquote(paste(plain(paste("IFN")), gamma)), cyt_ind)})`.
  This will change the label for `"IFNg"` but leave all the others as
  is.

- boxplot_width:

  numeric. Width parameter for geom_boxplot.

- font_size:

  numeric. Base font size for plots.

- line_width:

  numeric. Width of lines in plots.

- tile_colour:

  character. Colour to use for the tiles in the cytokine grid plot. If
  `NULL`, then no colour is used. Default is `NULL`.

- tile_alpha:

  numeric in `[0,1]`. Transparency level to use for the tiles in the
  cytokine grid plot. Default is `1` (no transparency, i.e. fully
  opaque).

- tile_fill:

  character vector. Fill colours to use for the tiles in the cytokine
  grid plot. Corresponds to the number of cytokines expressed in a
  combination. Earlier elements in the vector are used for cytokine
  combinations with fewer cytokines expressed. If `NULL`, the number of
  columns is automatically determined. Default is `NULL`.

## Value

A named list with names 'p_probs' and 'p_grid', corresponding to the
list of ggplot2 plots of posterior plots and the heat map of the
corresponding cytokine combination labels, respectively.
