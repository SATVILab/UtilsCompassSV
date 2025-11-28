# Plot COMPASS output

Plot COMPASS posterior probabilities as boxplots, and the PFS scores if
desired. A grid of figures is created whenever multiple COMPASS objects
are plotted simultaneous, with y-axis scales and cytokine combinations
displayed fixed across plots.

## Usage

``` r
plot_compass(
  c_obj,
  dir_save = getwd(),
  type = c("pp", "scores"),
  save = TRUE,
  save_format = "png",
  prob_min = 0.8,
  quant_min = 0.25,
  boxplot_width_scores = NULL,
  boxplot_width_pp = NULL,
  silent = FALSE,
  cyt_order = NULL,
  file_grid = NULL,
  file_ind = NULL,
  plot_prob_fill = NULL,
  shift_plot_heatmap_x = 0,
  shift_plot_scores_y = c(0, 0),
  shift_plot_pp_y = 0,
  shift_label = c(0.05, -0.04),
  prop_pp = c(0.7, 0.7),
  label = TRUE,
  return_plot_list = TRUE,
  facet = FALSE,
  n_col = NULL,
  cyt_lab = NULL,
  save_grid = TRUE,
  height_grid = NULL,
  width_grid = NULL,
  save_ind = FALSE,
  font_size = 14,
  line_width = 0.5,
  height_ind = NULL,
  width_ind = NULL,
  plot_scores_lims_y = NULL,
  font_size_labels = 14,
  tile_colour = NULL,
  tile_alpha = 1,
  tile_fill = NULL
)
```

## Arguments

- c_obj:

  object of class "COMPASSResult", or a list of such objects. Provides
  COMPASS data to plot.

- dir_save:

  character. Where to save the output. Default is working directory.

- type:

  "pp" and/or "scores". Specifies response type(s) to plot. If "pp" is
  included, then posterior probabilities of individual cytokine
  combinations are plotted. If "scores" is included, then the PFS and FS
  responses are included. Note that, at this stage, "pp" needs to be
  included, and so is added even if missing from `type`. Default is
  `c("pp", "scores")`.

- save:

  logical. If `TRUE`, then plots are saved. Default is `TRUE`.

- save_format:

  "pdf" or "png". Plot device to use. Default is "png".

- prob_min, quant_min:

  \[0,1\]. Specify the minimum probability of a response for the minimum
  quantile of samples that a cytokine combination must have to be
  included. For example, if `prob_min == 0.5` and `quant_min == 0.1`,
  then only cytokine combinations for which at least 10 Default is `0.8`
  and `0.25`.

- boxplot_width_pp, boxplot_width_scores:

  numeric. If not `NULL`, then supplied to `width` parameter of
  `geom_boxplot` for the posterior probability and scores plots,
  respectively. Purpose is to force the widths of the boxplots to be
  constant across elements in `c_obj`. Default is `NULL`.

- silent:

  logical. If `TRUE`, then any warnings that would have been otherwise
  given are not. Default is `FALSE`.

- cyt_order:

  character. If not `NULL`, then the order in which cytokines are
  arranged vertically along the annotation grid is given by this vector
  (with first element going at the bottom). If `NULL`, then cytokines
  are ordered by their order in COMPASS output. Default is `NULL`.

- file_grid, file_ind:

  character (vector). Names for grid plot and individual plots,
  respectively, to be saved as. If `is.null(file_grid)`, then the grid
  plot is simply named `compass_boxplots_grid`. If `is.null(file_ind)`,
  then each element has its name taken from the corresponding list name
  in `c_obj`. If `file_ind` is named, then the names of `c_obj` are used
  to map onto elements of `file_ind`.

- plot_prob_fill:

  character. If a colour, then the boxplots are filled according to the
  specified value. If a named vector of colours names correspond to
  names of the bottom-most level of the `c_obj` lists, then boxplots are
  filled accordingly. Otherwise, boxplots are filled according to names
  of lower-most list but using

- shift_plot_heatmap_x:

  \[0,1\]. Extent to shift prob_plot. Increasing it from 0 moves the
  start of the probability plot further to the right. Maximum value is 1
  (at which point the plot will effectively be pushed off the plotting
  surface). Useful to increase to a value such as 0.01 if the cytokine
  names are long and push the labelling grid too far to the right. Tweak
  as required. Default is `0`.

- shift_plot_scores_y:

  numeric vector of length two, restricted to \[-1,1\]. Specifies the
  amount to squeeze the scores plot in. The first element controls the
  bottom position of the plot, and the second the top. For both
  elements, a positive value means moving upwards. A value of zero
  corresponds no shift. Typically the bottom element and top element
  should both be shifted down (and therefore have negative values). Only
  applies if type includes both `"pp"` and `"scores"`. Default is
  `c(0,0)`.

- shift_plot_pp_y:

  \[0,1\]. Specifies amount by which to shift the upper point of the
  probability plot upwards. Positive values make it larger. Typically
  you want to shift this down, if anything, so use negative values.
  Default is 0.

- shift_label:

  numeric vector, values in `[0,1]`. Specifies amount by which to shift
  the labels of the plot (if used). Default is `c(0.05, -0.04)`.

- prop_pp:

  numeric vector, values in `[0,1]`. Specifies coordinate-wise the
  proportion of plot region for a single group in `c_obj` (if `c_obj` is
  a list; otherwise it's simply the proportion of the plot) that is
  devoted (before applying `shift_plot_pp_y`) to the plot of posterior
  probabilities. The first element is the proportion allocated to the
  probability plot along the x-axis, and (`1-prop_pp[1]`) is then the
  space allocated to the scores plot. The first element is set equal to
  0 if there is no scores plot. The second element is the proportion
  allocate to the probabilty plot along the y-axis, and (`1-prop_pp[1]`)
  is then the space allocated to the cytokine indicator grid.

- label:

  logical. If `TRUE` and `c_obj` is a list, then the names of elements
  in `c_obj` are used as labels for sub-figures. If `c_obj` is not a
  list, then no label is printed, regardless of the value of `label`.
  Default is `TRUE`.

- return_plot_list:

  logical. If `TRUE`, then a named list of the plots used to create the
  figure are returned. The first element is `"p_grid"` for the cytokine
  grid plot. The second element is `"p_probs"` for a list of the
  posterior probability plots. The third element is named `"p_scores"`,
  and has sub-elements that are the plots of the PFS and FS responses
  for each group. The `"p_scores"` element is only supplied if
  `"scores"` is in `"type"`.

- facet:

  logical vector. Whether to facet or save individual plots for each
  group in `c_obj`. If `TRUE` only, then only a faceted version is
  saved. If `FALSE` only, then plots are saved individually. If
  `c(TRUE, FALSE)`, then both the facted and individual plots are saved.

- n_col:

  integer. Number of columns for the grid plot when `facet = TRUE`.

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

- save_grid:

  logical. If `TRUE`, then a grid of all individual elements in `c_obj`
  are saved. Default is `TRUE`.

- height_grid, width_grid:

  numeric. Height and width, respectively, in centimetres of the saved
  figure (if saved). If `NULL`, then appropriate values are guessed at
  and used. Default is `NULL`.

- save_ind:

  logical. If `TRUE`, then plots for individual elements in `c_obj` are
  saved. Default is `FALSE`.

- font_size:

  numeric. Base font size for plots. Default is `14`.

- line_width:

  numeric. Width of lines in plots. Default is `0.5`.

- height_ind, width_ind:

  numeric. Height and width, respectively, in centimetres of the saved
  figure (if saved), when saving individual elements in `c_obj`. If
  `NULL`, then appropriate values are guessed at and used. Default is
  `NULL`.

- plot_scores_lims_y:

  numeric vector of length 2. If not `NULL`, specifies the y-axis range
  for the scores plots. Default is `NULL`.

- font_size_labels:

  numeric. Font size for plot labels. Default is `14`.

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

A list, where each element is a `ggplot2` object.

## Examples

``` r
library(UtilsCompassSV)
data("c_obj_list", package = "UtilsCompassSV")
plot_compass(
  c_obj = c_obj_list,
  type = c("pp", "scores"),
  return_plot_list = FALSE,
  shift_plot_scores = c(-0.05, 0.05),
  shift_plot_pp_y = -0.075,
  shift_plot_heatmap_x = 0.052
)
# The plot will then be saved to the working directory.

# Can also use Greek symbols for cytokines:
get_cyt_lab <- function(cyt) {
  lapply(cyt, function(cyt_ind) {
    switch(cyt_ind,
      "IFNg" = bquote("IFN" ~ gamma),
      cyt_ind
    )
  })
}
plot_compass(c_obj_list[1],
  type = c("pp"),
  return = FALSE, shift_plot_scores = c(-0.05, 0.05), facet = FALSE,
  shift_plot_pp_y = -0.05, shift_plot_heatmap_x = 0.052,
  cyt_lab = get_cyt_lab
)
```
