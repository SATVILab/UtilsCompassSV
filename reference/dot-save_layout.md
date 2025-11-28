# Create a layout based on post probs, cyt heat map and scores

Create a layout based on post probs, cyt heat map and scores

## Usage

``` r
.save_layout(
  ind,
  p_list_pp,
  p_list_scores = NULL,
  prop_pp,
  shift_plot_heatmap_x,
  shift_plot_scores_y,
  shift_plot_pp_y,
  height,
  width,
  dir_save,
  file,
  save_format,
  label,
  n_col,
  shift_label,
  font_size_labels
)
```

## Arguments

- ind:

  logical. If `TRUE`, then sets of posterior probability and PFS plots
  are saved individually.

- p_list_pp:

  list of `ggplot2` objects. Posterior probability plots and associated
  cytokine-label grids, as returned by
  `UtilsCompassSV:::.plot_compass_pp`.

- p_list_scores:

  list of `ggplot2` objects. PFS and FS response plots, as returned by
  `UtilsCompassSV:::.plot_compass_scores`.

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

- height, width:

  numeric. Height and width of saved plot(s).

- dir_save:

  character. Where to save the output. Default is working directory.

- file:

  character. File name for the saved plot.

- save_format:

  "pdf" or "png". Plot device to use. Default is "png".

- label:

  logical. If `TRUE` and `c_obj` is a list, then the names of elements
  in `c_obj` are used as labels for sub-figures. If `c_obj` is not a
  list, then no label is printed, regardless of the value of `label`.
  Default is `TRUE`.

- n_col:

  integer. Number of columns for the grid layout.

- shift_label:

  numeric vector, values in `[0,1]`. Specifies amount by which to shift
  the labels of the plot (if used). Default is `c(0.05, -0.04)`.

- font_size_labels:

  numeric. Font size for plot labels.
