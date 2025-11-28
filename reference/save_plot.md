# Save an individual layout

Save an individual plot layout.

## Usage

``` r
.save_plot(p, height, width, dir_save, file, save_format, n_row, n_col)
```

## Arguments

- p:

  object of class `gg`. Plot to save.

- height:

  numeric. Height of saved plot in centimeters.

- width:

  numeric. Width of saved plot in centimeters.

- dir_save:

  character. Where to save the output. Default is working directory.

- file:

  character. File name for the saved plot.

- save_format:

  "pdf" or "png". Plot device to use. Default is "png".

- n_row, n_col:

  integer. Number of objects plotted per row and column. Determines
  `width` and `height` if they are `NULL`.
