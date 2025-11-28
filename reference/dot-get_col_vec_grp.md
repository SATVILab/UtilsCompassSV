# Get colours for groups for boxplots

Get colours for groups for boxplots

## Usage

``` r
.get_col_vec_grp(plot_prob_fill = NULL, .grp = NULL)
```

## Arguments

- plot_prob_fill:

  character. If a colour, then the boxplots are filled according to the
  specified value. If a named vector of colours names correspond to
  names of the bottom-most level of the `c_obj` lists, then boxplots are
  filled accordingly. Otherwise, boxplots are filled according to names
  of lower-most list but using

- .grp:

  character vector. Names of groups in COMPASS object.

## Value

A named character vector, where names are names of groups and elements
are colours for the groups
