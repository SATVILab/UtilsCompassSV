# Convert cytokine combination format to or from COMPASS format

Convert cytokine combination format between COMPASS format and "standard
" +/- format. See `cyt_combn` parameter for details. NOTE: At present
only converts from COMPASS to standard format.

## Usage

``` r
convert_cyt_combn_format(
  cyt_combn,
  to,
  force = FALSE,
  silent = FALSE,
  check = TRUE,
  lab = NULL
)
```

## Arguments

- cyt_combn:

  character vector. Cytokine combination, specified either in
  "\<cyt\>\<+-\>\<cyt\>\<+-\>\<cyt\>\<+-\>..." (e.g. IFNg+IL2-TNF+) or
  COMPASS "\<!\>\<cyt\>&\<!\>\<cyt\>&\<!\>\<cyt\>..." (e.g.
  IFNg&!IL2&TNF) format.

- to:

  'compass' or 'std'. Format to convert to. If `'compass'`, then output
  format is COMPASS format (specified above). If `'std'`, then output
  format is standard format (specified above).

- force:

  logical. If `TRUE`, then the code to convert to the specified format
  will be run. If `FALSE`, then checks are made to ensure that
  conversion is not done if already in the requested format. Default is
  `FALSE`.

- silent:

  logical. If `FALSE`, then warnings are printed if `force == FALSE` and
  it is detected that `cyt_combn` already appears to be in the requested
  format. Default is `FALSE`.

- check:

  logical. If `TRUE`, then the converted format is checked that each
  element has the same number of cytokines. Note that check is only
  performed if the conversion is attempted. Default is `TRUE`.

- lab:

  named character vector. Names are names for markers/channels as found
  in the cytokine combination, and elements are corresponding
  names/channels. If `NULL`, then no labelling is done. Default is
  `NULL`.

## Value

character vector.

## Examples

``` r
convert_cyt_combn_format(c("IFNg&!IL2"), to = "std")
#> [1] "IFNg+IL2-"
convert_cyt_combn_format(c("IFNg+IL2-"), to = "compass")
#> [1] "IFNg&!IL2"
```
