# Example COMPASS result object

A COMPASSResult object containing example data for testing and
demonstration purposes.

## Usage

``` r
c_obj
```

## Format

A COMPASSResult object with the following structure:

- fit:

  Fitted model results from COMPASS analysis

- data:

  Input data used for COMPASS analysis

## Examples

``` r
data("c_obj", package = "UtilsCompassSV")
class(c_obj)
#> [1] "COMPASSResult"
```
