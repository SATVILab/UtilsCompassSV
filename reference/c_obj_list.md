# Example list of COMPASS result objects

A named list of COMPASSResult objects containing example data for
testing and demonstration purposes. Each element represents results from
different stimulation conditions.

## Usage

``` r
c_obj_list
```

## Format

A list with 4 elements, each a COMPASSResult object:

- Live Mtb:

  Results for live Mycobacterium tuberculosis stimulation

- Secreted Mtb proteins:

  Results for secreted Mtb protein stimulation

- Non-secreted Mtb proteins:

  Results for non-secreted Mtb protein stimulation

- EBV/CMV:

  Results for EBV/CMV stimulation

## Examples

``` r
data("c_obj_list", package = "UtilsCompassSV")
names(c_obj_list)
#> [1] "Live Mtb"                  "Secreted Mtb proteins"    
#> [3] "Non-secreted Mtb proteins" "EBV/CMV"                  
```
