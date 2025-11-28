# Calculate COMPASS-derived overall responder probability

Calculate the probability of responding to at least one cytokine
combination, assuming independence of the probability estimates between
cytokine combinations.

## Usage

``` r
response_prob(c_obj, exc = NULL)
```

## Arguments

- c_obj:

  object of class 'COMPASSResult'. The posterior probabilities for
  individual cytokine combinations are obtained here.

- exc:

  character vector. Specifies cytokine combination(s) to exclude. If
  `NULL`, then none except the all-negative population are excluded.
  Default is `NULL`.

## Value

A dataframe with columns sampleid and prob.

## Details

Calculates the probability of an individual responding to at least one
cytokine combination as (1-product((1-prob_i))), where prob_i is the
probability of responding to the i-th cytokine combination and the
product is taken over all cytokine combinations except the all-negative
cytokine combination.

## Examples

``` r
data("c_obj", package = "UtilsCompassSV")
response_prob(c_obj = c_obj)
#> # A tibble: 148 × 2
#>    sampleid     prob
#>    <chr>       <dbl>
#>  1 010673_D0   1    
#>  2 010782_D0   1    
#>  3 010782_D720 1    
#>  4 010978_D0   1    
#>  5 010993_D0   1    
#>  6 020185_D0   1    
#>  7 030442_D0   1    
#>  8 030448_D0   1    
#>  9 030545_D0   1    
#> 10 030697_D0   0.521
#> # ℹ 138 more rows
response_prob(
  c_obj = c_obj,
  exc = c(
    "IFNg&IL2&TNF&!IL17&!IL6&!IL22",
    "IFNg&!IL2&TNF&!IL17&!IL6&!IL22"
  )
)
#> # A tibble: 148 × 2
#>    sampleid     prob
#>    <chr>       <dbl>
#>  1 010673_D0   1    
#>  2 010782_D0   0.992
#>  3 010782_D720 0.996
#>  4 010978_D0   1    
#>  5 010993_D0   1    
#>  6 020185_D0   0.998
#>  7 030442_D0   1    
#>  8 030448_D0   1    
#>  9 030545_D0   0.897
#> 10 030697_D0   0.521
#> # ℹ 138 more rows
```
