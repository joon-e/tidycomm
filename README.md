
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidycomm <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidycomm)](https://CRAN.R-project.org/package=tidycomm)
[![Travis build
status](https://travis-ci.org/joon-e/tidycomm.svg?branch=master)](https://travis-ci.org/joon-e/tidycomm)
[![Codecov test
coverage](https://codecov.io/gh/joon-e/tidycomm/branch/master/graph/badge.svg)](https://codecov.io/gh/joon-e/tidycomm?branch=master)
<!-- badges: end -->

Tidycomm provides convenience functions for common tasks in
communication research. All functions follow the style and syntax of the
[tidyverse](https://www.tidyverse.org/).

Currently, tidycomm includes functions for various methods of univariate
and bivariate data description and analysis, data modification, and
intercoder reliability tests.

## Installation

Install tidycomm from CRAN:

``` r
install.packages("tidycomm")
```

Or install the most recent development version of tidycomm with:

``` r
devtools::install_github("joon-e/tidycomm")
```

## Usage

``` r
library(tidycomm)
```

tidycomm functions follow the style and syntax of the
[tidyverse](https://www.tidyverse.org/) functions:

  - they always assume a `tibble` as their first argument
  - they will always return a `tibble` as well, so they can be easily
    integrated into pipes
  - data variables (`tibble` columns) are passed to function calls
    directly as symbols

<!-- end list -->

``` r
WoJ %>% # Worlds of Journalism sample data
  describe(autonomy_selection, autonomy_emphasis)
#> # A tibble: 2 x 13
#>   Variable     N Missing     M    SD   Min   Q25   Mdn   Q75   Max Range
#>   <chr>    <int>   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 autonom~  1200       3  3.88 0.803     1     4     4     4     5     4
#> 2 autonom~  1200       5  4.08 0.793     1     4     4     5     5     4
#> # ... with 2 more variables: Skewness <dbl>, Kurtosis <dbl>
```

Most functions will automatically use all relevant variables in the data
if no variables are specified in the function call. For example, to
compute descriptive statistics for all numeric variables in the data,
just call `describe()` without further arguments:

``` r
WoJ %>% 
  describe()
#> # A tibble: 11 x 13
#>    Variable     N Missing     M     SD   Min   Q25   Mdn   Q75   Max Range
#>    <chr>    <int>   <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 autonom~  1200       3  3.88  0.803     1  4        4     4     5     4
#>  2 autonom~  1200       5  4.08  0.793     1  4        4     5     5     4
#>  3 ethics_1  1200       0  1.63  0.892     1  1        1     2     5     4
#>  4 ethics_2  1200       0  3.21  1.26      1  2        4     4     5     4
#>  5 ethics_3  1200       0  2.39  1.13      1  2        2     3     5     4
#>  6 ethics_4  1200       0  2.58  1.25      1  1.75     2     4     5     4
#>  7 work_ex~  1200      13 17.8  10.9       1  8       17    25    53    52
#>  8 trust_p~  1200       0  3.05  0.811     1  3        3     4     5     4
#>  9 trust_g~  1200       0  2.82  0.854     1  2        3     3     5     4
#> 10 trust_p~  1200       0  2.42  0.736     1  2        2     3     4     3
#> 11 trust_p~  1200       0  2.52  0.712     1  2        3     3     4     3
#> # ... with 2 more variables: Skewness <dbl>, Kurtosis <dbl>
```

Likewise, compute intercoder reliability tests for all variables by only
specifying the post and coder ID variables:

``` r
fbposts %>% # Facebook post codings sample data
  test_icr(post_id, coder_id)
#> # A tibble: 5 x 8
#>   Variable n_Units n_Coders n_Categories Level Agreement Holstis_CR
#>   <chr>      <int>    <int>        <int> <chr>     <dbl>      <dbl>
#> 1 type          45        6            4 nomi~     1          1    
#> 2 n_pictu~      45        6            7 nomi~     0.822      0.930
#> 3 pop_eli~      45        6            6 nomi~     0.733      0.861
#> 4 pop_peo~      45        6            2 nomi~     0.778      0.916
#> 5 pop_oth~      45        6            4 nomi~     0.867      0.945
#> # ... with 1 more variable: Krippendorffs_Alpha <dbl>
```

For detailed examples, see the vignettes:

``` r
browseVignettes("tidycomm")
```
