# tidycomm 0.3.0

## New features
* Converted `tibble` returns into `tdcmm/tibble` return objects (they behave just like tibbles but are in essence our own objects now)
* Added partial correlation in `correlate(..., partial = TRUE)`
* Added linear regression `regress()`
* Added one-sample t-test `t_test(..., my = ...)`
* Added `reverse_scale()`, `minmax_scale()`, `z_scale()`, and `center_scale()` to shift and modify continuous scales
* Added `tab_percentiles()`
* Added `visualize()` to visualize almost everything
* Added `snscomments` and `incvlcomments` as additional data sets

## Minor changes
* Changed `unianova()` and `t_test()` to build on `leveneTest()`
* Allowed `test_icr()` to work with grouped data
* Converted all code examples in documentation to be built on `tidycomm`-provided data sets

## Bugfixes
* Unified output to coherent number of after-comma digits

# tidycomm 0.2.0

## New features
* Added Fretwurst's Lotus and S-Lotus intercoder reliability coeffecients to `test_icr()` function
* `describe_cat()` function added to describe categorical variables

## Minor changes
* More descriptive error messages for common errors
* `describe()` now also reports 95% confidence intervals
* `describe()` now reports valid N instead of full N
* `test_icr()` now works with `tidyselect` selection

## Bugfixes
* Empty groups are dropped if describing with more than one grouping variable
* Krippendorff's Alpha returns 1 if variable has only one category
* `unianova()` now works with variable names containing whitespace
* Groups are dropped for `test_icr()` to avoid computational issues
