# tidycomm 0.4.2

## Installation / Compatibility
* Minimum required R version increased from 2.10 to 3.6.0.

## Bugfixes
* Updated GGally usage to rely on `ggmatrix` class instead of `gg`, ensuring compatibility with the latest ggplot2 changes.
* Migrated tests to testthat v3 to maintain correct test behavior.

## Documentation
* Merged README.Rmd with index for pkgdown site.
* Updated links to reflect the new GitHub organization (https://github.com/tidycomm/tidycomm).
* Enabled Bootstrap 5 and light/dark switch for the pkgdown documentation site.

# tidycomm 0.4.1

# tidycomm 0.3.0

## New features
* Converted `tibble` returns into `tdcmm/tibble` return objects (they behave just like tibbles but are in essence our own objects now)
* Added partial correlation in `correlate(..., partial = z_var)`
* Added correlation with a focus variable `correlate(..., with = focus_var)`
* Added linear regression `regress()`
* Added one-sample t-test `t_test(..., mu = ...)`
* Added `reverse_scale()`, `minmax_scale()`, `z_scale()`, `center_scale()`, `setna_scale()`, `recode_cat_scale()`, `recode_scale()`, and `dummify_cale()` to shift and modify continuous and categorical scales
* Added `tab_percentiles()`
* Added `visualize()` to visualize almost everything
* Added `snscomments` and `incvlcomments` as additional data sets

## Minor changes
* Changed `unianova()` and `t_test()` to build on `leveneTest()`
* Allowed `test_icr()` to work with grouped data
* Converted all code examples in documentation to be built on `tidycomm`-provided data sets
* Added `omega_squared`, `Levene_p`, and `var_equal` columns to default return from `unianova()`
* Added `d`, `se`, `t`, and `df`
* Removed `null.value` from list of return values in `unianova()` post-hoc test
* Renamed `unianova()` return column names to `Variable` (previously: `Var`), `Group_Var` (prev. `term`), `Delta_M` (prev. `estimate`), `p` (prev. `adj.p.value`), `conf.lower` and `conf.upper` (prev. `conf.low` and `conf.high`)

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
