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
