This update elevates the version directly to 0.4.1 as it introduces significant enhancements including new tdcmm/tibble returns and functions such as partial correlation, linear regression, one-sample t-test, scale transformations, percentiles, visualizations, and additional datasets (snscomments and incvlcomments). Additionally, we have applied a minor patch to encase the 'visualize()' examples within 'dontrun()' to reduce the elapsed time during the compilation of these examples.

## Test environments

* local Win 10 install, R 4.3.2
* OS X latest (on GitHub Actions), release
* Ubuntu 20.04 (on GitHub Actions), release
* Ubuntu 20.04 (on GitHub Actions), devel
* Windows latest (on GitHub Actions), release

There were 3 NOTES identified when testing the package on Windows using R-hub, but all are referenced in the open issues of the R-hub package and thus appear to be unrelated to this package:

> checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

> checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

> checking HTML version of manual ... NOTE
   Skipping checking math rendering: package 'V8' unavailable


## R CMD check results

0 errors | 0 warnings | 0 note


## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
