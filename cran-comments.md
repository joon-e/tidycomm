## Resubmission

* Fixed typos
* Included references for intercoder reliability coefficients
* Package 'tidyselect' has been moved to 'Suggests' instead of 'Imports',
  as it is only used for tests and vignettes

## Test environments
* local Win 10 install, R 3.6.1
* OS X (on travis-ci), release
* Linux (on travis-ci), release
* Linux (on travis-ci), devel
* win-builder, devel

Test environment config on travis-ci included installing the 'libgsl23' (linux)/
'gsl' (osx) packages, which are needed for R package 'gsl', which is a dependency
of the 'MBESS' package.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
