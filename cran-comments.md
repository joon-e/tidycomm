# Resubmission
This is a resubmission of package tidycomm, which was archived on 2025-08-25 due to failing checks.

The following issues were addressed:
1. Rd cross-references: All \link{} references to objects from other packages (e.g., ggplot2, tibble) now include proper package prefixes, e.g., \link[ggplot2]{ggplot} and \link[tibble]{tibble}.
2. Test failures: The visualize() function now returns GGally::ggmatrix objects (for richer visualization of correlation matrices). Tests expecting a 'gg' object were updated to expect 'ggmatrix' objects instead, resolving the previous test failures.

All checks now pass on R-release, R-devel, and Windows/Linux flavors.
Please consider re-activating the package. Thank you!

## About
This update elevates the version to 0.4.2 as it only introduces minor bugfixes without any function enhancement.

## Test environments

* local Win 10 install, R-4.5.1
* OS X latest (on GitHub Actions), release
* Ubuntu 24.04 (on GitHub Actions), release
* Ubuntu 24.04 (on GitHub Actions), devel
* Windows latest (on GitHub Actions), release

## R CMD check results

── R CMD check results ──────────────────────────────────────────────────────────────── tidycomm 0.4.2 ────
Duration: 3m 5.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded


## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
