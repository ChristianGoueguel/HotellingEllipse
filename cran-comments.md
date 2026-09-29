## Submission

This is an update (1.2.0 -> 1.3.0). It fixes a bug in `ellipseParam()`,
where the T-squared statistic and its cutoffs were returned on different
scales, and adds the `method` and `conf.limit` arguments. Dependencies used
only in examples and the vignette were moved from Imports to Suggests.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (aarch64), R release: `R CMD check --as-cran` (incoming checks included)
* GitHub Actions:
  * macos-latest (release)
  * windows-latest (release)
  * ubuntu-latest (devel)
  * ubuntu-latest (release)
  * ubuntu-latest (oldrel-1)

## Reverse dependencies

There are currently no reverse dependencies.
