# HotellingEllipse 1.3.0

## Breaking changes

-   `ellipseParam()` now returns `Tsquare$value` as Hotelling's T-squared statistic (the squared Mahalanobis distance). Previously it returned the F-scaled statistic, (n − k) / (k(n − 1)) × T², while `cutoff.95pct` and `cutoff.99pct` were on the T-squared scale. Comparing `value` against the cutoffs therefore used a threshold that was too high by a factor of k(n − 1) / (n − k) (about 2 for n = 50, k = 2), so outliers were missed. `value` and the cutoffs are now on the same scale, and a point lies outside the ellipse exactly when `value` exceeds the corresponding cutoff.

-   `ellipseParam()` returns an additional `angle` column in `Ellipse`.

## New features

-   The ellipse (ellipsoid) is now rotated when the selected components are correlated, so that it always matches the T-squared statistic. Previously it was always axis-aligned. The scores of the samples a PCA or PLS model was fitted on are uncorrelated, so for them `angle` is 0 and the results are identical to previous versions. Rotation only applies to correlated scores, e.g. new samples projected onto a model, PLS Y-scores, rotated (varimax) or ICA components.

-   New `method` argument in `ellipseParam()` and `ellipseCoord()` to choose the T-squared limit. The default, `method = "f"`, is the limit used in previous versions, k(n − 1) / (n − k) × F(k, n − k), so cutoffs and ellipses are unchanged. `method = "beta"` uses the exact distribution of T-squared for the observations used to estimate the mean and covariance, (n − 1)² / n × Beta(k/2, (n − k − 1)/2) (Tracy, Young and Mason, 1992), e.g. the scores of the samples a PCA or PLS model was built on. The F-based limit is more conservative for small n: with n = 10 and k = 2, the 99% F limit is 19.5, while no observation can have T-squared above (n − 1)² / n = 8.1.

## Bug fixes

-   When `k = 2`, `ellipseParam()` now computes `Tsquare` on components `pcx` and `pcy`, the same components used for the ellipse. Previously it always used the first two components.

-   With `threshold`, removing near-zero variance components could leave the number of components undefined or equal to 1. `threshold = 1` could also fail due to floating-point rounding. Both cases are now handled.

-   Missing, non-scalar or non-integer values of `k`, `pcx`, `pcy`, `pts`, `threshold`, `conf.limit`, `rel.tol` and `abs.tol` now give informative errors instead of "missing value where TRUE/FALSE needed". An error is also raised when there are too few observations for the number of components.

-   `nb.comp` is now returned as an integer.

## Other changes

-   `dplyr`, `FactoMineR`, `ggforce`, `ggplot2`, `purrr` and `rgl` moved from Imports to Suggests, since they are only used in examples, the vignette and the README. `lifecycle` is no longer a dependency. `glue`, used in the vignette, was added to Suggests. The package now imports only `magrittr`, `stats` and `tibble`.

-   The vignette now uses the `knitr::rmarkdown` engine, which its `rmarkdown::html_vignette` output requires. With recent versions of `knitr`, the previous `knitr::knitr` engine failed to build it.

# HotellingEllipse 1.2.0

In this version:

-   Fixed issue #3   

-   Improved functions description.

-   Added more robust checks for input types and values

-   Added an `is_integer` function that uses implicit integer division. When a number is divided by 1 using integer division (`%/%`), the result will be equal to the original number only if it's an integer. The `is_integer` function is used to check `k`, `pcx`, and `pcy`.

-   Improved error messages to be more informative and specific.

-   Converted input to matrix once at the beginning to avoid repeated conversions.

-   For both `ellipseParam` and `ellipseCoord` functions, the code is restructured into smaller, more focused functions for better readability and maintainability.

-   Extracted the columns `x[, pcx]` and `x[, pcy]` into separate vectors `x_col` and `y_col` before using them in the calculations. Use `drop = TRUE` to ensure that even if `x` is a single-column matrix, it's converted to a vector.

-   Added three new parameters for the `ellipseParam` function: `threshold`, `real.tol`, and `abs.tol`. The `threshold` parameter serves to select the minimum number of `k` components that cumulatively explain at least the specified proportion of variance. As for the two tolerance parameters, `rel_tol` (for relative tolerance) and `abs_tol` (for absolute tolerance), they serve as variance threshold of components deemed negligible if their variance is below EITHER of these thresholds.

-   Implemented a three-dimensional extension to the `ellipseCoord` function by introducing a third axis parameter, `pcz`. This addition enables the computation of coordinates for 3D ellipsoids, expanding the function's capabilities beyond its previous 2D computation.

-   Renamed some variables for better clarity (e.g., `m` to `pts`)

# HotellingEllipse 1.1.0

This version includes the following modifications:

-   Correction of an error that occurred in the `ellipseParam` and `ellipseCoord`functions, when data is data.frame, instead of tibble

-   Improvement of the package documentation

-   Changes in the vignette (the difference between `ellipseParam` and `ellipseCoord` functions to draw Hotelling's ellipse is much clearer for users).

# HotellingEllipse 1.0.0

Not released

# HotellingEllipse 0.9.0

Not released

# HotellingEllipse 0.8.0

Not released

# HotellingEllipse 0.7.0

Not released

# HotellingEllipse 0.6.0

Not released

# HotellingEllipse 0.5.0

Not released

# HotellingEllipse 0.4.0

Not released

# HotellingEllipse 0.3.0

Not released

# HotellingEllipse 0.2.0

Not released

# HotellingEllipse 0.1.0

Initial release
