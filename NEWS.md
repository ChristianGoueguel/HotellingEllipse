# HotellingEllipse 1.2.0

In this version:

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
