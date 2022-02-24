
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

# HotellingEllipse <img src="man/figures/logo.png" align="right" height="159"/>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/ChristianGoueguel/HotellingEllipse/workflows/R-CMD-check/badge.svg)](https://github.com/ChristianGoueguel/HotellingEllipse/actions)
[![codecov](https://codecov.io/gh/ChristianGoueguel/HotellingEllipse/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ChristianGoueguel/HotellingEllipse)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/HotellingEllipse)](https://CRAN.R-project.org/package=HotellingEllipse)
[![](https://cranlogs.r-pkg.org/badges/HotellingEllipse)](https://cran.r-project.org/package=HotellingEllipse)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

`HotellingEllipse` computes the lengths of the semi-minor and semi-major
axes for plotting Hotelling ellipse at 95% and 99% confidence intervals.
The package also provides the *x*-*y* coordinates at user-defined
confidence intervals.

## Installation

Install `HotellingEllipse` from CRAN:

``` r
install.packages("HotellingEllipse")
```

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("ChristianGoueguel/HotellingEllipse")
```

## Usage

Below is an overview of how `HotellingEllipse` can help draw a
confidence ellipse:

-   using `FactoMineR::PCA()` we first perform Principal Component
    Analysis (PCA) from a LIBS spectral dataset `data("specData")` and
    extract the PCA scores.

-   with `ellipseParam()` we get the T<sup>2</sup> value along with the
    values of the semi-minor and semi-major axes. Whereas,
    `ellipseCoord()` provides the *x* and *y* coordinates for drawing
    the Hotelling ellipse at user-defined confidence interval.

-   using `ggplot2::ggplot()` and `ggforce::geom_ellipse()` we plot the
    scatterplot of PCA scores as well as the corresponding Hotelling
    ellipse which represents the confidence region for the joint
    variables at 99% and 95% confidence intervals.

**Step 1.** Load the package.

``` r
library(HotellingEllipse)
```

**Step 2.** Load LIBS dataset into R session.

``` r
data("specData")
```

**Step 3.** Perform principal component analysis.

``` r
set.seed(123)
pca_mod <- specData %>%
  select(where(is.numeric)) %>%
  PCA(scale.unit = FALSE, graph = FALSE)
```

**Step 4.** Extract PCA scores.

``` r
pca_scores <- pca_mod %>%
  pluck("ind", "coord") %>%
  as_tibble() %>%
  print()
#> # A tibble: 171 × 5
#>      Dim.1   Dim.2   Dim.3   Dim.4    Dim.5
#>      <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
#>  1  14959.  -6726. -23825.  -2635. -30343. 
#>  2  37177. -16610. -14912.   8591. -17120. 
#>  3 109909. -48972.  38185. -11120.   2845. 
#>  4  10815.  49283.  18459.  -5409. -15114. 
#>  5 115992. -31406. -16042. -20505. -14349. 
#>  6  80481.  19867.  34014. -12234.    -42.8
#>  7  48511.  16542.   5560. -11718.   5443. 
#>  8 121815. -23423. -13403.  -8714.   3183. 
#>  9  96442. -17904. -13306.  -5513.   7043. 
#> 10  60801. -12077. -16888.   1918.   2769. 
#> # … with 161 more rows
```

**Step 5.** Run `ellipseParam()` for the first two principal components
(**k = 2**). We want to compute the length of the semi-axes of the
Hotelling ellipse (denoted **a** and **b**) when the first principal
component, PC1, is on the *x*-axis (**pcx = 1**) and, the second
principal component, PC2, is on the *y*-axis (**pcy = 2**).

``` r
res_2PCs <- ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)
```

``` r
str(res_2PCs)
#> List of 4
#>  $ Tsquare     : tibble [171 × 1] (S3: tbl_df/tbl/data.frame)
#>   ..$ value: num [1:171] 3.34 1.42 3.29 2.39 2.71 ...
#>  $ Ellipse     : tibble [1 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ a.99pct: num 310423
#>   ..$ b.99pct: num 91576
#>   ..$ a.95pct: num 249172
#>   ..$ b.95pct: num 73506
#>  $ cutoff.99pct: num 9.52
#>  $ cutoff.95pct: num 6.14
```

-   Semi-axes of the ellipse at 99% confidence level.

``` r
a1 <- pluck(res_2PCs, "Ellipse", "a.99pct")
b1 <- pluck(res_2PCs, "Ellipse", "b.99pct")
```

-   Semi-axes of the ellipse at 95% confidence level.

``` r
a2 <- pluck(res_2PCs, "Ellipse", "a.95pct")
b2 <- pluck(res_2PCs, "Ellipse", "b.95pct")
```

-   Hotelling’s T<sup>2</sup>.

``` r
T2 <- pluck(res_2PCs, "Tsquare", "value")
```

Another way to add Hotelling ellipse on the scatterplot of the scores is
to use the function `ellipseCoord()`. This function provides the *x* and
*y* coordinates of the confidence ellipse at user-defined confidence
interval. The confidence interval `conf.limit` is set at 95% by default.
Here, PC1 is on the *x*-axis (**pcx = 1**) and, the third principal
component, PC3, is on the *y*-axis (**pcy = 3**).

``` r
coord_2PCs_99 <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 3, conf.limit = 0.99, pts = 500)
coord_2PCs_95 <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 3, conf.limit = 0.95, pts = 500)
coord_2PCs_90 <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 3, conf.limit = 0.90, pts = 500)
```

``` r
str(coord_2PCs_99)
#> tibble [500 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ x: num [1:500] 310423 310398 310324 310201 310029 ...
#>  $ y: num [1:500] -1.06e-11 9.87e+02 1.97e+03 2.96e+03 3.95e+03 ...
```

**Step 6.** Plot PC1 *vs.* PC2 scatterplot, with the two corresponding
Hotelling ellipse. Points inside the two elliptical regions are within
the 99% and 95% confidence intervals for T<sup>2</sup>.

``` r
pca_scores %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), size = .5, linetype = "dotted", fill = "white") +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), size = .5, linetype = "dashed", fill = "white") +
  geom_point(aes(fill = T2), shape = 21, size = 3, color = "black") +
  scale_fill_viridis_c(option = "viridis") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .2) +
  labs(title = "Scatterplot of PCA scores", subtitle = "PC1 vs. PC2", x = "PC1", y = "PC2", fill = "T2", caption = "Figure 1: Hotelling's T2 ellipse obtained\n using the ellipseParam function") +
  theme_grey()
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="90%" height="90%" />

Or in the PC1-PC3 subspace at the confidence intervals set at 99, 95 and
90%.

``` r
ggplot() +
  geom_ellipse(data = coord_2PCs_99, aes(x0 = x, y0 = y, a = 1, b = 1, angle = 0), size = .9, color = "black", linetype = "dashed") +
  geom_ellipse(data = coord_2PCs_95, aes(x0 = x, y0 = y, a = 1, b = 1, angle = 0), size = .9, color = "darkred", linetype = "dotted") +
  geom_ellipse(data = coord_2PCs_90, aes(x0 = x, y0 = y, a = 1, b = 1, angle = 0), size = .9, color = "darkblue", linetype = "dotted") +
  geom_point(data = pca_scores, aes(x = Dim.1, y = Dim.3, fill = T2), shape = 21, size = 3, color = "black") +
  scale_fill_viridis_c(option = "viridis") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .2) +
  labs(title = "Scatterplot of PCA scores", subtitle = "PC1 vs. PC3", x = "PC1", y = "PC3", fill = "T2", caption = "Figure 2: Hotelling's T2 ellipse obtained\n using the ellipseCoord function") +
  theme_bw() +
  theme(panel.grid = element_blank())
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="90%" height="90%" />

**Note:** The easiest way to analyze and interpret Hotelling’s
T<sup>2</sup> for more than two principal components, is to plot
Hotelling’s T<sup>2</sup> *vs.* Observations, where the confidence
limits are plotted as a line. Thus, observations below the two lines are
within the T<sup>2</sup> limits. For example, `ellipseParam()` is used
with the first three principal components (**k = 3**).

``` r
res_3PCs <- ellipseParam(data = pca_scores, k = 3)
```

``` r
str(res_3PCs)
#> List of 3
#>  $ Tsquare     : tibble [171 × 1] (S3: tbl_df/tbl/data.frame)
#>   ..$ value: num [1:171] 2.215 0.941 2.18 1.586 1.798 ...
#>  $ cutoff.99pct: num 11.8
#>  $ cutoff.95pct: num 8.07
```

``` r
tibble(
  T2 = pluck(res_3PCs, "Tsquare", "value"), 
  obs = 1:nrow(pca_scores)
  ) %>%
  ggplot() +
  geom_point(aes(x = obs, y = T2, fill = T2), shape = 21, size = 3, color = "black") +
  geom_segment(aes(x = obs, y = T2, xend = obs, yend = 0), size = .5) +
  scale_fill_gradient(low = "black", high = "red", guide = "none") +
  geom_hline(yintercept = pluck(res_3PCs, "cutoff.99pct"), linetype = "dashed", color = "darkred", size = .5) +
  geom_hline(yintercept = pluck(res_3PCs, "cutoff.95pct"), linetype = "dashed", color = "darkblue", size = .5) +
  annotate("text", x = 160, y = 12.4, label = "99% limit", color = "darkred") +
  annotate("text", x = 160, y = 8.6, label = "95% limit", color = "darkblue") +
  labs(x = "Observations", y = "Hotelling’s T-square (3 PCs)", fill = "T2 stats", caption = "Figure 3: Hotelling’s T2-value vs. Observations") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="90%" height="90%" />
