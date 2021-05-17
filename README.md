
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

# HotellingEllipse <img src='man/figures/logo.png' align="right" height="159" />

[![R-CMD-check](https://github.com/ChristianGoueguel/HotellingEllipse/workflows/R-CMD-check/badge.svg)](https://github.com/ChristianGoueguel/HotellingEllipse/actions)
[![codecov](https://codecov.io/gh/ChristianGoueguel/HotellingEllipse/branch/master/graph/badge.svg?token=99T5QAZDZ4)](https://codecov.io/gh/ChristianGoueguel/HotellingEllipse)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/HotellingEllipse)](https://CRAN.R-project.org/package=HotellingEllipse)
<!-- badges: end -->

HotellingEllipse computes the lengths of the semi-minor and semi-major
axes for plotting Hotelling ellipse at 95% and 99% confidence intervals.
The package also provides the *x*-*y* coordinates at user-defined
confidence intervals.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ChristianGoueguel/HotellingEllipse")
```

## Usage

Below is an overview of how HotellingEllipse can help draw a confidence
ellipse:

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
#> # A tibble: 171 x 5
#>      Dim.1   Dim.2   Dim.3   Dim.4   Dim.5
#>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 144168. -36399.   2228.   -670.  13805.
#>  2 118520. -31465.  16300. -20686. -13872.
#>  3  90303. -28356.  31340. -60615.  15157.
#>  4 107107. -38209.  24897. -60366.  19449.
#>  5  74350.  -2148.  29814.  -8351.    494.
#>  6  97511. -17932.  22254. -15406.  -4195.
#>  7  82142.  19297. -34299. -12498.   -648.
#>  8  76261.  16566. -34382. -16293.    137.
#>  9  73705.  31091. -22577. -17182.   2438.
#> 10  68042.  25124. -26063. -19389.   6051.
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
#>  $ Tsquare     : tibble[,1] [171 × 1] (S3: tbl_df/tbl/data.frame)
#>   ..$ value: num [1:171] 2.28 2.65 8 8.63 1.05 ...
#>  $ Ellipse     : tibble[,4] [1 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ a.99pct: num 319536
#>   ..$ b.99pct: num 91816
#>   ..$ a.95pct: num 256487
#>   ..$ b.95pct: num 73699
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
interval. The confidence interval `confi.limit` is set at 95% by
default.

``` r
coord_2PCs <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 500)
```

``` r
str(coord_2PCs)
#> tibble[,2] [500 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ x: num [1:500] 256487 256466 256405 256304 256161 ...
#>  $ y: num [1:500] 7.20e-12 9.28e+02 1.86e+03 2.78e+03 3.71e+03 ...
```

**Step 6.** Plot PC1 *vs.* PC2 scatterplot, with the two corresponding
Hotelling ellipse. Points inside the two elliptical regions are within
the 99% and 95% confidence limits for T<sup>2</sup>.

``` r
pca_scores %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(fill = T2), shape = 21, size = 3, color = "black") +
  scale_fill_viridis_c(option = "viridis") +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), size = .5, linetype = "dotted") + 
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), size = .5, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .2) +
  labs(title = "Scatterplot of PCA scores", subtitle = "PC1 vs. PC2", x = "PC1", y = "PC2", fill = "T2", caption = "Figure 1") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="90%" height="90%" />

**Note:** The easiest way to analyze and interpret Hotelling’s
T<sup>2</sup> for more than two principal components, is to plot
Observations *vs.* Hotelling’s T<sup>2</sup> where the confidence limits
are plotted as a line. Thus, observations below the two lines are within
the T<sup>2</sup> limits. For example, below, `ellipseParam()` is used
with the first three principal components (**k = 3**).

``` r
res_3PCs <- ellipseParam(data = pca_scores, k = 3)
```

``` r
str(res_3PCs)
#> List of 3
#>  $ Tsquare     : tibble[,1] [171 × 1] (S3: tbl_df/tbl/data.frame)
#>   ..$ value: num [1:171] 1.51 1.757 5.299 5.722 0.697 ...
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
  labs(x = "Observations", y = "Hotelling’s T-square (3 PCs)", fill = "T2 stats", caption = "Figure 2") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="90%" height="90%" />
