test_that("ellipseParam function works correctly", {
  data("specData", package = "HotellingEllipse")
  set.seed(123)
  test_data <- as.data.frame(stats::prcomp(specData, rank. = 5)$x)

  # Test 1: Function runs without errors for valid input
  expect_no_error(ellipseParam(test_data))

  # Test 2: Function returns expected structure
  result <- ellipseParam(test_data)
  expect_type(result, "list")
  expect_named(result, c("Tsquare", "cutoff.99pct", "cutoff.95pct", "nb.comp", "Ellipse"))

  # Test 3: Error for missing input data
  expect_error(ellipseParam(), "Missing input data.")

  # Test 4: Error for invalid input data type
  s <- seq(1, 100)
  expect_error(ellipseParam(s), "The input data must be a matrix, data frame or tibble.")

  # Test 5: Function accepts data frame, tibble, and matrix
  expect_no_error(ellipseCoord(as.data.frame(test_data)))
  expect_no_error(ellipseCoord(tibble::as_tibble(test_data)))
  expect_no_error(ellipseCoord(as.matrix(test_data)))

  # Test 6: Error for invalid rel.tol
  expect_error(ellipseParam(test_data, rel.tol = -0.1), "'rel.tol' must be a non-negative numeric value.")

  # Test 7: Error for invalid abs.tol
  expect_error(ellipseParam(test_data, abs.tol = -0.1), "'abs.tol' must be a non-negative numeric value.")

  # Test 8: Error when abs.tol > rel.tol
  expect_error(ellipseParam(test_data, rel.tol = 0.001, abs.tol = 0.01), "'abs.tol' must be less than or equal to 'rel.tol'.")

  # Test 9: Error for invalid threshold
  expect_error(ellipseParam(test_data, threshold = 1.5), "Threshold must be a numeric value between 0 and 1.")
  expect_error(ellipseParam(test_data, threshold = 0), "Threshold must be a numeric value between 0 and 1.")

  # Test 10: Error for invalid k
  expect_error(ellipseParam(test_data, k = 1), "'k' must be an integer between 2 and the number of components in the data")
  expect_error(ellipseParam(test_data, k = 0), "'k' must be an integer between 2 and the number of components in the data")

  # Test 11: Error for invalid pcx
  expect_error(ellipseParam(test_data, pcx = 0), "'pcx' must be an integer between 1 and the number of components in the data")
  expect_error(ellipseParam(test_data, pcx = 200), "'pcx' must be an integer between 1 and the number of components in the data")

  # Test 12: Error for invalid pcy
  expect_error(ellipseParam(test_data, pcy = 0), "'pcy' must be an integer between 1 and the number of components in the data")
  expect_error(ellipseParam(test_data, pcy = 200), "'pcy' must be an integer between 1 and the number of components in the data")

  # Test 13: Error when pcx equals pcy
  expect_error(ellipseParam(test_data, pcx = 1, pcy = 1), "'pcx' and 'pcy' must be different integers.")

  # Test 14: Function works with valid threshold
  expect_no_error(ellipseParam(test_data, threshold = 0.95))

  # Test 15: Warning when threshold is too low
  expect_warning(ellipseParam(test_data, threshold = 0.1), "The specified threshold .* is lower than the variance explained by the first component")
})

test_that("process_fixed_comp function works correctly", {
  set.seed(123)
  test_data <- matrix(rnorm(300), ncol = 3)
  colnames(test_data) <- c("Comp1", "Comp2", "Comp3")

  comp_var <- apply(test_data, 2, stats::var)
  total_var <- sum(comp_var)
  relative_var <- comp_var / total_var
  nearzero_var <- relative_var < 0.001

  result <- process_fixed_comp(test_data, k = 2, pcx = 1, pcy = 2, nearzero_var, comp_var, relative_var, rel.tol = 0.001)

  expect_type(result, "list")
  expect_named(result, c("Tsquare", "cutoff.99pct", "cutoff.95pct", "nb.comp", "Ellipse"))
  expect_equal(result$nb.comp, 2)
  expect_s3_class(result$Ellipse, "tbl_df")
})

test_that("process_threshold function works correctly", {
  set.seed(123)
  test_data <- matrix(rnorm(300), ncol = 3)
  colnames(test_data) <- c("Comp1", "Comp2", "Comp3")

  relative_var <- apply(test_data, 2, stats::var) / sum(apply(test_data, 2, stats::var))
  nearzero_var <- relative_var < 0.001

  result <- process_threshold(test_data, threshold = 0.95, nearzero_var, relative_var)

  expect_type(result, "list")
  expect_named(result, c("Tsquare", "cutoff.99pct", "cutoff.95pct", "nb.comp"))
  expect_true(result$nb.comp >= 1 && result$nb.comp <= ncol(test_data))
})

test_that("compute_tsquared function works correctly", {
  set.seed(123)
  test_data <- matrix(rnorm(300), ncol = 3)
  result <- compute_tsquared(test_data, ncomp = 2)

  expect_type(result, "list")
  expect_named(result, c("Tsq", "Tsq_limits"))
  expect_s3_class(result$Tsq, "tbl_df")
  expect_named(result$Tsq_limits, c("99pct", "95pct"))
})

test_that("T-squared values are on the same scale as the cutoffs", {
  set.seed(123)
  test_data <- matrix(rnorm(150), ncol = 3)
  result <- compute_tsquared(test_data, ncomp = 2)
  x2 <- test_data[, 1:2]
  expected <- stats::mahalanobis(x2, colMeans(x2), stats::cov(x2))
  expect_equal(result$Tsq$value, unname(expected))

  n <- nrow(test_data)
  expect_equal(result$Tsq_limits[["95pct"]], 2 * (n - 1) / (n - 2) * stats::qf(0.95, 2, n - 2))
  result_beta <- compute_tsquared(test_data, ncomp = 2, method = "beta")
  expect_equal(result_beta$Tsq_limits[["95pct"]], (n - 1)^2 / n * stats::qbeta(0.95, 1, (n - 3) / 2))
})

test_that("T-squared uses pcx and pcy when k = 2", {
  set.seed(123)
  test_data <- matrix(rnorm(400), ncol = 4)
  # Make the columns uncorrelated, as PCA scores are
  test_data <- prcomp(test_data)$x
  result <- ellipseParam(test_data, pcx = 2, pcy = 4)
  x2 <- test_data[, c(2, 4)]
  expected <- stats::mahalanobis(x2, colMeans(x2), stats::cov(x2))
  expect_equal(result$Tsquare$value, unname(expected))

  # Points outside the 95% ellipse are exactly those above the 95% cutoff
  xc <- sweep(x2, 2, colMeans(x2))
  outside <- (xc[, 1] / result$Ellipse$a.95pct)^2 + (xc[, 2] / result$Ellipse$b.95pct)^2 > 1
  expect_equal(outside, result$Tsquare$value > result$cutoff.95pct, ignore_attr = TRUE)
})

test_that("method selects the T-squared limit", {
  set.seed(123)
  test_data <- stats::prcomp(matrix(rnorm(400), ncol = 4))$x
  n <- nrow(test_data)
  beta <- ellipseParam(test_data, method = "beta")
  f <- ellipseParam(test_data)
  expect_equal(beta$cutoff.99pct, (n - 1)^2 / n * stats::qbeta(0.99, 1, (n - 3) / 2))
  expect_equal(f$cutoff.99pct, 2 * (n - 1) / (n - 2) * stats::qf(0.99, 2, n - 2))
  expect_equal(beta$Tsquare, f$Tsquare)
  expect_error(ellipseParam(test_data, method = "chisq"), "should be one of")

  # The beta limit never exceeds the largest attainable T-squared, (n - 1)^2 / n
  small <- test_data[1:10, ]
  expect_lt(ellipseParam(small, method = "beta")$cutoff.99pct, (10 - 1)^2 / 10)
})

test_that("the ellipse matches T-squared for correlated scores", {
  set.seed(123)
  z <- matrix(rnorm(400), ncol = 2)
  test_data <- cbind(z[, 1], 0.8 * z[, 1] + 0.6 * z[, 2])
  result <- ellipseParam(test_data)
  expect_true(abs(result$Ellipse$angle) > 0.1)

  ax <- result$Ellipse
  xc <- sweep(test_data, 2, colMeans(test_data))
  u <- cos(ax$angle) * xc[, 1] + sin(ax$angle) * xc[, 2]
  v <- -sin(ax$angle) * xc[, 1] + cos(ax$angle) * xc[, 2]
  outside <- (u / ax$a.95pct)^2 + (v / ax$b.95pct)^2 > 1
  expect_equal(outside, result$Tsquare$value > result$cutoff.95pct)

  # Uncorrelated scores give an axis-aligned ellipse with the 1.2.0 semi-axes formula
  pca <- stats::prcomp(test_data)$x
  res_pca <- ellipseParam(pca)
  expect_identical(res_pca$Ellipse$angle, 0)
  expect_equal(res_pca$Ellipse$a.95pct, sqrt(res_pca$cutoff.95pct * stats::var(pca[, 1])))
  expect_equal(res_pca$Ellipse$b.95pct, sqrt(res_pca$cutoff.95pct * stats::var(pca[, 2])))
})

test_that("invalid scalar arguments give informative errors", {
  set.seed(123)
  test_data <- matrix(rnorm(300), ncol = 3)
  expect_error(ellipseParam(test_data, k = NA), "'k' must be an integer")
  expect_error(ellipseParam(test_data, k = 2.5), "'k' must be an integer")
  expect_error(ellipseParam(test_data, pcx = c(1, 2)), "'pcx' must be an integer")
  expect_error(ellipseParam(test_data, threshold = NA), "Threshold must be a numeric value")
  expect_error(ellipseParam(test_data, rel.tol = NA), "'rel.tol' must be a non-negative")
  expect_error(ellipseParam(test_data[1:3, ]), "At least 4 observations")
})

test_that("threshold path handles edge cases", {
  set.seed(123)
  test_data <- stats::prcomp(matrix(rnorm(400), ncol = 4))$x
  res <- ellipseParam(test_data, threshold = 1)
  expect_identical(res$nb.comp, 4L)

  # A near-zero component inside the selected range is removed
  test_data[, 2] <- test_data[, 2] * 1e-4
  expect_warning(res <- ellipseParam(test_data, threshold = 1), "removed")
  expect_identical(res$nb.comp, 3L)
})

test_that("equal-variance uncorrelated scores give angle 0", {
  # e.g. SIMPLS scores, which are scaled to equal variance: the ellipse is a circle
  set.seed(123)
  pca <- stats::prcomp(matrix(rnorm(200), ncol = 2))$x
  pca <- sweep(pca, 2, apply(pca, 2, stats::sd), "/")
  res <- ellipseParam(pca)
  expect_identical(res$Ellipse$angle, 0)
  expect_equal(res$Ellipse$a.95pct, res$Ellipse$b.95pct)
})

test_that("conf.limit sets the cutoff and semi-axis levels", {
  set.seed(123)
  test_data <- stats::prcomp(matrix(rnorm(400), ncol = 4))$x
  n <- nrow(test_data)
  f_limit <- function(level, k = 2) k * (n - 1) / (n - k) * stats::qf(level, k, n - k)

  # Default output is unchanged
  res <- ellipseParam(test_data)
  expect_identical(res, ellipseParam(test_data, conf.limit = c(0.95, 0.99)))
  expect_identical(res, ellipseParam(test_data, conf.limit = c(0.99, 0.95)))
  expect_named(res, c("Tsquare", "cutoff.99pct", "cutoff.95pct", "nb.comp", "Ellipse"))
  expect_named(res$Ellipse, c("a.99pct", "b.99pct", "a.95pct", "b.95pct", "angle"))

  # Custom levels are named after the level, from highest to lowest
  res <- ellipseParam(test_data, conf.limit = c(0.975, 0.999))
  expect_named(res, c("Tsquare", "cutoff.99.9pct", "cutoff.97.5pct", "nb.comp", "Ellipse"))
  expect_named(res$Ellipse, c("a.99.9pct", "b.99.9pct", "a.97.5pct", "b.97.5pct", "angle"))
  expect_equal(res$cutoff.97.5pct, f_limit(0.975))
  expect_equal(res$Ellipse$a.99.9pct, sqrt(f_limit(0.999) * stats::var(test_data[, 1])))
  expect_equal(res$Ellipse$b.97.5pct, sqrt(f_limit(0.975) * stats::var(test_data[, 2])))

  # A single level, and more than two levels
  res <- ellipseParam(test_data, conf.limit = 0.9)
  expect_named(res, c("Tsquare", "cutoff.90pct", "nb.comp", "Ellipse"))
  expect_named(res$Ellipse, c("a.90pct", "b.90pct", "angle"))
  res <- ellipseParam(test_data, k = 3, conf.limit = c(0.9, 0.95, 0.99))
  expect_named(res, c("Tsquare", "cutoff.99pct", "cutoff.95pct", "cutoff.90pct", "nb.comp"))
  expect_equal(res$cutoff.90pct, f_limit(0.9, k = 3))
  res <- ellipseParam(test_data, threshold = 0.9, conf.limit = 0.975, method = "beta")
  expect_named(res, c("Tsquare", "cutoff.97.5pct", "nb.comp"))

  # Invalid values
  expect_error(ellipseParam(test_data, conf.limit = 1), "'conf.limit' must be a numeric vector")
  expect_error(ellipseParam(test_data, conf.limit = c(0.95, NA)), "'conf.limit' must be a numeric vector")
  expect_error(ellipseParam(test_data, conf.limit = numeric(0)), "'conf.limit' must be a numeric vector")
  expect_error(ellipseParam(test_data, conf.limit = "0.95"), "'conf.limit' must be a numeric vector")
  expect_error(ellipseParam(test_data, conf.limit = c(0.95, 0.95)), "must not contain duplicated values")
})
