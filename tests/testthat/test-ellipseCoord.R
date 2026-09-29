test_that("ellipseCoord function works correctly", {
  data("specData", package = "HotellingEllipse")
  set.seed(123)
  test_data <- as.data.frame(stats::prcomp(specData, rank. = 5)$x)

  # Test 1: Function runs without errors for valid input
  expect_no_error(ellipseCoord(test_data))

  # Test 2: Function returns expected structure
  result <- ellipseCoord(test_data)
  expect_true(is.list(result))
  expect_named(result, c("x", "y"))
  expect_equal(length(result$x), 200)
  expect_equal(length(result$y), 200)

  # Test 3: Error for missing input data
  expect_error(ellipseCoord(), "Missing input data.")

  # Test 4: Error for invalid input data type
  s <- seq(1, 100)
  expect_error(ellipseCoord(s), "The input data must be a matrix, data frame or tibble.")

  # Test 5: Function accepts data frame, tibble, and matrix
  expect_no_error(ellipseCoord(as.data.frame(test_data)))
  expect_no_error(ellipseCoord(tibble::as_tibble(test_data)))
  expect_no_error(ellipseCoord(as.matrix(test_data)))

  # Test 6: Error for invalid confidence limit
  expect_error(ellipseCoord(test_data, conf.limit = 1.5), "Confidence level should be a numeric value between 0 and 1.")
  expect_error(ellipseCoord(test_data, conf.limit = 0), "Confidence level should be a numeric value between 0 and 1.")

  # Test 7: Error for invalid pcx
  expect_error(ellipseCoord(test_data, pcx = 0), "'pcx' must be an integer between 1 and the number of components in the data")
  expect_error(ellipseCoord(test_data, pcx = 200), "'pcx' must be an integer between 1 and the number of components in the data")

  # Test 8: Error for invalid pcy
  expect_error(ellipseCoord(test_data, pcy = 0), "'pcy' must be an integer between 1 and the number of components in the data")
  expect_error(ellipseCoord(test_data, pcy = 200), "'pcy' must be an integer between 1 and the number of components in the data")

  # Test 9: Error when pcx equals pcy
  expect_error(ellipseCoord(test_data, pcx = 1, pcy = 1), "'pcx' and 'pcy' must be different integers.")

  # Test 10: Error for invalid pts
  expect_error(ellipseCoord(test_data, pts = 0), "'pts' should be a positive integer.")
  expect_error(ellipseCoord(test_data, pts = -10), "'pts' should be a positive integer.")

  # Test 11: Function works with valid pcz
  expect_no_error(ellipseCoord(test_data, pcx = 1, pcy = 2, pcz = 3))

  # Test 12: Error for invalid pcz
  expect_error(ellipseCoord(test_data, pcz = 0), "'pcz' must be an integer between 1 and the number of components in the data")
  expect_error(ellipseCoord(test_data, pcz = 200), "'pcz' must be an integer between 1 and the number of components in the data")

  # Test 13: Error when pcz equals pcx or pcy
  expect_error(ellipseCoord(test_data, pcx = 1, pcy = 2, pcz = 1), "'pcx', 'pcy' and 'pcz' must be different integers.")
  expect_error(ellipseCoord(test_data, pcx = 1, pcy = 2, pcz = 2), "'pcx', 'pcy' and 'pcz' must be different integers.")
})

test_that("ellipse and ellipsoid points lie on the T-squared limit", {
  set.seed(123)
  z <- matrix(rnorm(600), ncol = 3)
  test_data <- z %*% matrix(c(1, 0.5, 0.2, 0, 1, 0.4, 0, 0, 1), 3)
  n <- nrow(test_data)

  xy <- ellipseCoord(test_data, pcx = 1, pcy = 3, conf.limit = 0.9, method = "beta")
  d2 <- test_data[, c(1, 3)]
  md <- stats::mahalanobis(as.matrix(xy), colMeans(d2), stats::cov(d2))
  expect_equal(md, rep((n - 1)^2 / n * stats::qbeta(0.9, 1, (n - 3) / 2), nrow(xy)))

  xyz <- ellipseCoord(test_data, pcz = 3, pts = 20)
  md3 <- stats::mahalanobis(as.matrix(xyz), colMeans(test_data), stats::cov(test_data))
  expect_equal(md3, rep(3 * (n - 1) / (n - 3) * stats::qf(0.95, 3, n - 3), nrow(xyz)))
})

test_that("uncorrelated scores give the 1.2.0 axis-aligned ellipse", {
  set.seed(123)
  pca <- stats::prcomp(matrix(rnorm(300), ncol = 3))$x
  xy <- ellipseCoord(pca, pcx = 2, pcy = 3, pts = 50)
  n <- nrow(pca)
  lim <- 2 * (n - 1) / (n - 2) * stats::qf(0.95, 2, n - 2)
  theta <- seq(0, 2 * pi, length.out = 50)
  expect_equal(xy$x, sqrt(lim * stats::var(pca[, 2])) * cos(theta) + mean(pca[, 2]), tolerance = 1e-8)
  expect_equal(xy$y, sqrt(lim * stats::var(pca[, 3])) * sin(theta) + mean(pca[, 3]), tolerance = 1e-8)
})
