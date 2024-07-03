test_that("ellipseCoord function works correctly", {
  data("specData", package = "HotellingEllipse")
  set.seed(123)
  pca_mod <- specData %>%
    select(where(is.numeric)) %>%
    FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)

  test_data <- pca_mod$ind$coord %>% as.data.frame()

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
