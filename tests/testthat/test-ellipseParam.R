test_that("ellipseParam function works correctly", {
  data("specData", package = "HotellingEllipse")
  set.seed(123)
  pca_mod <- specData %>%
    select(where(is.numeric)) %>%
    FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)

  test_data <- pca_mod$ind$coord %>% as.data.frame()

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
  expect_named(result, c("Tsq", "Tsq_limit1", "Tsq_limit2"))
  expect_s3_class(result$Tsq, "tbl_df")
  expect_true(is.numeric(result$Tsq_limit1))
  expect_true(is.numeric(result$Tsq_limit2))
})
