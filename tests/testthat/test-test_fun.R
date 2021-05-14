test_that("function stops when it should", {
  expect_error(ellipseParam(data = 0, k = 0, pcx = 0, pcy = 0))
  expect_error(ellipseCoord(data = 0, pcx = 0, pcy = 0, conf.limit = 0, pts = 0))
})

library(dplyr)
set.seed(123)
pca_mod <- specData %>%
  select(where(is.numeric)) %>%
  FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)

pca_scores <- pca_mod %>%
  purrr::pluck("ind", "coord") %>%
  tibble::as_tibble()


test_that("ellipseParam: input data", {
  pcscores <- tibble::tibble()
  expect_error(ellipseParam(data = pcscores), "Seems you forgot to provide data values.")
})


test_that("ellipseParam and ellipseCoord functions: k equal to 2", {

  res <- ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)
  xy_coord <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200)

  expect_error(ellipseParam(data = pca_scores, pcx = 1, pcy = 1), "Please provide two different components in pcx and pcy.")
  expect_error(ellipseParam(data = pca_scores, pcx = 0, pcy = 1), "No component is provided either in pcx or pcy, or both.")
  expect_error(ellipseCoord(data = pca_scores, pcx = 0, pcy = 1), "No component is provided either in pcx or pcy, or both.")
  expect_error(ellipseParam(data = pca_scores, k = 1), "k must be at least equal to 2.")
  expect_error(ellipseParam(data = pca_scores, k = ncol(pca_scores)+1), "k exceeds the number of component in the data.")
  expect_error(ellipseCoord(data = pca_scores, pcx = 0, pcy = 1), "No component is provided either in pcx or pcy, or both.")
  expect_error(ellipseCoord(data = pca_scores, conf.limit = 2), "Confidence level should be between 0 and 1")
  expect_type(res, "list")
  expect_type(xy_coord, "list")
  expect_named(res, c("Tsquared", "Ellipse", "cutoff.99pct", "cutoff.95pct"), ignore.order = TRUE, ignore.case = TRUE)
  expect_named(xy_coord, c("x", "y"), ignore.order = TRUE, ignore.case = TRUE)
  expect_output(str(res), "$ a.99pct", fixed = TRUE)
  expect_output(str(res), "$ b.99pct", fixed = TRUE)
  expect_output(str(res), "$ a.95pct", fixed = TRUE)
  expect_output(str(res), "$ b.95pct", fixed = TRUE)
  expect_equal(nrow(res$Tsquared), nrow(pca_scores))
  })


test_that("ellipseParam function: k more than 2", {

  res1 <- ellipseParam(data = pca_scores, k = 3, pcx = 1, pcy = 2)

  expect_error(ellipseParam(data = pca_scores, pcx = 1, pcy = 1), "Please provide two different components in pcx and pcy.")
  expect_error(ellipseParam(data = pca_scores, pcx = 0, pcy = 1), "No component is provided either in pcx or pcy, or both.")
  expect_error(ellipseParam(data = pca_scores, k = 1), "k must be at least equal to 2.")
  expect_error(ellipseParam(data = pca_scores, k = ncol(pca_scores)+1), "k exceeds the number of component in the data.")
  expect_type(res1, "list")
  expect_named(res1, c("Tsquared", "cutoff.99pct", "cutoff.95pct"), ignore.order = TRUE, ignore.case = TRUE)
  expect_equal(nrow(res1$Tsquared), nrow(pca_scores))
})


