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

res_t2 <- ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)
xy_coord <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200)


test_that("input errors", {
  expect_error(ellipseParam(data = pca_scores, pcx = 1, pcy = 1), "Please provide two different components in pcx and pcy.")
  expect_error(ellipseParam(data = pca_scores, pcx = 0, pcy = 1), "No component is provided either in pcx or pcy, or both.")
  expect_error(ellipseCoord(data = pca_scores, pcx = 0, pcy = 1), "No component is provided either in pcx or pcy, or both.")
  expect_error(ellipseParam(data = pca_scores, k = 1), "k must be at least equal to 2.")
  expect_error(ellipseParam(data = pca_scores, k = ncol(pca_scores)+1), "k exceeds the number of component in the data.")
  expect_error(ellipseCoord(data = pca_scores, pcx = 0, pcy = 1), "No component is provided either in pcx or pcy, or both.")
  expect_error(ellipseCoord(data = pca_scores, conf.limit = 2), "Confidence level should be between 0 and 1")
})

test_that("ellipseParam and ellipseCoord functions", {
  expect_type(res_t2, "list")
  expect_type(xy_coord, "list")
  expect_named(res_t2, c("Tsquared", "Ellipse", "cutoff.99pct", "cutoff.95pct"), ignore.order = TRUE, ignore.case = TRUE)
  expect_named(xy_coord, c("x", "y"), ignore.order = TRUE, ignore.case = TRUE)
  expect_output(str(res_t2), "$ a.99pct", fixed = TRUE)
  expect_output(str(res_t2), "$ b.99pct", fixed = TRUE)
  expect_output(str(res_t2), "$ a.95pct", fixed = TRUE)
  expect_output(str(res_t2), "$ b.95pct", fixed = TRUE)
  expect_equal(nrow(res_t2$Tsquared), nrow(pca_scores))

  })




