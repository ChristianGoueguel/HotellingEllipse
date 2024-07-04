#' @title Coordinate Points of the Hotelling's T-squared Ellipse
#'
#' @author Christian L. Goueguel <christian.goueguel@gmail.com>
#'
#' @description
#' This function calculates the coordinate points for drawing a Hotelling's T-squared
#' ellipse based on multivariate data. It can generate points for both 2D and 3D ellipses.
#'
#' @param x A matrix, data frame or tibble containing scores from PCA, PLS, ICA, or other dimensionality reduction methods. Each column should represent a component, and each row an observation.
#' @param pcx An integer specifying which component to use for the x-axis (default is 1).
#' @param pcy An integer specifying which component to use for the y-axis (default is 2).
#' @param pcz An integer specifying which component to use for the z-axis for 3D ellipsoids. If `NULL` (default), a 2D ellipse is computed.
#' @param conf.limit A numeric value between 0 and 1 specifying the confidence level for the ellipse (default is 0.95, i.e., 95% confidence).
#' @param pts An integer specifying the number of points to generate for drawing the ellipse (default is 200). Higher values result in smoother ellipses.
#'
#' @return A data frame containing the coordinate points of the Hotelling's T-squared ellipse:
#' \itemize{
#'   \item For 2D ellipses: columns `x` and `y`
#'   \item For 3D ellipsoids: columns `x`, `y`, and `z`
#' }
#'
#' @details
#' The function computes the shape and orientation of the ellipse based on the
#' Hotelling's T-squared distribution and the specified components. It then generates a set of
#' points that lie on the ellipse's surface at the specified confidence level.
#' For 2D ellipses, the function uses two components `pcx`
#' and `pcy`. For 3D ellipsoids, it uses three components `pcx`, `pcy`, and `pcz`.
#' The `conf.limit` parameter determines the size of the ellipse. A higher confidence
#' level results in a larger ellipse that encompasses more data points.
#'
#' @export ellipseCoord
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(HotellingEllipse)
#' library(dplyr)
#'
#' data("specData", package = "HotellingEllipse")
#'
#' # Perform PCA
#' set.seed(123)
#' pca_mod <- specData %>%
#'   select(where(is.numeric)) %>%
#'   FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)
#'
#' # Extract PCA scores
#' pca_scores <- pca_mod$ind$coord %>% as.data.frame()
#'
#' # Example 1: Calculate Hotelling's T-squared ellipse coordinates
#' xy_coord <- ellipseCoord(pca_scores, pcx = 1, pcy = 2)
#'
#' # Example 2: Calculate Hotelling's T-squared ellipsoid coordinates
#' xyz_coord <- ellipseCoord(pca_scores, pcx = 1, pcy = 2, pcz = 3)
#' }
#'
ellipseCoord <- function(x, pcx = 1, pcy = 2, pcz = NULL, conf.limit = 0.95, pts = 200) {

  if (missing(x)) {
    stop("Missing input data.")
  }
  if (!is.matrix(x) && !is.data.frame(x) && !tibble::is_tibble(x)) {
    stop("The input data must be a matrix, data frame or tibble.")
  }
  if (!is.numeric(conf.limit) || conf.limit <= 0 || conf.limit >= 1) {
    stop("Confidence level should be a numeric value between 0 and 1.")
  }

  x <- as.matrix(x)
  p <- as.integer(ncol(x))
  n <- nrow(x)

  if (!is_integer(pcx) || pcx < 1L || pcx > p) {
    stop(sprintf("'pcx' must be an integer between 1 and the number of components in the data (%d).", p))
  }
  if (!is_integer(pcy) || pcy < 1L || pcy > p) {
    stop(sprintf("'pcy' must be an integer between 1 and the number of components in the data (%d).", p))
  }
  if (pcx == pcy) {
    stop("'pcx' and 'pcy' must be different integers.")
  }
  if (!is_integer(pts) || pts <= 0) {
    stop("'pts' should be a positive integer.")
  }
  if (!is.null(pcz)) {
    if (!is_integer(pcz) || pcz < 1L || pcz > p) {
      stop(sprintf("'pcz' must be an integer between 1 and the number of components in the data (%d).", p))
    }
    if (pcz == pcx || pcz == pcy) {
      stop("'pcx', 'pcy' and 'pcz' must be different integers.")
    }
  }

  if (is.null(pcz)) {
    res_coord <- computeEllipse(x, pcx, pcy, n, conf.limit, pts)
  } else {
    res_coord <- computeEllipsoid(x, pcx, pcy, pcz, n, conf.limit, pts)
  }

  return(res_coord)
}



is_integer <- function(x) {
  x == as.integer(x)
  }


computeEllipse <- function(x, pcx, pcy, n, conf.limit, pts) {
  theta <- seq(0, 2 * pi, length.out = pts)
  p <- 2
  Tsq_limit <- ((p * (n - 1)) / (n - p)) * stats::qf(p = conf.limit, df1 = p, df2 = (n - p))
  x_col <- x[, pcx, drop = TRUE]
  y_col <- x[, pcy, drop = TRUE]
  res <- tibble::tibble(
    x = sqrt(Tsq_limit * as.numeric(stats::var(x_col))) * cos(theta) + as.numeric(mean(x_col)),
    y = sqrt(Tsq_limit * as.numeric(stats::var(y_col))) * sin(theta) + as.numeric(mean(y_col))
  )
  return(res)
}



computeEllipsoid <- function(x, pcx, pcy, pcz, n, conf.limit, pts) {
  theta <- seq(0, 2 * pi, length.out = pts)
  phi <- seq(0, pi, length.out = pts)
  grid <- expand.grid(theta = theta, phi = phi)
  sin_phi <- sin(grid$phi)
  cos_phi <- cos(grid$phi)
  cos_theta <- cos(grid$theta)
  sin_theta <- sin(grid$theta)
  p <- 3
  Tsq_limit <- ((p * (n - 1)) / (n - p)) * stats::qf(p = conf.limit, df1 = p, df2 = (n - p))
  x_col <- x[, pcx, drop = TRUE]
  y_col <- x[, pcy, drop = TRUE]
  z_col <- x[, pcz, drop = TRUE]
  res <- tibble::tibble(
    x = sqrt(Tsq_limit * as.numeric(stats::var(x_col))) * cos_theta * sin_phi + as.numeric(mean(x_col)),
    y = sqrt(Tsq_limit * as.numeric(stats::var(y_col))) * sin_theta * sin_phi + as.numeric(mean(y_col)),
    z = sqrt(Tsq_limit * as.numeric(stats::var(z_col))) * cos_phi + as.numeric(mean(z_col))
  )
  return(res)
}

