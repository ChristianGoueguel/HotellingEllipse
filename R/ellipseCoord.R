#' Coordinates for Hotelling's confidence ellipse
#' @description This function produces coordinates values for plotting confidence ellipse.
#' @title T-squared Confidence Ellipse Coordinates
#' @param data a data frame or tibble of scores coordinates
#' @param pcx an integer specifying which component is on the x-axis (by default 1)
#' @param pcy an integer specifying which component is on the y-axis (by default 2)
#' @param conf.limit an integer specifying the confidence level (by default 0.95)
#' @param pts an integer indicating the number of points in the ellipse (by default 200)
#'
#' @return a data frame with x and y coordinates
#'
#' @export ellipseCoord
#'
#' @author Christian L. Goueguel,
#' christian.goueguel@gmail.com
#'
#' @examples
#' ## Principal components analysis (PCA)
#' library(dplyr)
#' set.seed(123)
#' pca_mod <- specData %>%
#'   dplyr::select(where(is.numeric)) %>%
#'   FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)
#'
#' ## Extract PCA scores
#' pca_scores <- pca_mod %>%
#'    purrr::pluck("ind", "coord") %>%
#'    tibble::as_tibble()
#'
#' ## Compute Hotelling T-squared statistic and ellipse parameters
#' library(HotellingEllipse)
#' xy_coord <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200)
#'
ellipseCoord <- function(data, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200) {

  stopifnot(length(data) != 0)
  stopifnot(pcx != pcy)

  if (is.data.frame(data) == FALSE | tibble::is_tibble(data) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }

  if (pcx == 0 | pcy == 0) {
    stop("No component is provided either in pcx or pcy, or both.")
  }

  if(conf.limit < 0 | conf.limit > 1) {
    stop("Confidence level should be between 0 and 1")
  }

  # matrix of data
  X <- as.matrix(data)

  # Sample size
  n <- nrow(X)

  # Confidence limit
  alpha <- as.numeric(conf.limit)

  # Number of points
  m <- as.numeric(pts)
  p <- seq(0, 2*pi, length = m)

  # Confidence limit for Hotelling’s T-squared
  Tsq_limit <- (2*(n-1)/(n-2))*stats::qf(p = alpha, df1 = 2, df2 = (n-2))

  # Coordinate points
  rx <- sqrt(Tsq_limit*stats::var(X[, pcx]))
  ry <- sqrt(Tsq_limit*stats::var(X[, pcy]))

  res.coord <- tibble::tibble(
    x = rx*cos(p) + mean(X[, pcx], na.rm = TRUE),
    y = ry*sin(p) + mean(X[, pcy], na.rm = TRUE)
    )

  return(res.coord)

}
