#' @title Coordinate Points Of Hotelling Ellipse
#' @description Get the *x* and *y* coordinates of Hotelling ellipse.
#' @param data Data frame or tibble of PCA, PLS, and ICA scores, or from other feature projection methods.
#' @param pcx Integer specifying which component is on the x-axis (by default 1).
#' @param pcy Integer specifying which component is on the y-axis (by default 2).
#' @param conf.limit Number between 0 and 1 specifying the confidence level (by default 0.95).
#' @param pts Integer indicating the number of points for drawing the Hotelling ellipse (by default 200).
#' @return Data frame containing the *x* and *y* coordinate points of the Hotelling ellipse.
#' @export ellipseCoord
#' @author Christian L. Goueguel,
#' christian.goueguel@gmail.com
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
#' ## Get Hotelling ellipse coordinate points
#' library(HotellingEllipse)
#' xy_coord <- ellipseCoord(data = pca_scores, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200)
#'
ellipseCoord <- function(data, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200) {

  # check input validity
  if (length(data) == 0) {
    stop("Data must not be empty.")
  }
  if (pcx == pcy) {
    stop("pcx and pcy must be different.")
  }
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    stop("Data must be of class data.frame, tbl_df, or tbl.")
  }
  if (pcx == 0 || pcy == 0) {
    stop("pcx and pcy must be non-zero.")
  }
  if (conf.limit < 0 || conf.limit > 1) {
    stop("Confidence level should be between 0 and 1.")
  }

  # matrix of data
  X <- as.matrix(data)

  # Sample size
  n <- nrow(X)

  # Confidence limit
  alpha <- as.numeric(conf.limit)

  # Number of points
  m <- as.numeric(pts)
  p <- seq(0, 2*pi, length.out = m)

  # # Hotelling’s T-square limit
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
