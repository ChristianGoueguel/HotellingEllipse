#' @title Coordinate Points Of Hotelling Ellipse
#' @description Get the *x* and *y* coordinates of Hotelling ellipse.
#' @param data Data frame or tibble of PCA, PLS, and ICA scores, or from other feature projection methods.
#' @param pcx Integer specifying which component is on the x-axis (`pcx = 1` by default).
#' @param pcy Integer specifying which component is on the y-axis (`pcy = 2` by default).
#' @param pcz Optional (`NULL` by default). Integer specifying which component is on the z-axis.
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
ellipseCoord <- function(data, pcx = 1, pcy = 2, pcz = NULL, conf.limit = 0.95, pts = 200) {
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
  X <- as.matrix(data)
  n <- nrow(X)
  alpha <- as.numeric(conf.limit)
  m <- as.numeric(pts)

  if (is.null(pcz)) {
    theta <- seq(0, 2 * pi, length.out = m)
    Tsq_limit <- (2 * (n-1)/(n-2)) * stats::qf(p = alpha, df1 = 2, df2 = (n-2))
    res_coord <- tibble::tibble(
      x = sqrt(Tsq_limit * stats::var(X[, pcx])) * cos(theta) + mean(X[, pcx], na.rm = TRUE),
      y = sqrt(Tsq_limit * stats::var(X[, pcy])) * sin(theta) + mean(X[, pcy], na.rm = TRUE)
    )
  } else {
    # if (pcx == pcy == pcz) {
    #   stop("pcx, pcy and pcz must be different.")
    # }
    # if (pcz == 0) {
    #   stop("pcz must be non-zero.")
    # }
    theta <- seq(0, 2 * pi, length.out = m)
    phi <- seq(0, pi, length.out = m)
    grid <- expand.grid(Theta = theta, Phi = phi)
    Tsq_limit <- (3 * (n-1)/(n-3)) * stats::qf(p = alpha, df1 = 3, df2 = (n-3))
    res_coord <- tibble::tibble(
      x = sqrt(Tsq_limit * stats::var(X[, pcx])) * cos(grid$Theta) * sin(grid$Phi) + mean(X[, pcx], na.rm = TRUE),
      y = sqrt(Tsq_limit * stats::var(X[, pcy])) * sin(grid$Theta) * sin(grid$Phi) + mean(X[, pcy], na.rm = TRUE),
      z = sqrt(Tsq_limit * stats::var(X[, pcz])) * cos(grid$Phi) + mean(X[, pcz], na.rm = TRUE)
    )
  }
  return(res_coord)
}
