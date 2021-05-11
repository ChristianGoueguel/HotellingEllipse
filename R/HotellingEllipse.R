#' Hotelling's T-squared Statistic and Confidence Ellipse
#'
#' @description
#' Computes axis parameters of Hotelling ellipse for a bivariate scatter plot.
#' The function is created to be used with principal components analysis (PCA) or partial least squares (PLS) scores coordinates.
#' It shows the statistical limits computed using Hotelling T-squared distribution at 95% and 99% confidence levels.
#' @title Hotelling's T-squared Ellipse
#' @param data a data frame or tibble of scores coordinates
#' @param k number of components (by default 2)
#' @param pcx an integer specifying which component is on the x-axis (by default 1)
#' @param pcy an integer specifying which component is on the y-axis (by default 2)
#'
#' @return
#' Returns a list including:
#' (1) Tsquared, a data frame containing the T-squared statistic.
#' (2) Ellipse, a data frame containing the major and minor semi-axis values for both Hotelling T-squared ellipses.
#' (3) cutoff.99pct, an integer indicating the T-squared cutoff value at 99% confidence level.
#' (4) cutoff.95pct, an integer indicating the T-squared cutoff value at 95% confidence level.
#'
#' @export HotellingEllipse
#'
#' @author Christian L. Goueguel,
#' christian.goueguel@gmail.com
#'
#' @examples
#' ## Principal components analysis (PCA)
#' library(magrittr)
#' set.seed(123)
#' pca_mod <- LIBS_spec %>%
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
#' T2 <- HotellingEllipse(data = pca_scores, k = 2, pcx = 1, pcy = 2)
HotellingEllipse <- function(data, k = 2, pcx = 1, pcy = 2) {

  if (length(data) == 0) {
    stop("Seems you forgot to provide data values.")
  }

  if (is.data.frame(data) == FALSE | tibble::is_tibble(data) == FALSE) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }

  if (pcx == 0 | pcy == 0) {
    stop("No component is provided either in pcx or pcy, or both.")
  }

  if(pcx == pcy) {
    stop("Please provide two different components in pcx and pcy.")
  }

  if (k < 2) {
    stop("k must be at least equal to 2.")
  }

  if (k > ncol(data)) {
    stop("k exceeds the number of component in the data.")
  }

  if(k >= 2) {
    # matrix of data
    X <- as.matrix(data)

    # sample size
    n <- nrow(X)

    # number of principal component
    A <- as.numeric(k)

    # Squared Mahalanobis distance
    MDsq <- stats::mahalanobis(
      x = X,
      center = colMeans(X),
      cov = stats::cov(X),
      inverted = FALSE
    )

    # Hotelling’s T-squared statistic
    Tsq <- tibble::tibble(statistic = ((n-A)/(A*(n-1)))*MDsq)

    # 99% and 95% confidence limit for Hotelling’s T-squared
    Tsq_limit1 <- (A*(n-1)/(n-A))*stats::qf(p = 0.99, df1 = A, df2 = (n-A))
    Tsq_limit2 <- (A*(n-1)/(n-A))*stats::qf(p = 0.95, df1 = A, df2 = (n-A))

    # Hotelling’s T-squared ellipse semi-axes parameters
    a_limit1 <- sqrt(Tsq_limit1*stats::var(X[, pcx]))
    a_limit2 <- sqrt(Tsq_limit2*stats::var(X[, pcx]))
    b_limit1 <- sqrt(Tsq_limit1*stats::var(X[, pcy]))
    b_limit2 <- sqrt(Tsq_limit2*stats::var(X[, pcy]))

    axis_param <- tibble::tibble(
      a1 = a_limit1,
      b1 = b_limit1,
      a2 = a_limit2,
      b2 = b_limit2
    )

    res_list <- list(
      "Tsquared" = Tsq,
      "Ellipse" = axis_param,
      "cutoff.99pct" = Tsq_limit1,
      "cutoff.95pct" = Tsq_limit2
    )
    return(res_list)
  }

}
