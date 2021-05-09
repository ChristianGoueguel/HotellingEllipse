#' Hotelling's T-squared Statistic and Ellipse
#'
#' @description
#' Computes axis parameters of Hotelling ellipse for a bivariate scatter plot.
#' The function is created to be used with principal components analysis (PCA) or partial least squares (PLS) scores coordinates.
#' It shows the statistical limits computed using Hotelling T-squared distribution at 95% and 99% confidence levels.
#'
#' @param data a data frame or tibble of scores coordinates
#' @param k number of components (by default 2)
#' @param pcx an integer specifying which component is on the x-axis (by default 1)
#' @param pcy an integer specifying which component is on the y-axis (by default 2)
#'
#' @return
#' Returns a list including:
#' Tsquared a data frame containing the T-squared statistic
#' Ellipse a data frame containing the major and minor semi-axis values for both Hotelling T-squared ellipses
#' cutoff.99pct an integer indicating the T-squared cutoff value at 99% confidence level
#' cutoff.95pct an integer indicating the T-squared cutoff value at 95% confidence level
#'
#' @author Christian L. Goueguel, christian.goueguel@gmail.com
#'
#' @examples
#' ## Principal components analysis (PCA)
#' library(tidyverse)
#' set.seed(123)
#' pca_mod <- LIBS_spec %>%
#'   select(where(is.numeric)) %>%
#'   FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)
#'
#' ## Extract PCA scores
#' pca_scores <- pca_mod %>%
#'    pluck("ind", "coord") %>%
#'    as_tibble()
#'
#' ## Compute Hotelling T-squared statistic and ellipse parameters
#' library(HotellingEllipse)
#' T2 <- HotellingEllipse(data = pca_scores, k = 2, pcx = 1, pcy = 2)
#'
HotellingEllipse <- function(data, k = 2, pcx = 1, pcy = 2) {

  if (length(data) == 0) {
    stop("Seems you forgot to provide data values.")
  }

  if (is.data.frame(data) == FALSE | is_tibble(data) == FALSE) {
    stop("Input data must be a data.frame, tbl_df, tbl")
  }

  if (pcx == 0 | pcy == 0) {
    stop("Please provide PC axis.")
  }

  if(pcx == pcy) {
    stop("Please provide PC of different axis")
  }

  if (k < 2) {
    stop("k (nomber of PCs) must be at least equal to 2.")
  }

  # Squared Mahalanobis distance
  MDsq <- mahalanobis(
    x = as.matrix(data),
    center = colMeans(as.matrix(data)),
    cov = cov(as.matrix(data)),
    inverted = FALSE
  )

  # Hotelling’s T-squared statistic
  Tsq <- data.frame(statistic = ((n-k)/(k*(n-1)))*MDsq)

  if(k > 2) {
    return(Tsq)
  }

  if(k == 2) {
    # sample size
    n <- nrow(data)

    # 99% and 95% confidence limit for T-squared
    Tsq_limit1 <- (k*(n-1)/(n-k))*qf(p = 0.99, df1 = k, df2 = (n-k))
    Tsq_limit2 <- (k*(n-1)/(n-k))*qf(p = 0.95, df1 = k, df2 = (n-k))

    # Hotelling’s T-squared ellipse axis parameters
    a_limit1 <- as.numeric(sqrt(Tsq_limit1*var(data[pcx])))
    a_limit2 <- as.numeric(sqrt(Tsq_limit2*var(data[pcx])))
    b_limit1 <- as.numeric(sqrt(Tsq_limit1*var(data[pcy])))
    b_limit2 <- as.numeric(sqrt(Tsq_limit2*var(data[pcy])))

    axisvals <- data.frame(
      a1 = a_limit1,
      b1 = b_limit1,
      a2 = a_limit2,
      b2 = b_limit2
    )

    res_list <- list(
      "Tsquared" = Tsq,
      "Ellipse" = axisvals,
      "cutoff.99pct" = Tsq_limit1,
      "cutoff.95pct" = Tsq_limit2
    )

    return(res_list)
  }

}
