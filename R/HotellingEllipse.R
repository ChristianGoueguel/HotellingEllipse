#' Hotelling Ellipse
#'
#' @description
#' Computes axis parameters of Hotelling ellipse for a bivariate scatter plot.
#' The function is created to be used with PCA or PLS scores coordinates, i.e. on the PCs or LVs axes.
#' It shows the statistical limits computed using Hotelling T-squared distribution at 95% and 99% confidence levels.
#'
#' @param data Data frame or tibble of PCA/PLS scores coordinates.
#' @param k Integer, specifying the number of components (k = 2 by default).
#' @param pcx Integer, specifying which component is on the x-axis (pcx = 1 by default).
#' @param pcy Integer, specifying which component is on the y-axis (pcy = 2 by default).
#'
#' @return A list with the T-squared statistic, Hotelling ellipse axis parameters, and T-squared cutoff at 95% and 99% confidence levels.
#' @export
#'
#' @examples HotellingEllipse(data = PCA_scores, k = 2, pcx = 1, pcy = 2)
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
      "conf.limit.99pct" = Tsq_limit1,
      "conf.limit.95pct" = Tsq_limit2
    )

    return(res_list)
  }

}
