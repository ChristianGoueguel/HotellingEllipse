#' @title Lengths Of The Semi-Axes Of Hotelling Ellipse
#' @description Compute the lengths of the semi-axes of Hotelling ellipse.
#' @param data Data frame or tibble of PCA, PLS, or ICA scores, or from other feature projection methods.
#' @param k Integer specifying the number of components (by default 2).
#' @param pcx Integer specifying which component is on the x-axis (by default 1).
#' @param pcy Integer specifying which component is on the y-axis (by default 2).
#' @return
#' Returns a list that includes:
#'1. **Tsquare** Data frame containing Hotelling T2-value.
#'2. **Ellipse** Data frame containing the lengths of the semi-minor and semi-major axes.
#'3. **cutoff.99pct** Integer indicating the T-square cutoff at 99% confidence level.
#'4. **cutoff.95pct** Integer indicating the T-square cutoff at 95% confidence level.
#' @export ellipseParam
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
#' ## Get Hotelling T2-value and the lengths of the ellipse semi-axes
#' library(HotellingEllipse)
#' T2 <- ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)
#'
ellipseParam <- function(data, k = 2, pcx = 1, pcy = 2) {

  if (length(data) == 0) {
    stop("Seems you forgot to provide data values.")
  }

  if (is.data.frame(data) == FALSE & tibble::is_tibble(data) == FALSE) {
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

  if(k > 2) {
    # 99% and 95% confidence limit
    Tsq_limit1 <- (A*(n-1)/(n-A))*stats::qf(p = 0.99, df1 = A, df2 = (n-A))
    Tsq_limit2 <- (A*(n-1)/(n-A))*stats::qf(p = 0.95, df1 = A, df2 = (n-A))

    # Hotelling’s T-square
    Tsq <- tibble::tibble(value = ((n-A)/(A*(n-1)))*MDsq)

    res_list <- list(
      "Tsquare" = Tsq,
      "cutoff.99pct" = Tsq_limit1,
      "cutoff.95pct" = Tsq_limit2
    )
    return(res_list)
    }

  if(k == 2) {
    # 99% and 95% confidence limit
    Tsq_limit1 <- (A*(n-1)/(n-A))*stats::qf(p = 0.99, df1 = A, df2 = (n-A))
    Tsq_limit2 <- (A*(n-1)/(n-A))*stats::qf(p = 0.95, df1 = A, df2 = (n-A))

    # Hotelling ellipse semi-axes
    a_limit1 <- sqrt(Tsq_limit1*stats::var(X[, pcx]))
    a_limit2 <- sqrt(Tsq_limit2*stats::var(X[, pcx]))
    b_limit1 <- sqrt(Tsq_limit1*stats::var(X[, pcy]))
    b_limit2 <- sqrt(Tsq_limit2*stats::var(X[, pcy]))

    axis_param <- tibble::tibble(
      a.99pct = a_limit1,
      b.99pct = b_limit1,
      a.95pct = a_limit2,
      b.95pct = b_limit2
    )

    # Hotelling’s T-square
    Tsq <- tibble::tibble(value = ((n-2)/(2*(n-1)))*MDsq)

    res_list <- list(
      "Tsquare" = Tsq,
      "Ellipse" = axis_param,
      "cutoff.99pct" = Tsq_limit1,
      "cutoff.95pct" = Tsq_limit2
    )
    return(res_list)
    }
  }

